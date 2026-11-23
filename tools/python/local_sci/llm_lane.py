"""The 9B on beast: the model-server lease and the experimenter's three requests.

ServerLease takes beast's GPU lock (shared with the local-review jobs) with a
bounded wait, starts the local-review model server under its own compose project
with a loopback port, and stops it on exit. The requests go through
local-review's LLMClient, whose replies are constrained to a JSON schema. Nothing
the model returns is executed; the caller validates every setting.
"""
import fcntl
import json
import os
import subprocess
import sys
import time
from pathlib import Path

LOCAL_REVIEW = Path(os.path.expanduser("~/repos/local-review"))
sys.path.insert(0, str(LOCAL_REVIEW))
from local_review.llm import LLMClient  # noqa: E402

LOCK_FILE = Path("/mnt/data/llm/gpu.lock")
CHAT_MARK = Path("/mnt/data/llm/chat.active")
PORT = 8085
PROJECT = "local-sci"

SYSTEM = (
    "You run experiments for a scientist. The question and the measure are fixed; you choose which "
    "settings of the measure to try next, from the results so far, so as to find the setting where the "
    "measure is best. Reply only with the JSON asked for."
)


class LeaseError(Exception):
    """The GPU stayed busy past the wait limit, or the chat server is running."""


class ServerLease:
    def __init__(self, model="qwen3.5-9b", ctx=32768, wait_seconds=7200, status=lambda msg: None):
        self.model, self.ctx, self.wait_seconds, self.status = model, ctx, wait_seconds, status
        self.override = Path(os.path.expanduser("~/.cache/local-sci/compose-override.yaml"))

    def _env(self):
        args = (LOCAL_REVIEW / "config" / "serve" / f"{self.model}.args").read_text().split()
        if "-c" in args:
            args[args.index("-c") + 1] = str(self.ctx)
        env = dict(os.environ, LLM_ARGS=" ".join(args + ["-np", "1", "--no-webui"]))
        return env

    def _compose(self, *args):
        return ["docker", "compose", "-p", PROJECT, "-f", str(LOCAL_REVIEW / "compose.yaml"),
                "-f", str(self.override), *args]

    def __enter__(self):
        if CHAT_MARK.exists():
            raise LeaseError("the local-review chat server is running; stop it first")
        self.override.parent.mkdir(parents=True, exist_ok=True)
        self.override.write_text(f'services:\n  llm:\n    ports:\n      - "127.0.0.1:{PORT}:8080"\n')
        self.lock = open(LOCK_FILE, "w")
        deadline = time.time() + self.wait_seconds
        while True:
            try:
                fcntl.flock(self.lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
                break
            except BlockingIOError:
                if time.time() > deadline:
                    self.lock.close()
                    raise LeaseError(f"the GPU was busy for {self.wait_seconds // 60} minutes")
                self.status("waiting for the GPU")
                time.sleep(30)
        self.status("starting the model server")
        subprocess.run(self._compose("up", "-d", "--wait", "llm"), env=self._env(), check=True,
                       stdout=subprocess.DEVNULL)
        self.client = LLMClient(f"http://127.0.0.1:{PORT}", timeout=1800)
        self.client.wait_ready(900)
        return self.client

    def __exit__(self, *exc):
        subprocess.run(self._compose("down"), env=self._env(), stdout=subprocess.DEVNULL,
                       stderr=subprocess.DEVNULL)
        fcntl.flock(self.lock, fcntl.LOCK_UN)
        self.lock.close()
        return False


def _space_text(space, constraints, better):
    params = "; ".join(f"{p} from {lo} to {hi} in steps of {st}" for p, (lo, hi, st) in space.items())
    rules = "; ".join(constraints) if constraints else "none"
    return f"Parameters: {params}. Constraints: {rules}. A {better} value of the measure is better."


def _history_text(history):
    if not history:
        return "No settings tried yet."
    lines = ["round | setting | median | 95% interval | your reason"]
    for h in history:
        r = h["row"]
        interval = "n/a" if r["ci_low"] is None else f"{r['ci_low']:.3f} to {r['ci_high']:.3f}"
        median = "n/a" if r["median"] is None else f"{r['median']:.3f}"
        lines.append(f"{h['round']} | {h['setting']['name']} | {median} | {interval} | {h['reason']}")
    return "\n".join(lines)


def _setting_schema(space, max_items):
    props = {p: {"type": "integer", "minimum": lo, "maximum": hi} for p, (lo, hi, _) in space.items()}
    props["reason"] = {"type": "string", "maxLength": 300}
    return {
        "type": "object",
        "properties": {"settings": {"type": "array", "maxItems": max_items, "items": {
            "type": "object", "properties": props, "required": list(space) + ["reason"]}}},
        "required": ["settings"],
    }


def propose(client, question, measure_md, space, constraints, better, history, k, budget_left, seed):
    """Ask the 9B for up to k new settings with a reason each. Returns (settings, raw reply)."""
    k = max(0, min(k, budget_left))
    user = (
        f"Question:\n{question}\n\nMeasure:\n{measure_md}\n\nSetting space: {_space_text(space, constraints, better)}\n\n"
        f"Results so far (median over samples, with its 95% interval):\n{_history_text(history)}\n\n"
        f"Settings left in the budget: {budget_left}. Propose {k} new settings that have not been tried, "
        f"with a one-sentence reason for each based on the results so far."
    )
    reply, _ = client.chat_json(SYSTEM, user, _setting_schema(space, k), temperature=0.7, seed=seed, max_tokens=2000)
    return reply.get("settings", [])[:k], reply


def pick(client, question, measure_md, space, constraints, better, history, seed, n=3):
    """Ask the 9B for up to n final picks among the settings tried. Returns (picks, raw reply)."""
    schema = {"type": "object", "properties": {"picks": {"type": "array", "maxItems": n, "items": {
        "type": "object", "properties": {"name": {"type": "string"}, "reason": {"type": "string", "maxLength": 300}},
        "required": ["name", "reason"]}}}, "required": ["picks"]}
    user = (
        f"Question:\n{question}\n\nMeasure:\n{measure_md}\n\n{_space_text(space, constraints, better)}\n\n"
        f"All results:\n{_history_text(history)}\n\n"
        f"Choose up to {n} of the settings tried above, by their name in the setting column, to test on new "
        f"samples: the ones most likely to be best. Give a one-sentence reason for each."
    )
    reply, _ = client.chat_json(SYSTEM, user, schema, temperature=0.2, seed=seed, max_tokens=1000)
    return reply.get("picks", [])[:n], reply


def narrate(client, question, history, seed):
    """Ask the 9B for a short account of what it tried and why. Returns (text, raw reply)."""
    schema = {"type": "object", "properties": {"text": {"type": "string", "maxLength": 1500}}, "required": ["text"]}
    user = (
        f"Question:\n{question}\n\nWhat you tried, round by round, with your reasons:\n{_history_text(history)}\n\n"
        "In under 150 words, say what you tried in which order and why. Describe the search only; "
        "do not state whether any setting is better, significant, or confirmed."
    )
    reply, _ = client.chat_json(SYSTEM, user, schema, temperature=0.2, seed=seed, max_tokens=600)
    return reply.get("text", ""), reply


def as_json(obj):
    return json.dumps(obj, default=str)
