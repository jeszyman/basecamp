"""The experiment loop: a proposer chooses settings, the statistics code scores them.

run_study works on a study folder (question.md, measure.md, measure.py with
CASE_LABEL, NO_CHANGE_VALUE, NO_CHANGE, SPACE, CONSTRAINTS, BETTER, load, measure,
and a data folder path in data.txt) and a run folder. The proposer only names
settings; the loop checks each against the space and the settings already run,
scores it on the discovery half of the samples, and records it. After the
rounds, up to 3 picks are tested on the held-out half, where alone flags are set.
"""
import csv
import json
import random
import shutil
from pathlib import Path

from local_sci import planner
from local_sci.harness import (
    check_no_change,
    confirm,
    evaluate,
    load_measure,
    negative_control,
    split_samples,
)
from local_sci.report import plot, write_study_report
from local_sci.space import in_space, scripted_proposer, setting_name


def _clean(setting, space):
    point = {p: setting.get(p) for p in space}
    point["name"] = setting_name(point, space) if all(isinstance(v, int) for v in point.values()) else "invalid"
    return point


class ScriptedSearch:
    """The comparison experimenter: compass search, picks the 3 best discovery results."""

    def __init__(self, space, constraints, better, seed):
        self.space, self.constraints, self.better = space, constraints, better
        self.rng, self.state = random.Random(seed), {}

    def propose(self, history, k, budget_left):
        scored = [(h["setting"], h["row"]["median"]) for h in history]
        out = scripted_proposer(self.space, self.constraints, scored, min(k, budget_left), self.rng, self.state, self.better)
        return [dict(s, reason="compass search") for s in out], {"scripted": True}

    def pick(self, history):
        sign = 1 if self.better == "higher" else -1
        ranked = sorted((h for h in history if h["row"]["median"] is not None), key=lambda h: -sign * h["row"]["median"])
        return [{"name": h["setting"]["name"], "reason": "top discovery median"} for h in ranked[:3]], {"scripted": True}

    def narrate(self, history):
        return "A compass search: random settings first, then steps along one parameter at a time from the best result, halving the step when no step improved.", {}


def run_study(study_dir, run_dir, experimenter, rounds=4, per_round=5, split_seed=1, status=lambda m: None):
    study, run = Path(study_dir), Path(run_dir)
    run.mkdir(parents=True, exist_ok=True)
    data_dir = run / "data"
    data_dir.mkdir(exist_ok=True)
    for path in Path((study / "data.txt").read_text().strip()).iterdir():
        if path.is_file():
            shutil.copy2(path, data_dir / path.name)
    shutil.copy2(study / "measure.py", run / "measure.py")
    planner.check_code(run / "measure.py")
    mod = load_measure(str(run / "measure.py"))
    question = (study / "question.md").read_text()
    measure_md = (study / "measure.md").read_text()
    space, constraints = mod.SPACE, list(getattr(mod, "CONSTRAINTS", []))
    data, labels = mod.load(str(data_dir))
    check_no_change(mod, data, labels, mod.NO_CHANGE)
    discovery, holdout = split_samples(labels, split_seed)

    history, log, budget, empty = [], open(run / "proposals.jsonl", "w"), rounds * per_round, 0
    tried = {setting_name({p: mod.NO_CHANGE[p] for p in space}, space)}
    for rnd in range(1, rounds + 1):
        if budget <= 0:
            break
        status(f"round {rnd} of {rounds}")
        valid = []
        for attempt in range(2):
            want = min(per_round, budget) - len(valid)
            if want <= 0:
                break
            proposed, raw = experimenter.propose(history, want, budget)
            rejected = []
            for s in proposed:
                point = _clean(s, space)
                if point["name"] in tried or not in_space({p: point[p] for p in space}, space, constraints):
                    rejected.append({"setting": s, "why": "already run" if point["name"] in tried else "outside the space"})
                    continue
                tried.add(point["name"])
                valid.append((point, s.get("reason", "")))
            log.write(json.dumps({"round": rnd, "attempt": attempt + 1, "raw": raw, "rejected": rejected}, default=str) + "\n")
            if len(valid) >= min(per_round, budget):
                break
        budget -= min(per_round, budget)
        if not valid:
            empty += 1
            if empty == 2:
                break
            continue
        empty = 0
        for point, reason in valid:
            row = evaluate(mod, data, discovery, point)
            history.append({"round": rnd, "setting": point, "reason": reason, "row": row})

    picks_raw, raw = experimenter.pick(history)
    by_name = {h["setting"]["name"]: h["setting"] for h in history}
    picks = [by_name[p["name"]] for p in picks_raw if p.get("name") in by_name][:3]
    log.write(json.dumps({"picks": raw}, default=str) + "\n")
    if not picks and history:
        picks = [history[0]["setting"]]
    no_change_row = evaluate(mod, data, holdout, mod.NO_CHANGE, is_no_change=True)
    conf_rows = confirm(mod, data, holdout, picks)
    control = negative_control([no_change_row] + conf_rows, float(mod.NO_CHANGE_VALUE))
    narrative, raw = experimenter.narrate(history)
    log.write(json.dumps({"narrative": raw}, default=str) + "\n")
    log.close()

    with open(run / "experiments.tsv", "w", newline="") as fh:
        w = csv.writer(fh, delimiter="\t")
        w.writerow(["round", "setting", "reason", "n", "median", "ci_low", "ci_high"])
        for h in history:
            r = h["row"]
            w.writerow([h["round"], h["setting"]["name"], h["reason"], r["n"], r["median"], r["ci_low"], r["ci_high"]])
    plot([no_change_row] + conf_rows, float(mod.NO_CHANGE_VALUE), str(run / "figure.png"))
    report = write_study_report(str(run), question, measure_md, history, conf_rows, control, narrative,
                                "figure.png", sum(v == mod.CASE_LABEL for v in holdout.values()))
    return {"report": report, "history": history, "confirm": conf_rows, "control": control}
