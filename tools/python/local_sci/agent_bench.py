"""Known-answer agent test: can a local model search the literature, fetch data and test it?

Design: ~/repos/basecamp/docs/specs/2026-11-24-agent-known-answer-study.md.

The model gets one question and five tools (PubMed search, PubMed abstracts, the
cBioPortal API, Python in a sandbox folder, and answer). At each step it returns one
JSON object naming a tool and its arguments; the loop runs the tool and sends back the
result. Python runs under bubblewrap: it can write only the run's sandbox folder,
/mnt/gcs and /mnt/data are hidden, and it has no network. Runs are scored by code
against the known answer.

    python -m local_sci.agent_bench OUT_DIR --model qwen3.5-9b [--reasoning-budget N] [--runs 5]
"""
import argparse
import json
import os
import re
import subprocess
import sys
import time
import urllib.parse
import urllib.request
import xml.etree.ElementTree as ET
from pathlib import Path

QUESTION = ("Which established marker distinguishes malignant peripheral nerve sheath tumor (MPNST) from plexiform "
            "neurofibroma, and does its RNA expression differ between the two in the open nerve sheath tumor study "
            "nst_nfosi_ntap on cBioPortal? Find the marker in the literature, fetch the data, test it, and answer.")

TOOLS_TEXT = """Tools (choose one per step):
- pubmed_search: fields query. Searches PubMed; returns up to 10 PMIDs with titles.
- pubmed_abstracts: fields pmids (comma-separated, up to 5). Returns titles and abstracts.
- cbioportal: fields method (GET or POST), path (after https://www.cbioportal.org/api/, e.g. studies/STUDY/molecular-profiles), body (JSON text for POST, else empty), save_as (a file name). Saves the full JSON reply as save_as in your working folder and shows the record count and the first 1,500 characters. Useful paths: studies/STUDY/molecular-profiles; studies/STUDY/clinical-data?clinicalDataType=SAMPLE&attributeId=ATTR; molecular-profiles/PROFILE/molecular-data/fetch (POST, body {"sampleListId": "STUDY_all", "entrezGeneIds": [ID]}); genes/SYMBOL.
- python: field code. Runs Python 3 with pandas and scipy in your working folder (files saved by cbioportal are there); no network; 120 s limit; shows the first 1,500 characters of output. Print what you need to see.
- answer: fields marker (gene symbol), n_mpnst, n_plexiform (samples in each group you tested), median_mpnst, median_plexiform (median expression in each group), test, p_value, pmids_cited (comma-separated PMIDs whose abstracts you read), summary. Ends the task."""

SYSTEM = ("You are a careful research assistant working alone with tools. Reply only with the JSON asked for: "
          "one tool call per reply.")

SCHEMA = {"type": "object", "required": ["thought", "tool"], "properties": {
    "thought": {"type": "string", "maxLength": 400},
    "tool": {"enum": ["pubmed_search", "pubmed_abstracts", "cbioportal", "python", "answer"]},
    "query": {"type": "string", "maxLength": 300}, "pmids": {"type": "string", "maxLength": 100},
    "method": {"enum": ["GET", "POST"]}, "path": {"type": "string", "maxLength": 400},
    "body": {"type": "string", "maxLength": 2000}, "save_as": {"type": "string", "maxLength": 80},
    "code": {"type": "string", "maxLength": 4000},
    "marker": {"type": "string", "maxLength": 40},
    "n_mpnst": {"type": "integer"}, "n_plexiform": {"type": "integer"},
    "median_mpnst": {"type": "number"}, "median_plexiform": {"type": "number"},
    "test": {"type": "string", "maxLength": 100}, "p_value": {"type": "string", "maxLength": 40},
    "pmids_cited": {"type": "string", "maxLength": 200}, "summary": {"type": "string", "maxLength": 1000}}}

MAX_STEPS, KEEP_FULL, PREVIEW, OLD_PREVIEW = 30, 8, 1500, 200
EUTILS = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
CBIO = "https://www.cbioportal.org/api/"
UA = {"User-Agent": "local-sci-agent"}
PYTHON = os.path.expanduser("~/miniconda3/envs/basecamp/bin/python")

# Protein or antibody names a run may give for a gene, mapped to the gene symbol.
ALIASES = {"KI-67": "MKI67", "KI67": "MKI67", "MIB-1": "MKI67", "MIB1": "MKI67", "P16": "CDKN2A",
           "P16INK4A": "CDKN2A", "INK4A": "CDKN2A", "SOX-10": "SOX10", "S100": "S100B", "S-100": "S100B",
           "P53": "TP53", "H3K27ME3": "EED"}
KNOWN_POSITIVE = {"MKI67": "higher", "SOX10": "lower", "CDKN2A": "lower"}  # library-backed, see the design
NERVE_SHEATH = re.compile(r"MPNST|MALIGNANT PERIPHERAL NERVE SHEATH|NEUROFIBROMA")
STUDY, PROFILE = "nst_nfosi_ntap", "nst_nfosi_ntap_mrna_seq_tpm"
GROUPS = {"mpnst": "Malignant Peripheral Nerve Sheath Tumor", "plexiform": "Plexiform Neurofibroma"}


def symbol_of(marker):
    m = re.sub(r"\s+", "", (marker or "").upper()).split("(")[0]
    return ALIASES.get(m, m)


def truth(symbol):
    """The harness's own computation for one gene: Entrez ID, n and median TPM per group, two-sided Mann-Whitney p."""
    import statistics
    from scipy.stats import mannwhitneyu
    gene = json.loads(_get(CBIO + "genes/" + urllib.parse.quote(symbol)))
    entrez = gene["entrezGeneId"]
    types = {r["sampleId"]: r["value"] for r in json.loads(_get(
        CBIO + f"studies/{STUDY}/clinical-data?clinicalDataType=SAMPLE&attributeId=CANCER_TYPE_DETAILED"))}
    expr = json.loads(_get(CBIO + f"molecular-profiles/{PROFILE}/molecular-data/fetch",
                           data=json.dumps({"sampleListId": f"{STUDY}_all", "entrezGeneIds": [entrez]}).encode(),
                           headers={"Content-Type": "application/json"}))
    vals = {k: [float(r["value"]) for r in expr if types.get(r["sampleId"]) == g] for k, g in GROUPS.items()}
    return {"symbol": symbol, "entrez": entrez, "n_mpnst": len(vals["mpnst"]), "n_plexiform": len(vals["plexiform"]),
            "median_mpnst": statistics.median(vals["mpnst"]), "median_plexiform": statistics.median(vals["plexiform"]),
            "p": mannwhitneyu(vals["mpnst"], vals["plexiform"], alternative="two-sided").pvalue}


def _get(url, data=None, headers=None, timeout=60):
    req = urllib.request.Request(url, data=data, headers={**UA, **(headers or {})})
    with urllib.request.urlopen(req, timeout=timeout) as r:
        return r.read().decode()


def pubmed_search(query):
    ids = json.loads(_get(EUTILS + "esearch.fcgi?" + urllib.parse.urlencode(
        {"db": "pubmed", "term": query, "retmax": 10, "retmode": "json"})))["esearchresult"]["idlist"]
    if not ids:
        return "No results."
    summ = json.loads(_get(EUTILS + "esummary.fcgi?" + urllib.parse.urlencode(
        {"db": "pubmed", "id": ",".join(ids), "retmode": "json"})))["result"]
    return "\n".join(f"{i}: {summ.get(i, {}).get('title', '')}" for i in ids)


def pubmed_abstracts(pmids, seen):
    ids = [p for p in re.findall(r"\d+", pmids)][:5]
    if not ids:
        return "No PMIDs given."
    root = ET.fromstring(_get(EUTILS + "efetch.fcgi?" + urllib.parse.urlencode(
        {"db": "pubmed", "id": ",".join(ids), "retmode": "xml"})))
    out = []
    for art in root.iter("PubmedArticle"):
        pmid = art.findtext(".//MedlineCitation/PMID") or ""
        title = "".join(art.find(".//ArticleTitle").itertext()) if art.find(".//ArticleTitle") is not None else ""
        abstract = " ".join("".join(a.itertext()) for a in art.iter("AbstractText"))
        seen[pmid] = title + " " + abstract
        out.append(f"PMID {pmid}: {title}\n{abstract}")
    return "\n\n".join(out) or "No records."


def cbioportal(method, path, body, save_as, sandbox, calls):
    path = path.strip().lstrip("/")
    if path.startswith("http") or ".." in path:
        return "Error: give the path after https://www.cbioportal.org/api/ only."
    name = re.sub(r"[^\w.-]", "_", save_as or "reply.json")[:80] or "reply.json"
    data = None
    if method == "POST":
        try:
            data = json.dumps(json.loads(body or "{}")).encode()
        except json.JSONDecodeError as err:
            return f"Error: body is not valid JSON ({err})."
    target = (sandbox / name)
    if target.is_symlink() or target.resolve().parent != sandbox.resolve():
        return "Error: save_as must be a plain file name in the working folder."
    try:
        text = _get(CBIO + path, data=data, headers={"Content-Type": "application/json"} if data else None)
    except Exception as err:  # the model sees the HTTP error and can correct its request
        return f"Error: {err}"
    calls.append({"method": method, "path": path, "body": body or ""})  # recorded only when the request succeeded
    fd = os.open(target, os.O_WRONLY | os.O_CREAT | os.O_TRUNC | os.O_NOFOLLOW, 0o644)
    with os.fdopen(fd, "w") as fh:
        fh.write(text)
    try:
        obj = json.loads(text)
        count = f"{len(obj)} records" if isinstance(obj, list) else "1 object"
    except json.JSONDecodeError:
        count = "not JSON"
    return f"Saved {name} ({count}). First {PREVIEW} characters:\n{text[:PREVIEW]}"


def run_python(code, sandbox, step):
    script = sandbox / f"_step{step:02d}.py"
    script.write_text(code)
    # Only the system directories and the Python environment are visible (read-only), plus the
    # sandbox folder (writable); the home folder, the repositories and /mnt are not mounted.
    env_dir = os.path.dirname(os.path.dirname(PYTHON))
    cmd = ["bwrap"]
    for d in ("/usr", "/lib", "/lib64", "/bin", "/etc"):
        if os.path.exists(d):
            cmd += ["--ro-bind", d, d]
    cmd += ["--ro-bind", env_dir, env_dir, "--dev", "/dev", "--proc", "/proc", "--tmpfs", "/tmp",
            "--bind", str(sandbox), str(sandbox), "--unshare-net", "--unshare-pid", "--die-with-parent",
            "--chdir", str(sandbox), PYTHON, script.name]
    try:
        p = subprocess.run(cmd, capture_output=True, text=True, timeout=120)
        out = (p.stdout + ("\n" + p.stderr if p.stderr else "")).strip()
    except subprocess.TimeoutExpired:
        out = "Error: stopped after 120 s."
    return out[:PREVIEW] or "(no output)"


def prompt(history):
    lines = [f"Question: {QUESTION}", "", TOOLS_TEXT, "", f"Steps used: {len(history)} of {MAX_STEPS}.", ""]
    for i, h in enumerate(history):
        obs = h["observation"] if i >= len(history) - KEEP_FULL else h["observation"][:OLD_PREVIEW] + " [cut]"
        args = {k: v for k, v in h["call"].items() if k not in ("thought", "tool") and v}
        lines.append(f"Step {i + 1}. thought: {h['call'].get('thought', '')}\ntool: {h['call'].get('tool')} "
                     f"{json.dumps(args)[:600]}\nresult: {obs}\n")
    lines.append("Give the next step as JSON: thought, tool, and that tool's fields.")
    return "\n".join(lines)


def run_agent(client, run_dir, seed):
    run_dir = Path(run_dir)
    sandbox = run_dir / "work"
    sandbox.mkdir(parents=True, exist_ok=True)
    history, seen, calls, answer, t0 = [], {}, [], None, time.time()
    for step in range(1, MAX_STEPS + 1):
        try:
            call, _ = client.chat_json(SYSTEM, prompt(history), SCHEMA, temperature=0.5, seed=seed + step,
                                       max_tokens=3000 + getattr(client, "reply_extra", 0))
        except Exception as err:  # a reply that fails the schema or times out is logged and ends the run
            history.append({"call": {"tool": "error"}, "observation": f"model error: {err}"})
            break
        tool = call.get("tool")
        try:
            if tool == "answer":
                answer = call
                history.append({"call": call, "observation": "(answer)"})
                break
            if tool == "pubmed_search":
                obs = pubmed_search(call.get("query", ""))
            elif tool == "pubmed_abstracts":
                obs = pubmed_abstracts(call.get("pmids", ""), seen)
            elif tool == "cbioportal":
                obs = cbioportal(call.get("method", "GET"), call.get("path", ""), call.get("body", ""),
                                 call.get("save_as", ""), sandbox, calls)
            elif tool == "python":
                obs = run_python(call.get("code", ""), sandbox, step)
            else:
                obs = f"Error: unknown tool {tool}."
        except Exception as err:  # a failed tool call is shown to the model, which can retry
            obs = f"Error: {err}"
        history.append({"call": call, "observation": obs})
    record = {"seed": seed, "seconds": round(time.time() - t0), "steps": len(history), "answer": answer,
              "abstracts_seen": seen, "cbioportal_calls": calls, "history": history}
    (run_dir / "run.json").write_text(json.dumps(record, indent=1))
    return record


def cached_truth():
    """truth() with a cache; a gene cBioPortal does not know gives None."""
    cache = {}

    def truth_of(symbol):
        if symbol not in cache:
            try:
                cache[symbol] = truth(symbol)
            except Exception:  # unknown symbol or failed request: the run cannot be scored on that marker
                cache[symbol] = None
        return cache[symbol]
    truth_of.cache = cache
    return truth_of


def _close(reported, true):
    return reported is not None and abs(float(reported) - true) <= max(0.1 * abs(true), 0.1)


def score(record, truth_of):
    """The design's three criteria, by code. truth_of(symbol) gives the harness's own computation for a gene
    (see truth), or None when the gene is unknown.

    literature: a cited PMID whose abstract the run read names the marker and MPNST or neurofibroma.
    data: a successful cbioportal request asked the RNA profile for the marker's Entrez ID.
    analysis: the reported n per group equal the harness's (MPNST against plexiform neurofibroma only) and the
    reported medians are within 10% (or 0.1 TPM) of the harness's, so the comparison was computed on the
    fetched data with the right groups. Direction and p are reported, not scored."""
    a = record.get("answer") or {}
    symbol = symbol_of(a.get("marker"))
    out = {"marker": symbol or None, "literature": False, "data": False, "analysis": False,
           "direction": None, "known_positive": symbol in KNOWN_POSITIVE, "p_reported": a.get("p_value")}
    t = truth_of(symbol) if symbol else None
    if t is None:
        out["passed"] = False
        return out
    names = [symbol] + [k for k, v in ALIASES.items() if v == symbol]
    cited = re.findall(r"\d+", a.get("pmids_cited") or "")
    out["literature"] = any(p in record["abstracts_seen"] and NERVE_SHEATH.search(record["abstracts_seen"][p].upper())
                            and any(n in record["abstracts_seen"][p].upper() for n in names) for p in cited)
    out["data"] = any(PROFILE in c["path"] + c["body"] and re.search(rf"\b{t['entrez']}\b", c["path"] + c["body"])
                      for c in record["cbioportal_calls"])
    out["analysis"] = (a.get("n_mpnst") == t["n_mpnst"] and a.get("n_plexiform") == t["n_plexiform"]
                       and _close(a.get("median_mpnst"), t["median_mpnst"])
                       and _close(a.get("median_plexiform"), t["median_plexiform"]))
    if a.get("median_mpnst") is not None and a.get("median_plexiform") is not None:
        out["direction"] = "higher in MPNST" if a["median_mpnst"] > a["median_plexiform"] else "lower in MPNST"
    out["passed"] = out["literature"] and out["data"] and out["analysis"]
    return out


def main(argv=None):
    ap = argparse.ArgumentParser()
    ap.add_argument("out")
    ap.add_argument("--model", default="qwen3.5-9b")
    ap.add_argument("--reasoning-budget", type=int, default=None)
    ap.add_argument("--runs", type=int, default=5)
    args = ap.parse_args(argv)
    root = Path(args.out)
    from local_sci.llm_lane import ServerLease
    truth_of = cached_truth()
    rows = []
    with ServerLease(model=args.model, reasoning_budget=args.reasoning_budget,
                     status=lambda m: print("status:", m, flush=True)) as client:
        for i in range(1, args.runs + 1):
            rec = run_agent(client, root / f"{args.model}-run{i}", seed=i * 1000)
            s = score(rec, truth_of)
            rows.append((i, rec, s))
            print(args.model, "run", i, json.dumps(s), "steps", rec["steps"], "seconds", rec["seconds"], flush=True)
    lines = [f"# Known-answer agent test: {args.model}", "",
             "| Run | Steps | Minutes | Marker | Known positive | Literature | Data | Analysis | Direction | p reported | Passed |",
             "|---|---|---|---|---|---|---|---|---|---|---|"]
    for i, rec, s in rows:
        lines.append(f"| {i} | {rec['steps']} | {rec['seconds'] / 60:.1f} | {s['marker']} | {s['known_positive']} | "
                     f"{s['literature']} | {s['data']} | {s['analysis']} | {s['direction']} | {s['p_reported']} | "
                     f"{s['passed']} |")
    lines += ["", "Harness computation for each marker named: " + "; ".join(
        f"{t['symbol']} n {t['n_mpnst']}/{t['n_plexiform']}, median {t['median_mpnst']:.1f}/{t['median_plexiform']:.1f} TPM, "
        f"p {t['p']:.2g}" for t in truth_of.cache.values() if t)]
    lines += ["", f"Passed: {sum(s['passed'] for _, _, s in rows)} of {len(rows)}.", ""]
    (root / f"{args.model}.md").write_text("\n".join(lines))
    print("\n".join(lines))
    return 0


if __name__ == "__main__":
    sys.exit(main())
