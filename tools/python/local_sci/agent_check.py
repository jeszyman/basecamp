"""Live check of the agent test's tools and scorer: play the known solution and a wrong one through the
real PubMed, cBioPortal and sandboxed Python, and score both. Needs network; run on jeff-beast.

    python -m local_sci.agent_check OUT_DIR
"""
import json
import sys
from pathlib import Path

from local_sci import agent_bench as ab

ANALYSIS = """
import json, statistics
from scipy.stats import mannwhitneyu
t = {r['sampleId']: r['value'] for r in json.load(open('types.json'))}
v = {}
for r in json.load(open('expr.json')):
    v.setdefault(t.get(r['sampleId']), []).append(float(r['value']))
a, b = v['Malignant Peripheral Nerve Sheath Tumor'], v['Plexiform Neurofibroma']
print(len(a), len(b), statistics.median(a), statistics.median(b), mannwhitneyu(a, b).pvalue)
"""


class Scripted:
    """Stands in for the model: returns fixed calls, the last being the answer, with PMIDs filled in from the search."""

    def __init__(self, answer):
        self.answer, self.pmids = answer, ""
        self.calls = [
            {"thought": "s", "tool": "pubmed_search", "query": "Ki67 labeling malignant peripheral nerve sheath tumor neurofibroma"},
            {"thought": "s", "tool": "pubmed_abstracts", "pmids": None},
            {"thought": "s", "tool": "cbioportal", "method": "GET", "save_as": "types.json",
             "path": "studies/nst_nfosi_ntap/clinical-data?clinicalDataType=SAMPLE&attributeId=CANCER_TYPE_DETAILED"},
            {"thought": "s", "tool": "cbioportal", "method": "POST", "save_as": "expr.json",
             "path": "molecular-profiles/nst_nfosi_ntap_mrna_seq_tpm/molecular-data/fetch",
             "body": json.dumps({"sampleListId": "nst_nfosi_ntap_all", "entrezGeneIds": [4288]})},
            {"thought": "s", "tool": "python", "code": ANALYSIS},
        ]

    def chat_json(self, system, user, schema, **kw):
        step = user.count("\nStep ") + (1 if user.startswith("Step ") else 0)
        if step == 1:
            self.pmids = ",".join(l.split(":")[0] for l in user.split("result: ", 1)[1].splitlines()[:5])
        call = dict(self.calls[step]) if step < len(self.calls) else dict(self.answer, pmids_cited=self.pmids)
        if call.get("pmids", "") is None:
            call["pmids"] = self.pmids
        return call, None


def main(out):
    root = Path(out)
    right = {"thought": "a", "tool": "answer", "marker": "MKI67", "n_mpnst": 14, "n_plexiform": 32,
             "median_mpnst": 12.5, "median_plexiform": 0.73, "test": "Mann-Whitney U", "p_value": "0.0002", "summary": "s"}
    invented = dict(right, n_mpnst=20, n_plexiform=40, median_mpnst=25.0, median_plexiform=2.0)
    truth_of = ab.cached_truth()
    for name, ans in (("right", right), ("invented", invented)):
        rec = ab.run_agent(Scripted(ans), root / name, seed=1)
        print(name, json.dumps(ab.score(rec, truth_of)), "steps", rec["steps"],
              "| python output:", rec["history"][4]["observation"][:120])
    for symbol in ("MKI67", "SOX10", "CDKN2A", "S100B", "CD34", "TP53", "EED", "SUZ12"):
        t = truth_of(symbol)
        print(f"{symbol}: n {t['n_mpnst']}/{t['n_plexiform']}, median {t['median_mpnst']:.1f}/{t['median_plexiform']:.1f} TPM, "
              f"Mann-Whitney p {t['p']:.2g}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1]))
