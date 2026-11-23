"""Build the uxm-settings study tables from marker-region pat files.

Writes to OUT_DIR:
  atlas_ranked.tsv  the U250 atlas with CpG indexes on the hg38 analysis-set genome,
                    plus rank, each marker's 0-based position within its target cell
                    type in Markers.U250.hg38.tsv (ranks 0-24 are the U25 atlas markers)
  homog.tsv.gz      sample, rlen, name, U, X, M: wgbstools homog counts per marker for
                    rlen 3 to 8, run as uxm deconv runs it (default thresholds 0.25,0.75)
  labels.tsv        sample, label: NH_ libraries healthy, PRO_ libraries cancer

Usage: build_tables.py ATLAS_TRANSLATED MARKERS_FILE PAT_DIR OUT_DIR
"""
import csv
import glob
import gzip
import os
import subprocess
import sys
import tempfile

WGBSTOOLS = os.path.expanduser("~/repos/wgbs_tools/wgbstools")
PYTHON = os.path.expanduser("~/miniconda3/envs/emseq/bin/python")
RLENS = range(3, 9)


def main(atlas_path, markers_path, pat_dir, out_dir):
    os.makedirs(out_dir, exist_ok=True)
    rank, seen = {}, {}
    with open(markers_path, newline="") as fh:
        for r in csv.DictReader(fh, delimiter="\t"):
            rank[r["region"]] = seen.get(r["target"], 0)
            seen[r["target"]] = rank[r["region"]] + 1
    with open(atlas_path, newline="") as fh:
        reader = csv.DictReader(fh, delimiter="\t")
        atlas = list(reader)
        fields = list(reader.fieldnames or [])
    with open(os.path.join(out_dir, "atlas_ranked.tsv"), "w", newline="") as fh:
        w = csv.writer(fh, delimiter="\t")
        w.writerow(fields + ["rank"])
        for r in atlas:
            w.writerow([r[f] for f in fields] + [rank[r["name"]]])

    pats = sorted(glob.glob(os.path.join(pat_dir, "*.markers.pat.gz")))
    env = dict(os.environ, PATH=os.path.dirname(PYTHON) + ":" + os.environ["PATH"])
    # A region that is a marker for two cell types appears twice in the atlas; homog
    # refuses duplicated blocks, so each region is counted once (as uxm does).
    unique = list({r["name"]: r for r in reversed(atlas)}.values())[::-1]
    with tempfile.TemporaryDirectory() as tmp:
        blocks = os.path.join(tmp, "markers.bed")
        with open(blocks, "w") as fh:
            for r in unique:
                fh.write("\t".join(r[c] for c in ("chr", "start", "end", "startCpG", "endCpG")) + "\n")
        with gzip.open(os.path.join(out_dir, "homog.tsv.gz"), "wt", newline="") as out, \
                open(os.path.join(out_dir, "labels.tsv"), "w", newline="") as lab:
            w = csv.writer(out, delimiter="\t")
            w.writerow(["sample", "rlen", "name", "U", "X", "M"])
            lab.write("sample\tlabel\n")
            for pat in pats:
                sample = os.path.basename(pat).split(".")[0]
                lab.write(f"{sample}\t{'healthy' if sample.startswith('NH_') else 'cancer'}\n")
                for rlen in RLENS:
                    prefix = os.path.join(tmp, f"{sample}.l{rlen}")
                    subprocess.run([PYTHON, WGBSTOOLS, "homog", "-f", "--genome", "hg38", "--rlen", str(rlen),
                                    "-b", blocks, pat, "--prefix", prefix], check=True, env=env,
                                   stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
                    if not os.path.exists(prefix + ".uxm.bed.gz"):
                        sys.exit(f"{sample} rlen {rlen}: wgbstools homog wrote no output")
                    with gzip.open(prefix + ".uxm.bed.gz", "rt") as fh:
                        rows = [line.rstrip("\n").split("\t") for line in fh if not line.startswith("#")]
                    if len(rows) != len(unique):
                        sys.exit(f"{sample} rlen {rlen}: {len(rows)} homog rows for {len(unique)} markers")
                    for r, a in zip(rows, unique):
                        if (r[0], r[1]) != (a["chr"], a["start"]):
                            sys.exit(f"{sample} rlen {rlen}: homog row {r[:3]} out of marker order at {a['name']}")
                        w.writerow([sample, rlen, a["name"], r[5], r[6], r[7]])
                print(sample, "done", flush=True)


if __name__ == "__main__":
    main(*sys.argv[1:5])
