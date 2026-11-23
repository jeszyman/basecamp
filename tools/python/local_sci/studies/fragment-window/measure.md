## Quantity

For one cancer sample and one window [lo, hi] bp, the net signal-to-noise gain over using all fragments: e(w) × √r(w). Here r(w) is the sample's share of fragments inside the window (the reads kept), and e(w) is the factor by which the window raises tumor fraction. A value above 1 means the window gains more from enrichment than it loses from discarded reads.

## How it is computed

Each sample is treated as its ichorCNA tumor fraction (tf, measured on all fragments) of tumor fragments plus fragments with the mean healthy length distribution. If H(w) is the mean healthy share inside the window, the tumor fraction inside the window is 1 − (1 − tf) · H(w) / r(w), and e(w) is that value divided by tf. Lengths come from per_bam_fragment_frequencies.tsv (bp_50 to bp_450, each row summing to 1); tf and the cancer or healthy label come from sample_selection_metadata.tsv.

## No-change value

1.0, at the full range 50-450 bp, where r(w) = 1 and e(w) = 1.

## What this design cannot tell

The gain is modeled from length profiles and tumor fractions, not measured by rerunning ichorCNA on size-selected reads. The √r(w) noise term ignores GC and mappability effects and the tumor-fraction floor at which calls fail. Dividing by tf makes the value unstable for samples with very low tumor fraction.
