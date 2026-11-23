## Quantity

For one cancer sample and one window [lo, hi] bp: how many healthy standard deviations the sample's share of fragments inside the window sits above the healthy mean, minus the same number for the 100-150 bp window. A value above 0 means the window places the sample further above the healthy samples than 100-150 bp does.

## How it is computed

The share inside a window is the sum of the sample's length frequencies from lo to hi bp (per_bam_fragment_frequencies.tsv, bp_50 to bp_450, each row summing to 1). The z-score uses the mean and sample standard deviation of the healthy samples' shares for the same window. The cancer or healthy label comes from the selected column of sample_selection_metadata.tsv; samples marked excluded are left out.

## No-change value

0, at 100-150 bp, the short window of Cristiano et al. 2019 (DELFI).

## What this design cannot tell

The share is genome-wide, not the short-to-long ratio in 5 Mb bins that DELFI uses. The z-score uses the healthy samples of one cohort, about 8 in each half of the split, so the healthy standard deviation is itself noisy. A window that gains separation through lower healthy variance and one that gains it through higher cancer shares count the same.
