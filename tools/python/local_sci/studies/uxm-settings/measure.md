## Quantity

For one patient and one setting: how many healthy standard deviations the patient's epithelial fraction sits above the healthy mean, minus the same number under the published UXM default. A value above 0 means the setting places the patient further above the healthy donors than the default does.

The epithelial fraction is the sum of the deconvolution fractions of the 14 cell types whose atlas name contains "-Ep": Bladder, Breast-Basal, Breast-Luminal, Colon, Fallopian, Gastric, Head-Neck, Kidney, Lung-Alveolar, Lung-Bronchial, Ovary, Prostate, Small-Intestine and Thyroid epithelium. Hepatocytes, keratinocytes and pancreatic duct cells are not in the sum.

## How it is computed

The reference is the hg38 U250 atlas shipped with UXM (UXM_deconv repository, supplemental/Atlas.U250.l4.hg38.full.tsv), restricted to the 36 cell types of its published U25 atlas. A marker is a region that is unmethylated in one cell type and methylated in the others. The setting "markers" keeps the top N markers of each cell type in the order of the published marker list, whose first 25 are the U25 atlas. For each sample and marker, the value is the share of reads covering at least "rlen" CpGs of the marker whose methylated fraction is at most 0.25 (wgbs_tools homog, as uxm deconv runs it). The mixture is fitted by non-negative least squares with each marker weighted by its read count and is scaled to sum to 1, as in uxm deconv. "breast" set to 0 removes the Breast-Basal-Ep and Breast-Luminal-Ep cell types and their markers before the fit. The z-score uses the mean and sample standard deviation of the healthy donors' epithelial fractions under the same setting.

Reads come from the filtered alignments of the 55 libraries, cut to the marker regions with 1 kb on each side, and converted to read patterns with wgbs_tools on an hg38 analysis-set CpG index. Patients are the PRO_ libraries and healthy donors the NH_ libraries.

## No-change value

0, at 25 markers per cell type, at least 4 CpGs per read, and breast types included: the U25 l4 atlas, the published default of UXM.

## What this design cannot tell

The atlas reference values were computed from reads of at least 4 CpGs, so a setting with another rlen compares sample values and reference values made at different read lengths. The held-out half has about 20 patients and 7 to 8 healthy donors, so the healthy standard deviation is itself noisy. A setting that gains separation through lower variance among healthy donors and one that gains it through higher patient fractions count the same. Without a tumor fraction for each patient, a higher epithelial fraction cannot be checked against how much tumor DNA a sample carries.
