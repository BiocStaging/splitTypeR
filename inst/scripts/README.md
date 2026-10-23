# Description of the files in inst/extdata

This document describes how the files present in the `inst/extdata`
directory of the **splitTypeR** package were generated. Both files are used
in the package vignette.


## Tiriac_NormExpCounts.RDS

A `RangedSummarizedExperiment` object containing the normalized expression
of 35,818 genes (rows) for 39 pancreatic ductal adenocarcinoma (PDAC)
patient-derived organoids (PDOs) (columns).

### Source

The RNA-seq data of the PDAC PDOs published in:

> Tiriac H, Belleau P, Engle DD, et al. Organoid Profiling Identifies Common
> Responders to Chemotherapy in Pancreatic Cancer. Cancer Discov.
> 2018;8(9):1112-1129. doi:10.1158/2159-8290.CD-18-0349

The raw sequencing data are available from [TO COMPLETE: repository and
accession number, e.g. dbGaP or GEO].

### Processing

1. The RNA-seq reads were aligned on the GRCh38.p14 human genome with the
   GENCODE v48 annotation using STAR v2.7.9a.
2. The expected counts were obtained with [TO COMPLETE: quantification tool
   and version, e.g. RSEM].
3. Only one PDO per patient was retained.
4. The expected counts were processed with DESeq2 v1.50.2 on the genes
   expressed in at least 20% of the PDOs. [TO COMPLETE: definition of
   "expressed" (e.g. minimum count) and the normalization method used.]

### Content

* `assays(se)[["normalizedCounts"]]`: the normalized expected counts.
* `rowRanges(se)`: the genomic location of each gene, with the `gene_id`
  (Ensembl identifier), `gene_type` and `gene_name` columns.
* `colData(se)`: the `Organoid_Line`, `Birth.Sex`, `Tissue.Source` and
  `Tissue.Type` of each PDO.


## Moffitt_Tiriac_2018_Assignation.txt

A tab-delimited text file with two columns:

* `Line`: the name of the PDO (matching the column names of
  `Tiriac_NormExpCounts.RDS`).
* `Moffitt`: the subtype (`Basal-like` or `Classical`) assigned to the PDO.

The file contains the subtype of 44 PDOs. Among them, 37 PDOs are present
in `Tiriac_NormExpCounts.RDS` and are used for the comparison in the
vignette.

### Source

The subtypes, as defined by Moffitt et al. (2015), assigned to the PDOs in
Tiriac et al. (2018). [TO COMPLETE: location of the original assignment in
the publication, e.g. supplementary table number.]
