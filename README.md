# Modeling Microbial Community Coalescence via Compositional Directed Acyclic Graphical Models

# Author Contributions Checklist Form

## Data

### Abstract

Our real data analysis consists of two datasets.

- Female Genital Tract 16S RNA Data：microbial 16S RNA data in women's cervix of uterus and vagina from the Human Microbiome Project 2,
  which can be accessed via the R package *HMP2Data*, as a part of the multi-omic microbiome study, pregnancy initiative (MOMS-PI).
- Oral and Stool Microbiome Data：16S rRNA sequences of longitudinal fecal and oral
  samples from 97 patients with acute myeloid leukemia (AML) receiving induction chemotherapy.

### Availability

Yes, the data to replicate our results is available (we have included it with the submission).

### Description

The two datasets are provided in the directory /*data* and are respectively included in the sub-directory */data/realdata_moms_pi* and */data/realdata_aml*. Please see the README.md in directory /*data* for detail.

## Code

### Abstract

The code provided includes the main code to conduct the compDAG method.

### Description

* *coalescence.stan* and *nocausal.stan* are stan files to conduct posterior sampling.
* *compDAG.R* is the R file to conduct the compDAG method.

## Instructions for Use

### Reproducibility

All the figures and tables in the article can be reproduced (i.e.Figures 1-2,Tables 1-6 and Table S.1).

We provided the results of intermediate output in the directory */output* to avoid having to rerun computationally-intensive steps of the workflow. You can reproduce these intermediate results by following the instructions in *README.md* in the directory */output*.

How to reproduce analyses (e.g., workflow information, makefile, wrapper scripts)

- Table 1 run: “table1_code.R”
- Table 2 run: “table2_code.R”
- Table 3 run: “table3_code.R”
- Table 4 run: “table4_code.R”
- Table 5 run: “table5_code.R”
- Table 6 run: “table6_code.R”
- Table S.1 run: “tableS1_code.R”
- Figure 1 run: “figure1_code.R”
- Figure 2 run: “figure2_code.R”
