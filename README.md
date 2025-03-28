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

* 02-5-(0-4)compDAG.stan: stan file to conduct posterior sampling for the parameters of $y_b$ with (0-4) parents.
* 02-2nocausal.stan: stan file to conduct posterior sampling if there is no causal relationship between $y_1$ and $y_2$.
* 02-1coalescence-hyper.stan: stanfile to conduct posterior sampling for different choices of hyperparameters.
* 02compDAG.R :R file to conduct compDAG method for two communities.
* 02compDAG_param.R: R file to conduct compDAG method and save the chain.
* 02-3multiCompDAG.R: R file to conduct compDAG method and estimate the community-level causal relationships for three communities.
* 02-5compDAG.R: R file to conduct compDAG method and estimate the community-level causal relationships for five communities.
* 02-3microbe.R: R file to conduct compDAG method and estimate the microbe-level causal relationships for three communities.
* 02-5microbe.R: R file to conduct compDAG method and estimate the microbe-level causal relationships for five communities.
* 03bQCD.R: R file to conduct bQCD method.
* 00load_all.R : R file to load all the required packages and functions.
* 03bQCD.R: R file to conduct bQCD method.
* main.R: R file to conduct simulation studies in the paper.
* PARAMS.R R file to read parameter settings from bash file.

### Reproducibility

All the figures and tables in the article and supplementary material can be reproduced (i.e.Figures 1-2, Tables 1-5, Figures S.1-S.14 and Table S.1-S.11).

We provided the results of intermediate output in the directory */output* to avoid having to rerun computationally-intensive steps of the workflow. You can reproduce these intermediate results by following the instructions in *README.md* in the directory */output*.

How to reproduce analyses (e.g., workflow information, makefile, wrapper scripts)

- Table 1-4 run: table1_code.R
- Table 5 run: table5_code.R
- Table S.1 run: tableS1_code.R
- Table S.2 run: tableS2_code.R
- Table S.3 run: tableS3_code.R
- Table S.4 run: tableS4_code.R
- Table S.5 run: tableS5_code.R
- Table S.6 run: tableS6_code.R
- Table S.7 run: tableS7_code.R
- Table S.8 run: tableS8_code.R
- Table S.9 run: tableS9_code.R
- Table S.10-S.11 run: tableS10_code.R
- Figure 1 run: figure1_code.R
- Figure 2 run: figure2_code.R
- Figure S.1 run: figureS1_code.R
- Figure S.2-S.9 run: figureS2_code.R
- Figure S.10 run: figureS10_code.R
- Figure S.11 run: figureS11_code.R
- Figure S.12 run: figureS12_code.R
- Figure S.13 run: figureS13_code.R
- Figure S.14 run: figureS14_code.R
