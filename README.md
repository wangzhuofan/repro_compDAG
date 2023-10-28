

# Modeling Microbial Community Coalescence via Compositional Directed Acyclic Graphical Models

# Author Contributions Checklist Form

## Data

### Abstract 
Our data source consists of measurements of the background gamma-ray spectrum that were
collected from June to December of 2012 at the University of Texas J.J. Pickle Research
Campus (hereafter, PRC). The measurements were taken with a cesium-iodide scintillator
detector hooked up to a GPS unit and driven around campus in a golf cart. This enabled us to
characterize the background radiation over a wide spatial area.

### Availability 
Yes, the data to replicate our results is available (we have included it with the submission).

### Description 

The data set contains five files.

- The global background spectrum for the University of Texas Pickle Research Campus (PRC)
(“background_train_mean.csv”).
- The measured spectra of cesium and cobalt from the our laboratory experiments (“ 2013 -
cs137-5cm.csv” and “co60-4min-320cps-c7-20cm.csv” respectively).
- The raw data for the anomaly-detection experiment on real data: training and testing data
(“training_data.csv”, “testing_data.csv”).

#### Permissions 
The data was originally collected by the authors.

#### Licensing information
Creative Commons CC0 (see full license at https://wiki.creativecommons.org/wiki/Data).

#### Link to data
github.com/

#### Data provenance, including identifier or link to original data if different than above

Collected from June-December 2012 by the authors at the University of Texas Pickle Research
Campus.

#### File format
csv

#### Metadata 
The data is in different matrix files, where for each matrix each column represents a photon
measurement channel.

#### Version information
1.

## Code

### Abstract 
The code provided includes all the scripts necessary to replicate all the experiments in the
paper, for both real and simulated data.

### Description 
How delivered (R package, Shiny app, etc.)
The code to reproduce the experiments consists of R scripts.

### Licensing information

The code is licensed under the terms of the MIT license

## Instructions for Use

### Reproducibility 
What is to be reproduced (e.g., "All tables and figure from paper", "Tables 1-4”, etc.)
"All tables and figure from paper", Tables 1- 3 , Figures 1- 8.

How to reproduce analyses (e.g., workflow information, makefile, wrapper scripts)

- Table 1 run: “Paper_Table1.R”
- Table 2 in the paper can be obtained by first running
“Paper_generate_radition_ground_truth.R”, here one needs to specify the parameters of the
anomaly. Then run: “Paper_simulated_radition.R” to obtain comparisons between different
methods.
- Table 3 can be obtained using the script: “Paper_small_anomaly.R”.
- Figure 1- 2 can be reproduce by generating the desired pre and post change densities using
“generate_radiation_ground_truth.R”, and then (for Figure 1 ) using code from “Paper_Var_L.R
and “Paper_simulated_radition.R”(for Figure 2).
- Figure 3: requires the data available in the “Paper_Simulated_example” folder and can be
plotted with “Paper_Table1.R”.
- Figure 4-6: can be obtained by generating the desired pre and post change densities using
“generate_radiation_ground_truth.R”.
- Figure 7 can be reproduced running “Paper_Var_L.R”.
- Figure 8: requires the testing data and can be plotted after running the script
“Paper_small_real_anomaly.R”.
