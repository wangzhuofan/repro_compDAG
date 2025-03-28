This directory contains simulated data files and real data files for the project compDAG.

* 01generate_simulated_data.R: R file to generate simulated datasets according to the proposed compDAG moel. You can adjust $n$ and $signal$ (which correspond to sample size $n$ and signal $\pi_{21}$ in paper) in code to change data generating settings.
* 01generate_sparse_data.R: R file to generate simulated data datasets when $p_1=p_2=100$.
* 01generate_misspecified_data.R: R file to generate simulated datasets according to the linear structural equation moel (misspecified model). You can adjust $n$ and $signal$ (which correspond to sample size $n$ and signal $\pi_{21}$ in paper) in code to change data generating settings.
* 01generate_lingam_comp_data.R: R file to generate anti-log-ratio-transformation data.
* 01generate_confounder_data.R: R file to generate three-community data where the causal relationship is $y_1 \rightarrow y_2, y_1\rightarrow y_3, y_2 \rightarrow y_3.$
* 01generate_latent_comp_data.R: R file to generate data with true zeros.
* 01generate_multi_data.R: R file to generate multi-community data with $y_1\rightarrow y_2 \rightarrow \dots,\rightarrow y_B$.
* 01generate_cyclic_data.R: R file to generate data from a cyclic graph.
* realdata_aml/rawdata/Genus.csv: CSV file which contains the raw data for oral_and_stool real data application. Rows represent taxon while Columns represent samples (including samples from oral or stool).
* realdata_aml/rawdata/metadata.csv: CSV file which contains the meta data for oral_and_stool real data application. It provides information for samples, including patient, timepoint, material (from oral or stool).
* realdata_aml/preprocessed_aml_data.RData: RData file which contains the preprocessed AML data.
* realdata_aml/preprocessing.R: R file to clean and preprocess the raw data.
* realdata_moms_pi/preprocessed_moms_pi_data.RData: RData file which contains the preprocessed MOMS-PI data.
* realdata_moms_pi/preprocessing.R: R file to clean and preprocess the raw MOMS-PI data.

<!-- Please replace the contents of this file with relevant instructions for your repository or remove this file entirely.

This directory would generally contain the real data files (or facsimile versions of them in place of confidential data) and simulated data files.

Cleaned data that are produced by processing raw input data might also be placed here.

Code to generate the simulated data and (if relevant) and facsimile data provided in place of confidential real data might also be placed here. 

Code for data cleaning could potentially also be placed here rather than in `code`. -->
