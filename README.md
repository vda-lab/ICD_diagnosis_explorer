# ICD Diagnosis explorer
This repository provides a proof-of-principle web interface to explore intensive care patients (from the MIMIC-III dataset) using STAD-R.

For an online demo, see [here](https://dalcaide.shinyapps.io/diagnosis_explorer/)

### Computation of STAD-R network from ICD codes
[MIMIC-III](https://mimic.physionet.org/) is an openly available dataset. We provide in example code to compute both the distance matrices (file `1_compute_diagnosis_distance.R`) and STAD-R networks (`2_compute_STAD_R_network.R`) from diagnosis lists (See `compute_STAD_R_network` folder).