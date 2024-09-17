**Title**: Using ISAJET for High-Energy Physics Calculations

**Overview**: This project involved utilizing the **ISAJET** software package to calculate specific particle physics parameters in the context of high-energy physics experiments.

**Technologies** Used: ISAJET, Fortran, Bash, Python (for data post-processing).
Requirements: the Code can be downloaded from https://www.nhn.ou.edu/~isajet/
            You need Fortran compiler.

**Description**: ISAJET is a Monte Carlo event generator used for simulating high-energy particle collisions. In this project, I used ISAJET to perform simulations and calculate various physical values, such as cross-sections and event kinematics, within a specific theoretical framework. The software was configured to model proton-proton collisions at the Large Hadron Collider (LHC), providing detailed predictions of particle behavior under different experimental conditions.

**Key Features**:

Monte Carlo simulations for proton-proton collisions.
Calculation of cross-sections and event rates for different physics processes.
Generation of parton-level and particle-level events, allowing for detailed analysis of outcomes.
Results: The use of ISAJET allowed me to obtain accurate predictions for particle interaction parameters, contributing to ongoing research in experimental high-energy physics. The values calculated were used to compare theoretical models against real-world experimental data from CERN.

Conclusion: By leveraging ISAJET's advanced event simulation capabilities, this project provided key insights into particle collision behavior, aiding in the interpretation of experimental results.

**SLHA Files**
In high-energy physics, particularly in the context of supersymmetric (SUSY) models and beyond the Standard Model (BSM) physics, an SLHA file refers to a file format defined by the SUSY Les Houches Accord (SLHA). It is designed to provide a standardized way of exchanging particle physics data, such as mass spectra, couplings, decay widths, and other parameters used in the simulation of new physics models.

The script **run_nuhm_input.sh** simply calculates the SLHA files for various model.
Modify the run_SLHA_files.sh


You can set the model by "**which_model**" and the output name for the SLHA files by "**name_output**" parameters in shell script.

1. 1 for mSUGRA:
2. 2 for mGMSB:
3. 3 for non-universal SUGRA:
4. 4 for SUGRA with truly unified gauge couplings:
5. 5 for non-minimal GMSB:
6. 6 for SUGRA+right-handed neutrino:
7. 7 for minimal anomaly-mediated SUSY breaking:
8. 8 for non-minimal AMSB:
9. 9 for mixed moduli-AMSB:
10. 10 for Hypercharged-AMSB:
11. 11 for NUHM from D-term:
12. 12 for general mirage mediation (GMM):
13. 13 for natural AMSB (nAMSB): 

This project involves scanning the parameter space of low-mass supersymmetry models. The first step is configuring and compiling the ISAJET software to set up the environment. Afterward, a shell script is executed to run the calculations and generate the desired results.

