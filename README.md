# creepy-barnacle
Script repository Weed evolution: Genetic differentiation among wild, weedy, and crop radish

This paper was built on the MSU High Performance Computing Cluster. Building it elsewhere will require installing several programs, and may require some editing of submission scripts. At a minimum, the machine running it will need:

- git 2.10.1
- R 3.2.0 (packages will auto-install)
- pandoc 1.17.3
- python 2.7.2
- Eigensoft 6.0.1
- STRUCTURE 2.3.4

To build a copy of paper data:

Clone this repo:

> git clone https://github.com/ACharbonneau/creepy-barnacle.git

Run 0_PaperSetup.sh, from inside the new repo:

> cd creepy-barnacle

> bash scripts/0_PaperSetup.sh

That will download the raw dataset, build a coherent dataframe from it, and run all of the flowering time analysis, as well as produce the files needed to run STRUCTURE and SmartPCA. Files used downstream go into the MungedData folder, figures and html output will be in Figures.

Then manually run both STRUCTURE and SmartPCA, using the scripts provided. These are independent, and can be run in parallel:

> cd STRUCTURE

> bash ../scripts/2_STRUCTURE_submit.sh

> cd ../smartPCA

> qsub ../scripts/2_SmartPCA.qsub -N Marker

Once these have finished, manually begin the plotting prep and functions:

From inside STRUCTURE folder:

> bash ../scripts/3_AllTheStructureParsing.sh

From the smartPCA folder:

> cd ../smartPCA

> module load R/3.2.0

> Rscript SmartPCAPlot.R



Assuming all the parts ran correctly, the Figures folder should contain all figures from the paper, plus several unused supplemental ones, and html files explaining the analysis.

## Script Descriptions

#### General Scripts

- 0_PaperSetup.sh

	Creates workspace, and launches 1_RunPhenotypicAnalysis

- 0_2015_Marker_Paper.Rproj

	R Project file

#### Phenotypic Analysis and Data Prep

- 1_RunPhenotypicAnalysis

	Launches 1.1_MarkerPaperSetup.Rmd and 1.2_PhenotypicAnalysis.Rmd

- 1.1_MarkerPaperSetup.Rmd  

	Preps data for analysis, makes files for running STRUCTURE and SmartPCA

- 1.1.2_biallele.py

	Python script for putting microsatellite data into a biallelic format for SmartCPCA, runs inside MarkerPaperSetup.Rmd

- 1.2_PhenotypicAnalysis.Rmd  

	Completes flowering time analysis

#### Genotypic Analysis

##### STRUCTURE

- 2_STRUCTURE_submit.sh

	Shell script for simultaneously submitting multiple runs of STRUCTURE_submit.qsub

- 2.1_STRUCTURE_submit.qsub

	Generic qsub file for starting STRUCTURE on cluster

- 2.1.1_mainparams and 2.1.1_extraparams

	Required parameter files for running STRUCTURE

- 3_AllTheStructureParsing.sh

	Shell script that takes output from STRUCTURE and puts it into a format readable by R scripts

- 3.1_structureparse.py

	Does the actual format rewriting, called by 2.2_AllTheStructureParsing.sh

- 3.2_STRUCTURE2015.R  

	R script for generating all STRUCTURE plots

- 3.2_STRUCTURE7.R

	R script for generating STRUCTURE plot for main paper figure, where coloring matches the SmartPCA plot

##### SmartPCA

- 2_SmartPCA.qsub

	Submits SmartPCA job to cluster

- 3_SmartPCAPlot.R  

	R script for generating SmartPCA plots
