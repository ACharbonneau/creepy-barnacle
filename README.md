# creepy-barnacle
Script repository Weed evolution: Genetic differentiation among wild, weedy, and crop radish

To build a copy of paper:

Clone this repo in an empty directory:

> git clone https://github.com/ACharbonneau/creepy-barnacle.git

Run PaperSetup.sh, from inside the new repo:

> cd creepy-barnacle
> sh PaperSetup.sh


##Scripts

####General Scripts

- 2015_Marker_Paper.Rproj
	
	R Project file
	
- Functionarium.R
	
	Generic custom functions

####Phenotypic Analysis

- MarkerPaperSetup.Rmd  
- PhenotypicAnalysis.Rmd  

To run:

> rmarkdown::render('MarkerPaperSetup.Rmd', output_file = '../output/MarkerPaperSetup.html') 
> rmarkdown::render('PhenotypicAnalysis.Rmd', output_file = '../output/PhenotypicAnalysis.html') 


####Genotypic Analysis

- STRUCTURE_submit.qsub 
	
	Generic qsub file for starting STRUCTURE on HPC

- STRUCTURE_submit.sh
	
	Shell script for simultaneously submitting multiple runs of STRUCTURE_submit.qsub
	
- mainparams and extraparams
	
	Required parameter files for running STRUCTURE
 
- biallele.py 

	Python script for putting microsatellite data into a biallelic format for SmartCPCA
	
- SNP_SSR_NoNZIL_allPC.R  
	
	R script for generating SmartPCA plots
	
- AllTheStructureParsing.sh

	Shell script that takes output from STRUCTURE and puts it into a format readable by R scripts
	
- structureparse.py

	Does the actual format rewriting, called by AllTheStructureParsing.sh 
	
- STRUCTURE2015.R  

	R script for generating all STRUCTURE plots
	
- STRUCTURE7.R

	R script for generating STRUCTURE plot for main paper figure, where coloring matches the SmartPCA plot
	
#####Running STRUCTURE

mainparams 
extraparams 
STRUCTURE_submit.qsub 
STRUCTURE_submit.sh 

OUTPUT NAMES MUST START WITH THE RANDOMIZATION NUMBER AND HAVE A "-{K}" OR IT WILL BREAK THE R SCRIPT!!

To run:

- Randomize input files by row. Name as in "1rand.txt"
- Edit the STRUCTURE_submit.sh file as necessary with correct number of randomized files "`seq 1 20`", and the correct filenames and number or K to run for each randomization "-t 1-20".

- Run the submit file from the folder that has the randomized input files, the parameter files, and STRUCTURE_submit.qsub

- When those runs are completed, run AllTheStructureParsing.sh

- In R, run STRUCTURE2015.R with the working directory set to parsed_data

- To recapitulate the STRUCTURE plot from the main paper figure, run STRUCTURE7.R

#####Running SmartPCA

- biallele.py
- MarkerPaperSetup.Rmd


SmartPCA can only take biallelic data. SSR data is converted to fake bialleles using biallele.py. This is done automatically as part of MarkerPaperSetup.Rmd

To run:

> smartpca.perl -i Marker.geno -a Marker.snp -b Marker.ind -o Marker.pca -p Marker.plot -e Marker.eval -l Marker.log

