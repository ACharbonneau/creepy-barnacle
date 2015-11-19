# creepy-barnacle
Script repository for 2015 paper about raphanus population structure.

##Scripts
####General Scripts

2015_Marker_Paper.Rproj  
Functionarium.R  

####Phenotypic Scripts
MarkerPaperSetup.Rmd  
PhenotypicAnalysis.Rmd  

####Genotypic Scrips
extraparams  
mainparams  
nacho.qsub  
biallele.py  
SNP_SSR_NoNZIL_allPC.R  
STRUCTURE2015.R  
STRUCTURE_submit.sh  

###Phenotypic Analysis
MarkerPaperSetup.Rmd  
PhenotypicAnalysis.Rmd  

To run:
> rmarkdown::render('MarkerPaperSetup.Rmd', output_file = '../output/MarkerPaperSetup.html') 
> rmarkdown::render('PhenotypicAnalysis.Rmd', output_file = '../output/PhenotypicAnalysis.html') 


###Genotypic Analysis
####Running STRUCTURE
mainparams 
extraparams 
nacho.qsub 
STRUCTURE_submit.sh 

OUTPUT NAMES MUST START WITH THE RANDOMIZATION NUMBER AND HAVE A "-{K}" OR IT WILL BREAK THE R SCRIPT!!

To run:

- randomize input files by row. Name as in "1rand.txt"
- Edit the STRUCTURE_submit.sh file as necessary with correct number of randomized files "`seq 1 20`", and the correct filenames and number or K to run for each randomization "-t 1-20".

- Run the submit file from the folder that has the randomized input files, the parameter files, and nacho.qsub
- Move everything onto local computer, then run AllTHeStructureParsing.sh

- Open AlltheProbabilities.txt in Textwrangler:
Find:

> (\d+)_STRUCTURE-(\d+)_f$\n^Estimated Ln Prob of Data   = (.+)$\n^Mean value of ln likelihood = (.+)$\n^Variance of ln likelihood   = (.+)\n

Replace:

>            \1,\2,\3,\4,\5\n

- In R, run STRUCTURE2015.R with the working directory set to parsed_data

- In R, PlotKprobabilities with AlltheProbabilities.txt

####Running SmartPCA
biallele.py
MarkerPaperSetup.Rmd


SmartPCA can only take biallelic data. SSR data is converted to fake bialleles using biallele.py. This is done automatically as part of MarkerPaperSetup.Rmd

To run:

> smartpca.perl -i Marker.geno -a Marker.snp -b Marker.ind -o Marker.pca -p Marker.plot -e Marker.eval -l Marker.log

