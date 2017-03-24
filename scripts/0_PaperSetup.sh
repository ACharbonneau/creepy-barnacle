#! /bin/bash -login
# Build Weed evolution: Genetic differentiation among wild, weedy, and crop radish

mkdir Figures
mkdir OriginalData
mkdir MungedData
mkdir STRUCTURE
mkdir smartPCA


cd OriginalData || exit
wget -L https://www.dropbox.com/s/xgs16dh5c7f0cex/WeedEvo.zip
unzip WeedEvo.zip

cd ../scripts || exit
module load R/3.2.0 || exit
module load pandoc/1.17.3 || exit
Rscript 1_RunPhenotypicAnalysis.R
