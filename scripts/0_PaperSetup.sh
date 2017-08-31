#! /bin/bash -login
# Build Weed evolution: Genetic differentiation among wild, weedy, and crop radish

mkdir Figures
mkdir OriginalData
mkdir MungedData
mkdir STRUCTURE
mkdir smartPCA

wget https://web.stanford.edu/group/rosenberglab/software/CLUMPP_Linux64.1.1.2.tar.gz
gunzip CLUMPP*tar.gz; tar xvf CLUMPP*.tar
rm CLUMPP*.tar

cd OriginalData || exit
wget -L https://www.dropbox.com/s/29snqitigf2pss3/WeedEvo.zip
unzip WeedEvo.zip

cd ../scripts || exit
module load R/3.2.0 || exit
module load pandoc/1.17.3 || exit
Rscript 1_RunPhenotypicAnalysis.R
