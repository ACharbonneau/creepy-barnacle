# Build Weed evolution: Genetic differentiation among wild, weedy, and crop radish 

mkdir Figures
mkdir OriginalData
mkdir MungedData

cd OriginalData
wget -L https://www.dropbox.com/s/y5lqrwf1q4zmxvo/WeedEvo.zip
unzip WeedEvo.zip

cd ../scripts
module load R/3.2.0
module load pandoc/1.17.3
R --file=1_RunPhenotypicAnalysis.R
