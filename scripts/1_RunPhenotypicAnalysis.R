# This runs MarkerPaperSetup.Rmd on CultivarGH2013.csv, IsraelSpainPops2013GH.csv,
# LaleField2005.csv, 2012FieldData.csv, 2003QstParents.csv, 2004QstOffspring.csv,
# 2013plantsSpring.csv, and Summer2010dataSummary.csv
# Then runs PhenotypicAnalysis.Rmd


# Install function for packages
packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}
packages(knitr)
packages(rmarkdown)


rmarkdown::render('../scripts/1.1_MarkerPaperSetup.Rmd', output_file = '../Figures/MarkerPaperSetup.html')
rmarkdown::render('../scripts/1.2_PhenotypicAnalysis.Rmd', output_file = '../Figures/PhenotypicAnalysis.html')
rmarkdown::render('../scripts/1.3_SummaryStatistics.Rmd', output_file= '../Figures/SummaryStatistics.html')
