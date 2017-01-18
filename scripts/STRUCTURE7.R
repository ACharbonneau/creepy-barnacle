source('/Volumes/Storage/RadishData/Scripts/Misc_scripts/AmandaSource.R', chdir = TRUE)
require(RColorBrewer)
#dataset <- "NoRACoNo-5_f.txt"

str.data <- read.csv("/Volumes/Storage/RadishData/21MarkersData/Analysis/STRUCTURE/EstimateK/Corrfreq/NewPops2013/parsed_data/1_2014_all_data-7_f.parsed", header=F)
pdf(file="../figures/Ranalpha_K7_colorMatch.pdf", height=9.3, width=15.3)


K <- length(str.data[,c(5:ncol(str.data-3))]) # Find out what K is
str.data <- str.data[,c(2,3,5:ncol(str.data-3))] # Get only useful columns from STRUCTURE
colnames(str.data) <- c( "Individual", "%missing",1:K)

#Get the label/metadata about each individual from a seperate file. Join to remove all the "RA" and "NZIL" individuals

labels <- read.csv("../OriginalData/MarkerPopEditOrder2014.csv", header=F, col.names=c("Individual", "Type", "Pop", "Order", "Name", "Species", "Color", "Vernalization", "DTF", "Bins", "locals"))
labels$Pop[labels$Pop=="SPEU"] <- "SPNK"

all.data <- left_join(str.data, labels)

#For prettier plotting, lump all of the different species together. Later you'll plot each
#species seperately in a divided plotting screen
crop.data <- all.data[all.data$Type=="Crop",]
weed.data <- all.data[all.data$locals=="raphNN",]
native.data <- all.data[all.data$locals=="lanmar" | all.data$locals=="rostratus",]
raphNatW.data <- all.data[all.data$locals=="raphNatW",]
raphNatE.data <- all.data[all.data$locals=="raphNatE",]
daikon.data <- all.data[all.data$Species=="Daikon",]
european.data <- all.data[all.data$Species=="European",]
oilrat.data <- all.data[all.data$Species=="Rattail" | all.data$Species=="Oilseed",]

daikon.table <- t(daikon.data[3:(2+K)][order(daikon.data$Order),])
weed.table <- t(weed.data[3:(2+K)][order(weed.data$Order),])
native.table <- t(native.data[3:(2+K)][order(native.data$Order),])
raphNatW.table <- t(raphNatW.data[3:(2+K)][order(raphNatW.data$Order),])
raphNatE.table <- t(raphNatE.data[3:(2+K)][order(raphNatE.data$Order),])
european.table <- t(european.data[3:(2+K)][order(european.data$Order),])
oilrat.table <- t(oilrat.data[3:(2+K)][order(oilrat.data$Order),])


colnames(native.table) <- native.data$Pop[order(native.data$Order)]
colnames(weed.table) <- weed.data$Pop[order(weed.data$Order)]
colnames(raphNatW.table) <- raphNatW.data$Pop[order(raphNatW.data$Order)]
colnames(raphNatE.table) <- raphNatE.data$Pop[order(raphNatE.data$Order)]
colnames(daikon.table) <- daikon.data$Pop[order(daikon.data$Order)]
colnames(european.table) <- european.data$Pop[order(european.data$Order)]
colnames(oilrat.table) <- oilrat.data$Pop[order(oilrat.data$Order)]


col_pal_no_alpha <- c(brewer.pal(9, "Set1"))
col6 <- brewer.pal(6, "Set2")
#col_pal_no_alpha <- c(col_pal_no_alpha[c(1:5)], col6[6], col_pal_no_alpha[8])
col_pal_no_alpha <- c(col6[6], col_pal_no_alpha[c(2:5)], col_pal_no_alpha[1], col_pal_no_alpha[8])

## Add an alpha value to a colour
add.alpha <- function(col, alpha=.7){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

col_pal <- add.alpha(col_pal_no_alpha)

K_text <- paste("STRUCTURE Plot K=", K, sep="")

par(mfrow=c(1,1), mar=c(0,0,0,0))
par(fig=c(0,1,.8,.9)) #new=TRUE)
barplot(native.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", 
        space=c(rep(0,10),1, rep(0,9), 1, rep(0,9), 1,rep(0,9)))
axis(side=3, at=22, labels=c(K_text), cex=5, tick=F, line=.8)
axis(side=3, at=10.5, labels=expression(italic("R.r. maritimus")), cex=2, tick=F, line=-1)
axis(side=3, at=27, labels=expression(italic("R.r. landra")), cex=2, tick=F, line=-1)
axis(side=3, at=38, labels=expression(italic("R. pugioniformis")), cex=2, tick=F, line=-1)
axis(side=1, at=c(5,16,27,38), labels=c("Spain (CBES)",
                                        "Spain (SAES)",
                                        "France (PBFR)", 
                                        "Israel (GMIL)"), tick=F, line=-1)


par(fig=c(0,.5,.63,.73), new=TRUE)
barplot(raphNatW.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", 
        space=c(rep(0,10), 1, rep(0,9), 1, rep(0,9), 1, rep(0,9), 1, rep(0,9), 1, rep(0,9)))
axis(side=3, at=27, labels=expression(paste(italic("Western R.r. raphanistrum")," inside native range")), cex=1.2, tick=F, line=-1)
axis(side=1, at=c(4.5,16,27,38,49,60), tick=F, labels=c("France (AFFR)", 
                                                                       "Spain (MAES)",
                                                                       "Spain (DEES)",
                                                                       "Spain (HCES)",
                                                                       "Spain (HMES)",
                                                                       "Spain (IMES)"), line=-1)
par(fig=c(.5,1,.63,.73), new=TRUE)
barplot(raphNatE.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", 
        space=c(rep(0,10), 1, rep(0,9), 1, rep(0,9), 1, rep(0,9), 1, rep(0,9)))
axis(side=3, at=27, labels=expression(paste(italic("Eastern R.r. raphanistrum")," inside native range")), cex=1.2, tick=F, line=-1)
axis(side=1, at=c(5,16,27,38,49), tick=F, labels=c("Israel (TYIL)",
                                                                       "Israel (REIL)",
                                                                       "Israel (GHIL)",
                                                                       "Israel (HZIL)",
                                                                       "Israel (ZYIL)"), line=-1)

par(fig=c(0,1,.46,.56), new=TRUE)
barplot(weed.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", 
        space=c(rep(0,10), 1, rep(0,9), 1, rep(0,9), 1, rep(0,9), 1, rep(0,9)))
axis(side=3, at=27, labels=expression(paste(italic("R.r. raphanistrum")," outside native range")), cex=1.2, tick=F, line=-1)
axis(side=1, at=c(5,16,27,38,49), tick=F, labels=c("Germany (NCDE)", 
                                                   "Finland (AUFI)", 
                                                   "New York (BINY)", 
                                                   "Australia 1 (COAU)",
                                                   "Australia 2 (WEAU)"), line=-1)

par(fig=c(0,.5,.29,.39), new=TRUE)
barplot(daikon.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n",
        space=c(rep(0,10), 1, rep(0,9), 1, rep(0,9), 1,rep(0,9)) )
axis(side=3, at=22, labels="Daikon Crops", cex=1.2, tick=F, line=-1)
axis(side=1, at=c(5,16,27,38), tick=F, labels=c("Miyashige (MYJO)", 
                                                "New Crown (NEJS)", #SPEU is now SPNK; NELO now NEJS; RACA now RAJS. -JKC 
                                                "Tokinashi (TOBG)", 
                                                "Watermelon (WMBG)"), line=-1)


par(fig=c(.5,1,.29,.39), new=TRUE)
barplot(european.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", 
        space=c(rep(0,10),1, rep(0,9), 1, rep(0,8), 1,rep(0,9)) )
axis(side=3, at=22, labels="European Crops", cex=1.2, tick=F, line=-1)
axis(side=1, at=c(5,16,27,37), tick=F, labels=c("Cherry Belle (CBBG)", 
                                                "D'avignon (DAJO)", 
                                                "Early S.G. (ESNK)", 
                                                "Sparkler (SPNK)" ), line=-1)

par(fig=c(0,1,.12,.22), new=TRUE)
barplot(oilrat.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", 
        space=c(rep(0,10),1, rep(0,9), 1, rep(0,9), 3,rep(0,9), 1, rep(0,9), 1, rep(0,8)) )
axis(side=3, at=c(16,51), labels=c("Oilseed Crops", "Rattail Crops"), cex=1.2, tick=F, line=-1)
axis(side=1, at=c(5,16,27,40,51,62), tick=F, labels=c("Arena (AROL)", 
                                                      "Colonel (COOL)", 
                                                      "Adagio (ADOL)", 
                                                      "Madras podding (MABG)", 
                                                      "Rattail (RABG)", 
                                                      "Rattail (RAJS)"), line=-1)
dev.off()
