source('/Volumes/Storage/RadishData/Scripts/Misc_scripts/AmandaSource.R', chdir = TRUE)
require(ggplot2)
require(dplyr)
require(RColorBrewer)
#dataset <- "NoRACoNo-5_f.txt"

########################################################



pdf(file="../figures/RanalphaNewPops.pdf", height=9.3, width=15.3)

ghost_count <- c(0,0,0) #c("K", "Run", "Ghosts")
pop_ghost_count <- c(0,0,0)

########################################################

#### You have to add a +1 to File_Num and change the second 'for loop' to 2:length(ALLTHEFILES) for datasets without a K=1
#### 

##Get all the files from this directory to put into a single PDF
#Sort the filelist into numerical order by K instead of order from filesystem

ALLTHEFILES <- dir("../OriginalData/parsed_data/")

File_Num <- length(ALLTHEFILES) #+1

metadata <- rep(NA, 2) #Runs, Ks

#This ridiculous thing just makes is so I can get the highest run number without a loop, and the next one gets the highest K
metadata[1] <- max(as.numeric(as.matrix(as.data.frame(regmatches(ALLTHEFILES, regexec("^([0-9]+)", ALLTHEFILES))))))

metadata[2] <- max(as.numeric(as.matrix(as.data.frame(regmatches(ALLTHEFILES, regexec("-([0-9]+)", ALLTHEFILES))))))


File_list <- matrix(
  c(
    rep(1:metadata[2], metadata[1]),
    rep(1:metadata[1], each=metadata[2]),
    rep(NA, File_Num)
  ), 
  nrow=File_Num, ncol=3)
colnames(File_list) <- c("K", "randomization", "runname")


for(n in 1:File_Num){
  nameoffile <- ALLTHEFILES[n]
  randomization <- regexec("^([0-9]+)", nameoffile)
  random_new <- regmatches(nameoffile, randomization)
  R <- random_new[[1]][2]
  
  testedK <- regexec("-([0-9]+)", nameoffile)
  K_new <- regmatches(nameoffile, testedK)
  K <- K_new[[1]][2]
  
  File_list[File_list[,1]==as.numeric(K) & File_list[,2]==as.numeric(R),3] <- nameoffile
}

#Sort each STRUCTURE file and metadata file by the plant ID number then combine them. This gets rid
#of the random order of indivduals needed to run STRUCTURE

for(run_num in c(1:metadata[1])){
  
  for(i in c(3:metadata[2])){  # <- 1:length for K=1, 2:length for no
    
    dataset <- File_list[File_list[,1]==i & File_list[,2]==run_num,3]
    str.data <- 0
    str.data <- read.csv(paste("../OriginalData/parsed_data/",dataset, sep=""), header=F)
    K <- length(str.data[,c(5:ncol(str.data-3))]) # Find out what K is
    str.data <- str.data[,c(2,3,5:ncol(str.data-3))] # Get only useful columns from STRUCTURE
    colnames(str.data) <- c( "Individual", "%missing",1:K)
    
    #Get the label/metadata about each individual from a seperate file. Join to remove all the "RA" and "NZIL" individuals
    
    labels <- read.csv("../OriginalData/MarkerPopEditOrder2014.csv", header=F, col.names=c("Individual", "Type", "Pop", "Order", "Name", "Species", "Color", "Vernalization", "DTF", "Bins", "locals"))
    labels$Pop[labels$Pop=="SPEU"] <- "SPNK"
    
    all.data <- left_join(str.data, labels)
    

    if (i > 1) {ghosts <- apply(all.data[,3:(2+K)], 2, max)
                newghost <- c(i, run_num, sum(ghosts < .5))
                ghost_count <- rbind(ghost_count, newghost)}
    
    if (i > 1) { popghosts <- aggregate(all.data, by=list(all.data$Pop), FUN=mean)[4:(3+K)]
                 popghosts <- apply(popghosts, 2, max)
                 newpopghost <- c(i, run_num, sum(popghosts < .5))
                 pop_ghost_count <- rbind(pop_ghost_count, newpopghost)}
    
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
    
    
    col_pal1 = brewer.pal(12, "Set3")
    col_pal2 = brewer.pal(8, "Dark2")
    col_pal3 = brewer.pal(12, "Paired")
    col_pal = c(col_pal1, col_pal2, col_pal3)
    
    K_text <- paste("STRUCTURE Plot K=", K, sep="")
    #SPEU is now SPNK; NELO now NEJS; RACA now RAJS. -JKC 
    par(mfrow=c(1,1), mar=c(0,0,0,0))
    par(fig=c(0,1,.8,.9)) #new=TRUE)
    barplot(native.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", 
            space=c(rep(0,10),1, rep(0,9), 1, rep(0,9), 1,rep(0,9)))
    axis(side=3, at=22, labels=c(K_text), cex=5, tick=F, line=.8)
    axis(side=3, at=16, labels=expression(italic("R.r. landra")), cex=2, tick=F, line=-1)
    axis(side=3, at=38, labels=expression(italic("R. pugioniformis")), cex=2, tick=F, line=-1)
    axis(side=1, at=c(5,16,27,38), labels=c("Spain (CBES)",
                                            "Spain (SAES)",
                                            "France (PBFR)", 
                                            "Israel (GMIL)"), tick=F, line=-1)
    
    par(fig=c(0,1,.46,.56), new=TRUE)
    barplot(weed.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", 
            space=c(rep(0,10), 1, rep(0,9), 1, rep(0,9), 1, rep(0,9), 1, rep(0,9)))
    axis(side=3, at=27, labels=expression(paste(italic("R.r. raphanistrum")," outside native range")), cex=1.2, tick=F, line=-1)
    axis(side=1, at=c(5,16,27,38,49), tick=F, labels=c("Germany (NCDE)", 
                                                       "Finland (AUFI)", 
                                                       "New York (BINY)", 
                                                       "Australia 1 (COAU)",
                                                       "Australia 2 (WEAU)"), line=-1)
    
    
    par(fig=c(0,.5,.63,.73), new=TRUE)
    barplot(raphNatW.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", 
            space=c(rep(0,10), 1, rep(0,9), 1, rep(0,9), 1, rep(0,9), 1, rep(0,9), 1, rep(0,9)))
    axis(side=3, at=33, labels=expression(paste(italic("Western R.r. raphanistrum")," inside native range")), cex=1.2, tick=F, line=-1)
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
    
    
    par(fig=c(0,.5,.29,.39), new=TRUE)
    barplot(daikon.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n",
            space=c(rep(0,10), 1, rep(0,9), 1, rep(0,9), 1,rep(0,9)) )
    axis(side=3, at=22, labels="Daikon Crops", cex=1.2, tick=F, line=-1)
    axis(side=1, at=c(5,16,27,38), tick=F, labels=c("Miyashige (MYJO)", 
                                                    "New Crown (NEJS)", 
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
    
  }    
}

dev.off()

ghost_count <- data.frame(ghost_count)
colnames(ghost_count) <- c("K", "Run", "Ghosts")
ghost_count <- ghost_count[2:nrow(ghost_count), ]

pdf(file="../figures/GhostInd.pdf", height=9.3, width=15.3)

arrange( ghost_count, K) %>% ggplot( aes(K, Ghosts)) + geom_point() + geom_jitter(height = 0)

dev.off()

write.csv(ghost_count, "../output/ghosts.csv")

pop_ghost_count <- data.frame(pop_ghost_count)

colnames(pop_ghost_count) <- c("K", "Run", "Ghosts")
pop_ghost_count <- pop_ghost_count[2:nrow(pop_ghost_count), ]

pdf(file="../figures/GhostPops.pdf", height=9.3, width=15.3)

arrange( pop_ghost_count, K) %>% ggplot( aes(K, Ghosts)) + geom_point() + geom_jitter(height = 0)

dev.off()

write.csv(pop_ghost_count, "../output/PopGhosts.csv")
