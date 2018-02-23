# Install function for packages    
packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}
packages(RColorBrewer)
packages(ggplot2)
packages(dplyr)

########################################################
ApproxBayesFac <- function(x) {
  x <- x[!is.na(x)]
  max.probs <- max(x)
  new.probs <- x - max.probs
  trans.probs <- exp(new.probs)
  BayesProb <- trans.probs/sum(trans.probs)
  return(BayesProb)
}
########################################################

# Estimate best K

Prob_data <- read.csv("AlltheProbabilities.txt", header=F,sep=",")

colnames(Prob_data) <- c("RunNumber", "KNumber", "Estimated_Ln_Prob_of_Data", "Mean_value_of_ln_likelihood", "Variance_of_ln_likelihood")

ghost_count <- c(0,0,0) #c("K", "Run", "Ghosts")
pop_ghost_count <- c(0,0,0)

# Plot probabilities

probs <- list()
for(i in levels(as.factor(Prob_data$RunNumber))){
  probs[[i]] <- cbind(Prob_data$KNumber[Prob_data$RunNumber==i], 	Prob_data$Estimated_Ln_Prob_of_Data[Prob_data$RunNumber==i],
                      ApproxBayesFac(Prob_data$Estimated_Ln_Prob_of_Data[Prob_data$RunNumber==i])
  )
}

pdf(file="../Figures/STRUCTUREprobs.pdf")

for(i in levels(as.factor(Prob_data$RunNumber))){
  plot(probs[[i]][ order(probs[[i]][,1]) ,3]#probs[[i]][,3]
       ,xlab="Proposed K", ylab="Approximate Bayes Factor",
       main=paste("Run", i, sep=" "))

  text(x=seq(1,max(Prob_data$KNumber)),
       y=probs[[i]][ order(probs[[i]][,1]),3]#probs[[i]][,3]
       , pos=1, cex=.7)
}

BestK = rep(NA, length(levels(as.factor(Prob_data$RunNumber))))

for(a in seq(1,length(levels(as.factor(Prob_data$RunNumber))))){
  BestK[a] <- probs[[a]][,1] [probs[[a]][,3]== max(probs[[a]][,3])]
}

BestK = BestK[seq(1,length(levels(as.factor(Prob_data$RunNumber))))]
plot(density(BestK, bw=.4), main= "Density Plot of Best Ks")


dev.off()

########################################################

### Plot STRUCTURE bargraphs

pdf(file="../Figures/STRUCTURE_All.pdf", height=9.3, width=15.3)

##Get all the files from this directory to put into a single PDF
#Sort the filelist into numerical order by K instead of order from filesystem

ALLTHEFILES <- dir("parsed_data/")

File_Num <- length(ALLTHEFILES)

File_list <- data.frame(matrix(unlist(strsplit(ALLTHEFILES, "\\_|\\-")), ncol = 4, byrow = T))
File_list$runname <- ALLTHEFILES
colnames(File_list) <- c("randomization", "structure", "K", "f.parsed", "runname")
File_list$K <- as.numeric(levels(File_list$K))[File_list$K]
File_list$randomization <- as.numeric(levels(File_list$randomization))[File_list$randomization]
File_list <- select(File_list, K, randomization, runname)
File_list <- arrange(File_list, randomization, K)


#Sort each STRUCTURE file and metadata file by the plant ID number then combine them. This gets rid
#of the random order of indivduals needed to run STRUCTURE


for( strrun in c(1:length(File_list$K))){
  #strrun <- 1
  dataset <- File_list[strrun, 3]
  str.data <- 0
  str.data <- read.csv(paste("parsed_data/",dataset, sep=""), header=F)
  K <- File_list[strrun, 1] # Find out what K is
  str.data <- str.data[,c(2,3,5:ncol(str.data))] # Get only useful columns from STRUCTURE
  colnames(str.data) <- c( "Individual", "%missing",1:K)
  str.data$ID <- unlist(strsplit(as.character(str.data$Individual), "_QTL.+"))

  #Get the label/metadata about each individual from a seperate file.

  labels <- read.csv("../OriginalData/MarkerPopOrder.csv", header=F,
           col.names=c("ID", "Type", "Pop", "Order", "Name", "Species", "Color", "Vernalization", "DTF", "Bins", "locals"))

  labels$ID <- as.character(labels$ID)

  all.data <- left_join(str.data, labels)


  ghosts <- apply(all.data[,3:(2+K)], 2, max)
  newghost <- c(File_list$K[strrun], File_list$randomization[strrun], sum(ghosts < .5))
  ghost_count <- rbind(ghost_count, newghost)

  popghosts <- aggregate(all.data[3:(2+K)], by=list(all.data$Pop), FUN=mean)
  popghosts <- apply(popghosts, 2, max)
  newpopghost <- c(File_list$K[strrun], File_list$randomization[strrun], sum(popghosts < .5))
  pop_ghost_count <- rbind(pop_ghost_count, newpopghost)

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

dev.off()

ghost_count <- data.frame(ghost_count)
colnames(ghost_count) <- c("K", "Run", "Ghosts")
ghost_count <- ghost_count[2:nrow(ghost_count), ]

pdf(file="../Figures/GhostInd.pdf", height=9.3, width=15.3)

arrange( ghost_count, K) %>% ggplot( aes(K, Ghosts)) + geom_point() + geom_jitter(height = 0)

dev.off()

write.csv(ghost_count, "../output/ghosts.csv")

pop_ghost_count <- data.frame(pop_ghost_count)

colnames(pop_ghost_count) <- c("K", "Run", "Ghosts")
pop_ghost_count <- pop_ghost_count[2:nrow(pop_ghost_count), ]

pdf(file="../Figures/GhostPops.pdf", height=9.3, width=15.3)

arrange( pop_ghost_count, K) %>% ggplot( aes(K, Ghosts)) + geom_point() + geom_jitter(height = 0)

dev.off()

write.csv(pop_ghost_count, "../output/PopGhosts.csv")
