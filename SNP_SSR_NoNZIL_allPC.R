rm( list=ls())
#setwd("/Volumes/Storage/RadishData/2005MarkerData/SmartPCA/SNP_SSR_NoNZIL_allPC/")
#setwd("/Volumes/Storage/RadishData/2005MarkerData/SmartPCA/2014SmartPCA/")
setwd("/Volumes/Storage/RadishData/21MarkersData/Analysis/SmartPCA/2014SmartPCAnoNZIL/")
#setwd("/Volumes/Storage/RadishData/2005MarkerData/SmartPCA/2014all_pops/")


#PCA.dat <- read.table("Marker.pca", skip=109)
PCA.dat <- read.table("Marker.pca", skip=11)

#labels.dat <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/MarkerPopEditOrder.csv", header=F, col.names=c("Individual", "Type", "Pop", "Order", "Name", "Species", "Color", "Vernalization", "DTF", "Bins"))

labels.dat <- read.csv("/Volumes/Storage/RadishData/21MarkersData/Analysis/MarkerPopEditOrder2014.csv", header=F, col.names=c("Individual", "Type", "Pop", "Order", "Name", "Species", "Color", "Vernalization", "DTF", "Bins", "locals"))


#Use only for no NZIL runs:
labels.dat <- labels.dat[grep("NZIL", labels.dat$Pop, invert=TRUE),]



#Use for all runs:
labels.dat <- labels.dat[grep("UnknownType", labels.dat$Type, invert=TRUE),]
pca.lab <- data.frame(PCA.dat, labels.dat[grep("RA\\d\\d\\d", labels.dat$Pop, invert=TRUE),]) 
#pca.lab <- data.frame(PCA.dat, labels.dat)


#pca.lab <- pca.lab[(row.names(pca.lab)!="345"),] #This plant has all "0" values from SmartPCA

pca.lab$new.name <- factor( paste(pca.lab$Species, pca.lab$Pop, sep="," ) )
pca.lab <- droplevels(pca.lab)
Crop.col <- "gray0"
lanmar.col <- "dodgerblue4"
raphNN.col <- "firebrick"
raphNat.col <- "tan"
rost.col <- "palegreen3"
conf.col <- "orchid"

c_never <- "dodgerblue4"
b_Forty6To100 <- "tan"
a_less_45 <- "firebrick"

dtf_col <- c(a_less_45, b_Forty6To100, c_never)

cropspec_col <- c("grey", "darkmagenta", "darkorange2", "gray0")

confusus <- 1
Daikon <- 15
European <- 7
landra <- 3
maritimus <- 17
Oilseed <- 2
raphanistrum <- 16
Rattail <- 0
rostratus <- 4
UnknownSp <- 5

species.order <- pca.lab[order(pca.lab$Species),]

raphNN.data <- species.order[species.order$locals=="raphNN",]
lanmar.data <- species.order[species.order$locals=="lanmar",]
Crop.data <- species.order[species.order$Type=="Crop",]
rost.data <-  species.order[species.order$locals=="rostratus",]
raphNat.data <- species.order[species.order$locals=="raphNat",]
conf.data <- species.order[species.order$locals=="confusus",]

raphNN.sym <- c(1:length(levels(droplevels(raphNN.data$Pop))))
lanmar.sym <- c(1:length(levels(droplevels(lanmar.data$Pop))))
Crop.sym <- c(1:length(levels(droplevels(Crop.data$Pop))))
rost.sym <- c(1:length(levels(droplevels(rost.data$Pop))))
raphNat.sym <- c(1:length(levels(droplevels(raphNat.data$Pop))))
conf.sym <- c(1:length(levels(droplevels(conf.data$Pop))))

#pdf(file="squareNoRA_2014DTF_pca_All_FT_rost.pdf", width=8.5, height=8.5)
pdf(file="squareNoRA_pca_CropColors.pdf", width=8.5, height=8.5)

plot((species.order$V1 * -1), 
	(species.order$V2 * -1),
	type="n", 
	xlab="Eigenvector 1", ylab="Eigenvector 2", cex.lab=1.1, 
	#xlim=c(-0.1, 0.2), ylim=c(-0.1, 0.2) #For paper
	#xlim=c(-0.1, 0.11), ylim=c(-0.3, 0.13) #newpops, w/NZIL
	xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.2) #newpops, w/o NZIL
	#xlim=c(-.07, 0.09), ylim=c(-0.2, 0.11)
)

par(new=TRUE)
plot((raphNN.data$V1 * -1), 
	(raphNN.data$V2 * -1), 
	pch=raphNN.sym[droplevels(raphNN.data$new.name)], 
	col=raphNN.col, 
	lwd=2,
	#pch=1,
	#col=dtf_col[raphNN.data$Bins],
	#xlim=c(-0.1, 0.2), ylim=c(-0.1, 0.2),
	#xlim=c(-0.1, 0.11), ylim=c(-0.3, 0.13), #newpops, w/NZIL
	xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.2),#newpops, w/o NZIL
	#xlim=c(-.07, 0.09), ylim=c(-0.2, 0.11),

	axes=FALSE, xlab="", ylab="", cex=1.7)

par(new=TRUE)	
plot((lanmar.data$V1 * -1), 
	(lanmar.data$V2 * -1), 
	pch=lanmar.sym[droplevels(lanmar.data$new.name)], 
	col=lanmar.col,
	lwd=2,
	#pch=2,
	#col=dtf_col[lanmar.data$Bins],
	#xlim=c(-0.1, 0.2), ylim=c(-0.1, 0.2),
	#xlim=c(-0.1, 0.11), ylim=c(-0.3, 0.13), #newpops, w/NZIL
	xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.2),#newpops, w/o NZIL
	#xlim=c(-.07, 0.09), ylim=c(-0.2, 0.11),
	axes=FALSE, xlab="", ylab="", cex=1.7)

par(new=TRUE)
plot((Crop.data$V1 * -1), 
	(Crop.data$V2 * -1), 
	pch=Crop.sym[droplevels(Crop.data$new.name)], 
	col=cropspec_col[droplevels(Crop.data$locals)],
  lwd=2,
  #col=Crop.col, 
	#pch=3,
	#col=dtf_col[Crop.data$Bins],
	#xlim=c(-0.1, 0.2), ylim=c(-0.1, 0.2),
	#xlim=c(-0.1, 0.11), ylim=c(-0.3, 0.13), #newpops, w/NZIL
	xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.2),#newpops, w/o NZIL
	#xlim=c(-.07, 0.09), ylim=c(-0.2, 0.11),
	axes=FALSE, xlab="", ylab="", cex=1.7)
	
par(new=TRUE)
plot((raphNat.data$V1 * -1), 
	(raphNat.data$V2 * -1), 
	pch=raphNat.sym[droplevels(raphNat.data$new.name)], 
	col=raphNat.col, 
	lwd=2,
	#pch=4,
	#col=dtf_col[raphNat.data$Bins],
	#xlim=c(-0.1, 0.2), ylim=c(-0.1, 0.2),
	#xlim=c(-0.1, 0.11), ylim=c(-0.3, 0.13), #newpops, w/NZIL
	xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.2),#newpops, w/o NZIL
	#xlim=c(-.07, 0.09), ylim=c(-0.2, 0.11),
	axes=FALSE, xlab="", ylab="", cex=1.7)	
	
par(new=TRUE)
plot((rost.data$V1 * -1), 
	(rost.data$V2 * -1), 
	pch=rost.sym[droplevels(rost.data$new.name)], 
	col=rost.col,
	lwd=2,
	#pch=5,
	#col=dtf_col[rost.data$Bins], 
	#xlim=c(-0.1, 0.2), ylim=c(-0.1, 0.2),
	#xlim=c(-0.1, 0.11), ylim=c(-0.3, 0.13), #newpops, w/NZIL
	xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.2),#newpops, w/o NZIL
	#xlim=c(-.07, 0.09), ylim=c(-0.2, 0.11),
	axes=FALSE, xlab="", ylab="", cex=1.5)		
	
par(new=TRUE)
plot((conf.data$V1 * -1), 
	(conf.data$V2 * -1), 
	pch=conf.sym[droplevels(conf.data$new.name)], 
	col=conf.col,
	lwd=2,
	#pch=6,
	#col=dtf_col[conf.data$Bins],
	#xlim=c(-0.1, 0.2), ylim=c(-0.1, 0.2),
	#xlim=c(-0.1, 0.11), ylim=c(-0.3, 0.13), #newpops, w/NZIL
	xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.2),#newpops, w/o NZIL
	#xlim=c(-.07, 0.09), ylim=c(-0.2, 0.11),
	axes=FALSE, xlab="", ylab="", cex=1.7)	

########### Plots for paper ####################

	
############## Adding New populations with NZIL ################
#legend(-0.106,-0.215, legend=levels(droplevels(raphNN.data$new.name)), 
#	pch=raphNN.sym, col=raphNN.col, title="Non-native RRR", cex=1)

#legend(0.066, -0.09, legend=levels(droplevels(Crop.data$new.name)), 
#	pch=Crop.sym, col=Crop.col, title="Crop", cex=1)  

#legend(-0.106, -0.14, legend=levels(droplevels(lanmar.data$new.name)), 
#	pch=lanmar.sym, col=lanmar.col, title="landra & maritimus", cex=1)
	
#legend(0.035, -0.132, legend=levels(droplevels(raphNat.data$Pop)), 
#	pch=raphNat.sym, col=raphNat.col, title="native RRR", cex=1)

#legend(0.035, -0.09, legend=levels(droplevels(rost.data$Pop)), 
#	pch=rost.sym, col=rost.col, title="rostratus", cex=1, bty="n")
#rect(0.035, -0.128, 0.064, -0.09)

############## Adding New populations without NZIL ################
legend(-0.004, 0.21, legend=levels(droplevels(raphNN.data$new.name)), 
	pch=raphNN.sym, col=raphNN.col, title="Non-native RRR", cex=1)

legend(-0.107, 0.21, legend=levels(droplevels(Crop.data$new.name)), 
#	pch=Crop.sym, col=Crop.col, title="Crop", cex=1)  
pch=Crop.sym, col=c(rep(cropspec_col[1], 4), rep(cropspec_col[2], 4), rep(cropspec_col[3], 3), rep(cropspec_col[4], 3)), 
title="Crop", cex=1) 

legend(0.055, 0.21, legend=levels(droplevels(lanmar.data$new.name)), 
	pch=lanmar.sym, col=lanmar.col, title="landra & maritimus", cex=1, bty="n")

rect(0.054, 0.162, 0.1065, 0.21)
	
legend(0.079, 0.16, legend=levels(droplevels(raphNat.data$Pop)), 
	pch=raphNat.sym, col=raphNat.col, title="native RRR", cex=1)

legend(0.054, 0.16, legend=levels(droplevels(rost.data$Pop)), 
	pch=rost.sym, col=rost.col, title="rostratus", cex=1)


########################## DTF color Coding #########################

#legend(0.039, 0.21, legend=c("Native raphanistrum", "landra & maritimus", "Non-Native raphanistrum", "Crop varieties", "rostratus"), pch=c(4,2,1,3,5), col="gray0" )
#legend(0.039, 0.15, legend=c("Flowers in < 45 days", "Flowers in 46-100 days", "Flowers in > 100 days"), pch=16, col=c(a_less_45, b_Forty6To100, c_never), bty="n" )
#rect(0.039, 0.15, 0.1065, 0.11)

########################## Simplifed legend for Evo #################

#legend(0.003, 0.21, legend=c("Wild sub-species", "Native range raphanistrum", "World raphanistrum", "Crop varieties", "Rostratus"),cex=1.5, pch=16, col=c(lanmar.col, raphNat.col, raphNN.col, Crop.col, rost.col) )

########################## All pops except NZIL######################
#legend(0.065, 0.12, legend=levels(droplevels(raphNN.data$Pop)), 
#	pch=raphNN.sym, col=raphNN.col, title="Non-native R.r.r", cex=1)

#legend(0.057, -0.05, legend=levels(droplevels(Crop.data$new.name)), 
#	pch=Crop.sym, col=Crop.col, title="Crop", cex=1)  

#legend(-0.076, -0.12, legend=levels(droplevels(lanmar.data$Pop)), 
#	pch=lanmar.sym, col=lanmar.col, title="landra & maritimus", cex=1, ncol=2)

#legend(0.016, -0.08, legend=levels(droplevels(raphNat.data$Pop)), 
#	pch=raphNat.sym, col=raphNat.col, title="native R.r.r", ncol=2, cex=1)

#legend(-0.0035, -0.18, legend=levels(droplevels(rost.data$Pop)), 
#	pch=rost.sym, col=rost.col, title="rostratus", cex=1)
	

dev.off()

#Just Crops
plot((Crop.data$V1 * -1), 
     (Crop.data$V2 * -1), 
     pch=Crop.sym[droplevels(Crop.data$new.name)], 
     col=cropspec_col[droplevels(Crop.data$locals)],
     lwd=2, cex=2)
legend( -.09, .04, legend=levels(droplevels(Crop.data$locals)), pch=16,
        col=cropspec_col, bty='n', ncol=2)
