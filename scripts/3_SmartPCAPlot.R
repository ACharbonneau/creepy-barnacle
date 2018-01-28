rm( list=ls())
# Install function for packages    
packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

packages(RColorBrewer)
packages(pheatmap)
packages(dplyr)
packages(ggplot2)



#SmartPCA drops the ID's but keeps them in the same order, so bind output to input

PCA.dat <- read.table("Marker.pca", skip=11)

PCA.ind <- read.table("../MungedData/Marker.ind")

colnames(PCA.ind) <- c("Individual", "U", "PopNum")

pca.lab <- cbind(PCA.ind, PCA.dat)

#Get other useful labels and join

labels.dat <- read.csv("../OriginalData/MarkerPopOrder.csv", header=F, 
                       col.names=c("Individual", "Type", "Pop", "Order", "Name", "Species", "Color", "Vernalization", "DTF", "Bins", "locals"))

pca.lab <- full_join(pca.lab, labels.dat)

pca.lab$new.name <- factor( paste( pca.lab$Name, " (", pca.lab$Pop, ")", sep="" ) )
pca.lab <- droplevels(pca.lab)


pdf(file="../Figures/SmartPCA.pdf", width=10, height=8.5)


## Compute variance explained

pca.eval <- data.table::fread("Marker.eval")

v1var <- (pca.eval[1,1] / sum(pca.eval)) * 100
v2var <- (pca.eval[2,1] / sum(pca.eval)) * 100

v1_10var <- (sum(pca.eval[1:10,]) / sum(pca.eval)) * 100

#Divergence <- read.table("../OrigOutput/Marker.log", skip=60, header = T, nrows = 9)

#Differences <- read.table("../OrigOutput/Marker.log", skip=581, header = T, nrows = 36, row.names = NULL)

#Set up plotting colors

## Add an alpha value to a colour
add.alpha <- function(col, alpha=.7){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

col_pal <- brewer.pal(9, "Set1")
col6 <- brewer.pal(6, "Set2")
col_pal <- c(col_pal[c(1:5)], col6[6], col_pal[c(8,9)], "gray20", "black")

ALLTHECOLORS <- add.alpha(col_pal)

CropD.col <- ALLTHECOLORS[6]
CropE.col <- ALLTHECOLORS[1]
CropO.col <- ALLTHECOLORS[3]
CropR.col <- ALLTHECOLORS[8]

lanmar.col <- ALLTHECOLORS[7]
raphNN.col <- ALLTHECOLORS[5]
raphNatW.col <- ALLTHECOLORS[4]
raphNatE.col <- ALLTHECOLORS[2]
rost.col <-  ALLTHECOLORS[10]
conf.col <- "orchid"

c_never <- "dodgerblue4"
b_Forty6To100 <- "tan"
a_less_45 <- "firebrick"

dtf_col <- c(a_less_45, b_Forty6To100, c_never)

cropspec_col <- c("grey", "darkmagenta", "darkorange2", "gray0")

#Set up plotting symbols

# Subset data and order for plotting

species.order <- pca.lab[order(pca.lab$Species),]

raphNN.data <- species.order[species.order$locals=="raphNN",]
lanmar.data <- species.order[species.order$locals=="lanmar",]
CropD.data <- species.order[species.order$locals=="Daikon",]
CropE.data <- species.order[species.order$locals=="European",]
CropO.data <- species.order[species.order$locals=="Oilseed",]
CropR.data <- species.order[species.order$local=="Rattail",]
rost.data <-  species.order[species.order$locals=="rostratus",]
raphNatW.data <- species.order[species.order$locals=="raphNatW",]
raphNatE.data <- species.order[species.order$locals=="raphNatE",]
conf.data <- species.order[species.order$locals=="confusus",]

raphNN.sym <- c(21,24,3:length(levels(droplevels(raphNN.data$Pop))))
lanmar.sym <- c(1:length(levels(droplevels(lanmar.data$Pop))))
CropD.sym <- c(1:length(levels(droplevels(CropD.data$Pop))))
CropE.sym <- c(1:length(levels(droplevels(CropE.data$Pop))))
CropO.sym <- c(1:length(levels(droplevels(CropO.data$Pop))))
CropR.sym <- c(21, 24, 25)
rost.sym <- 21
raphNatW.sym <- c(1:length(levels(droplevels(raphNatW.data$Pop))))
raphNatE.sym <- c(1:length(levels(droplevels(raphNatE.data$Pop))))
conf.sym <- c(1:length(levels(droplevels(conf.data$Pop))))

tempar <- par()$mar


par( xpd=TRUE, mar=c(5.1, 5.1, 5.1, 10))

plot((species.order$V1 ), 
	(species.order$V2 * -1),
	type="n", 
	xlab=paste("Eigenvector 1; ", round(v1var, 2), "% of variance", sep = ""), 
	           ylab=paste("Eigenvector 2; ", round(v2var, 2),"% of variance", sep = ""), cex.lab=1.1, 
	xlim=c(-0.12, 0.10), ylim=c(-0.2, 0.1) #newpops, w/o NZIL
)

par(new=TRUE)
plot((raphNN.data$V1 ), 
	(raphNN.data$V2 * -1), 
	pch=raphNN.sym[droplevels(raphNN.data$new.name)], 
	col=raphNN.col, bg=ALLTHECOLORS[8],
	lwd=2.5,
	xlim=c(-0.12, 0.10), ylim=c(-0.2, 0.1),#newpops, w/o NZIL
	axes=FALSE, xlab="", ylab="", cex=1.7)

par(new=TRUE)	
plot((lanmar.data$V1 ), 
	(lanmar.data$V2 * -1), 
	pch=lanmar.sym[droplevels(lanmar.data$new.name)], 
	col=lanmar.col,
	lwd=2.5,
	xlim=c(-0.10, 0.10), ylim=c(-0.2, 0.1),#newpops, w/o NZIL
	axes=FALSE, xlab="", ylab="", cex=1.7)

par(new=TRUE)
plot((CropD.data$V1 ), 
	(CropD.data$V2 * -1), 
	pch=CropD.sym[droplevels(CropD.data$new.name)], 
  lwd=2.5,
  col=CropD.col, 
	xlim=c(-0.12, 0.10), ylim=c(-0.2, 0.1),#newpops, w/o NZIL
	axes=FALSE, xlab="", ylab="", cex=1.7)

par(new=TRUE)
plot((CropE.data$V1 ), 
     (CropE.data$V2 * -1), 
     pch=CropE.sym[droplevels(CropE.data$new.name)], 
     lwd=2.5,
     col=CropE.col, 
     xlim=c(-0.12, 0.10), ylim=c(-0.2, 0.1),#newpops, w/o NZIL
     axes=FALSE, xlab="", ylab="", cex=1.7)

par(new=TRUE)
plot((CropO.data$V1 ), 
     (CropO.data$V2 * -1), 
     pch=CropO.sym[droplevels(CropO.data$new.name)], 
     lwd=2.5,
     col=CropO.col, 
     xlim=c(-0.12, 0.10), ylim=c(-0.2, 0.1),#newpops, w/o NZIL
     axes=FALSE, xlab="", ylab="", cex=1.7)

par(new=TRUE)
plot((CropR.data$V1 ), 
     (CropR.data$V2 * -1), 
     pch=CropR.sym[droplevels(CropR.data$new.name)], 
     bg=ALLTHECOLORS[8],
     lwd=2.5,
     col=CropR.col, 
     xlim=c(-0.12, 0.10), ylim=c(-0.2, 0.1),#newpops, w/o NZIL
     axes=FALSE, xlab="", ylab="", cex=1.7)
	
par(new=TRUE)
plot((raphNatW.data$V1 ), 
	(raphNatW.data$V2 * -1), 
	pch=raphNatW.sym[droplevels(raphNatW.data$new.name)], 
	col=raphNatW.col, 
	lwd=2.5,
	xlim=c(-0.12, 0.10), ylim=c(-0.2, 0.1),#newpops, w/o NZIL
	axes=FALSE, xlab="", ylab="", cex=1.7)	

par(new=TRUE)
plot((raphNatE.data$V1 ), 
     (raphNatE.data$V2 * -1), 
     pch=raphNatE.sym[droplevels(raphNatE.data$new.name)], 
     col=raphNatE.col, 
     lwd=2.5,
     xlim=c(-0.12, 0.10), ylim=c(-0.2, 0.1),#newpops, w/o NZIL
     axes=FALSE, xlab="", ylab="", cex=1.7)	
	
par(new=TRUE)
plot((rost.data$V1 ), 
	(rost.data$V2 * -1), 
	pch=rost.sym[droplevels(rost.data$new.name)], 
	bg=ALLTHECOLORS[8],
	col=rost.col,
	lwd=2.5,
	xlim=c(-0.12, 0.10), ylim=c(-0.2, 0.1),#newpops, w/o NZIL
	axes=FALSE, xlab="", ylab="", cex=1.5)		
	
par(new=TRUE)
plot((conf.data$V1 ), 
	(conf.data$V2 * -1), 
	pch=conf.sym[droplevels(conf.data$new.name)], 
	col=conf.col,
	lwd=2.5,
	xlim=c(-0.12, 0.10), ylim=c(-0.2, 0.1),#newpops, w/o NZIL
	axes=FALSE, xlab="", ylab="", cex=1.7)	

########### Plots for paper ####################

# Landra and maritimus

rect(0.059, -0.165, 0.119, -0.118, col="white")

legend(0.065,-0.12, legend=levels(droplevels(lanmar.data$new.name)), 
       pch=lanmar.sym, col=col_pal[7], title=expression(italic("R.r. landra")), cex=.9, bty="n" )


# Non Natives
rect(-0.08, -0.16, -0.025, -0.08, col="white")
legend( -0.08, -0.095, legend=levels(droplevels(raphNN.data$new.name)), 
       pch=raphNN.sym, col=raphNN.col, pt.bg=ALLTHECOLORS[8], 
       title = expression( italic("R.r. raphanistrum")), cex=.9, bty="n" )

text(-0.052, -0.09, "Non-native" , cex=.9)


# Native RRR
rect(-0.127, 0.06, -0.04, 0.13, col="white")

legend(-0.127, 0.121, legend=levels(droplevels(raphNatW.data$new.name)), 
       pch=raphNatW.sym, col=raphNatW.col, cex=.9, bty="n" )

legend(-0.085, 0.118, legend=levels(droplevels(raphNatE.data$new.name)), 
       pch=raphNatE.sym, col=raphNatE.col, bty="n" )

text(-0.103, 0.124, expression( "Native" ), cex=.9)
text(-0.074, 0.1235, expression(italic("R.r. raphanistrum")), cex=.9)

## R. pugioniformis
rect(-0.0, 0.085, 0.042, 0.12, col="white")
legend(-0.0, 0.115, legend=levels(droplevels(rost.data$new.name)), 
       pch=rost.sym, pt.bg=ALLTHECOLORS[8],
       col="black", title=expression(italic("R. pugioniformis")), cex=.9, bty = "n" )



# Crops
rect(0.095, -0.045, 0.17, 0.105, col="white")

legend(0.095, 0.10, legend=levels(droplevels(CropD.data$new.name)), 
       pch=CropD.sym, col=CropD.col, title="Crop", cex=.9, bty="n" ) 

legend(0.095, 0.055, legend=levels(droplevels(CropE.data$new.name)), 
       pch=CropE.sym, col=CropE.col, cex=.9, bty="n" )  

legend(0.095, 0.019, legend=levels(droplevels(CropO.data$new.name)), 
       pch=CropO.sym, col=CropO.col, cex=.9, bty="n" )  

legend(0.095, -0.008, legend=levels(droplevels(CropR.data$new.name)), 
       pch=CropR.sym, col=CropR.col, pt.bg=ALLTHECOLORS[8], cex=.9, bty="n" )  



dev.off()

#pdf(file="../figures/SmartPCADivergence.pdf", width=5, height=5)

#pheatmap(Divergence[1:9,1:9], cluster_rows=TRUE, show_rownames=TRUE, cluster_cols=TRUE)

#dev.off()

