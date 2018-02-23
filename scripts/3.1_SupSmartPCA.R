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

pdf(file="../Figures/SuppSmartPCA.pdf", width=10, height=8.5)


labels.dat <- read.csv("../OriginalData/MarkerPopOrder.csv", header=F,
                       col.names=c("Individual", "Type", "Pop", "Order", "Name", "Species", "Color", "Vernalization", "DTF", "Bins", "locals"))
#Just for making supplement
biallele <- read.table("../MungedData//AllBialleleGeno.csv", sep = ",", header = T)
meta <- select(biallele, SSR.ID, Pop)
prbialelle <- as.data.frame(na.omit(select(biallele, -Pop)))

JustPCA <- prcomp(as.data.frame(select(prbialelle, -SSR.ID)))

JustPCAX <- as.data.frame(JustPCA$x)
JustPCAX$UniqID <- prbialelle$SSR.ID
JustPCAX <- left_join(JustPCAX, meta, by=c(UniqID="SSR.ID"))
pca.lab <- left_join(JustPCAX, labels.dat, by=c(UniqID="Individual", Pop="Pop"))
pca.lab$Pop <- as.factor(pca.lab$Pop)
pca.lab$new.name <- factor( paste( pca.lab$Name, " (", pca.lab$Pop, ")", sep="" ) )


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

plot((species.order$PC1 ),
     (species.order$PC2 * -1),
     type="n",
     xlab="PC1", ylab="PC2", cex.lab=1.1,
     xlim=c(-4, 4), ylim=c(-5, 3) #newpops, w/o NZIL
)

par(new=TRUE)
plot((raphNN.data$PC1 ),
     (raphNN.data$PC2 * -1),
     pch=raphNN.sym[droplevels(raphNN.data$new.name)],
     col=raphNN.col, bg=ALLTHECOLORS[8],
     lwd=2.5,
     xlim=c(-4, 4), ylim=c(-5, 3),#newpops, w/o NZIL
     axes=FALSE, xlab="", ylab="", cex=1.7)

par(new=TRUE)
plot((lanmar.data$PC1 ),
     (lanmar.data$PC2 * -1),
     pch=lanmar.sym[droplevels(lanmar.data$new.name)],
     col=lanmar.col,
     lwd=2.5,
     xlim=c(-4, 4), ylim=c(-5, 3),#newpops, w/o NZIL
     axes=FALSE, xlab="", ylab="", cex=1.7)

par(new=TRUE)
plot((CropD.data$PC1 ),
     (CropD.data$PC2 * -1),
     pch=CropD.sym[droplevels(CropD.data$new.name)],
     lwd=2.5,
     col=CropD.col,
     xlim=c(-4, 4), ylim=c(-5, 3),#newpops, w/o NZIL
     axes=FALSE, xlab="", ylab="", cex=1.7)

par(new=TRUE)
plot((CropE.data$PC1 ),
     (CropE.data$PC2 * -1),
     pch=CropE.sym[droplevels(CropE.data$new.name)],
     lwd=2.5,
     col=CropE.col,
     xlim=c(-4, 4), ylim=c(-5, 3),#newpops, w/o NZIL
     axes=FALSE, xlab="", ylab="", cex=1.7)

par(new=TRUE)
plot((CropO.data$PC1 ),
     (CropO.data$PC2 * -1),
     pch=CropO.sym[droplevels(CropO.data$new.name)],
     lwd=2.5,
     col=CropO.col,
     xlim=c(-4, 4), ylim=c(-5, 3),#newpops, w/o NZIL
     axes=FALSE, xlab="", ylab="", cex=1.7)

par(new=TRUE)
plot((CropR.data$PC1 ),
     (CropR.data$PC2 * -1),
     pch=CropR.sym[droplevels(CropR.data$new.name)],
     bg=ALLTHECOLORS[8],
     lwd=2.5,
     col=CropR.col,
     xlim=c(-4, 4), ylim=c(-5, 3),#newpops, w/o NZIL
     axes=FALSE, xlab="", ylab="", cex=1.7)

par(new=TRUE)
plot((raphNatW.data$PC1 ),
     (raphNatW.data$PC2 * -1),
     pch=raphNatW.sym[droplevels(raphNatW.data$new.name)],
     col=raphNatW.col,
     lwd=2.5,
     xlim=c(-4, 4), ylim=c(-5, 3),#newpops, w/o NZIL
     axes=FALSE, xlab="", ylab="", cex=1.7)

par(new=TRUE)
plot((raphNatE.data$PC1 ),
     (raphNatE.data$PC2 * -1),
     pch=raphNatE.sym[droplevels(raphNatE.data$new.name)],
     col=raphNatE.col,
     lwd=2.5,
     xlim=c(-4, 4), ylim=c(-5, 3),#newpops, w/o NZIL
     axes=FALSE, xlab="", ylab="", cex=1.7)

par(new=TRUE)
plot((rost.data$PC1 ),
     (rost.data$PC2 * -1),
     pch=rost.sym[droplevels(rost.data$new.name)],
     bg=ALLTHECOLORS[8],
     col=rost.col,
     lwd=2.5,
     xlim=c(-4, 4), ylim=c(-5, 3),#newpops, w/o NZIL
     axes=FALSE, xlab="", ylab="", cex=1.5)

par(new=TRUE)
plot((conf.data$PC1 ),
     (conf.data$PC2 * -1),
     pch=conf.sym[droplevels(conf.data$new.name)],
     col=conf.col,
     lwd=2.5,
     xlim=c(-4, 4), ylim=c(-5, 3),#newpops, w/o NZIL
     axes=FALSE, xlab="", ylab="", cex=1.7)

########### Plots for paper ####################

# Landra and maritimus

rect(2.2, -3.5, 4.6, -2, col="white")

legend(2.5,-2.2, legend=levels(droplevels(lanmar.data$new.name)),
       pch=lanmar.sym, col=col_pal[7], title=expression(italic("R.r. landra")),
       title.adj=.2,cex=.9, bty="n" )


# Non Natives
rect(-3.2, -4.5, -.7, -2.2, col="white")
legend( -2.9,-2.65, legend=levels(droplevels(raphNN.data$new.name)),
        pch=raphNN.sym, col=raphNN.col, pt.bg=ALLTHECOLORS[8], title.adj=.2,
        title = expression( italic("R.r. raphanistrum")), cex=.9, bty="n" )

text(-2., -2.5, "Non-native" , cex=.9)


# Native RRR
rect(-4.2, 2.1, -.8, 4.1, col="white")

legend(-4.2, 3.8, legend=levels(droplevels(raphNatW.data$new.name)),
       pch=raphNatW.sym, col=raphNatW.col, cex=.9, bty="n" )

legend(-2.5, 3.8, legend=levels(droplevels(raphNatE.data$new.name)),
       pch=raphNatE.sym, col=raphNatE.col, bty="n" )

text(-3.1, 3.9, expression( "Native" ), cex=.9)
text(-2.05, 3.88, expression(italic("R.r. raphanistrum")), cex=.9)

## R. pugioniformis
rect(-.2, 2.3, 1.5, 3.1, col="white")
legend(-0.1, 3, legend=levels(droplevels(rost.data$new.name)),
       pch=rost.sym, pt.bg=ALLTHECOLORS[8],
       col="black", title=expression(italic("R. pugioniformis")), cex=.9, bty = "n" )



# Crops
rect(3.5, 0, 6.5, 3, col="white")

legend(3.6, 3, legend=levels(droplevels(CropD.data$new.name)),
       pch=CropD.sym, col=CropD.col, title=expression(italic("R. sativus")), cex=.9, bty="n" )

legend(3.6, 2, legend=levels(droplevels(CropE.data$new.name)),
       pch=CropE.sym, col=CropE.col, cex=.9, bty="n" )

legend(3.6, 1.2, legend=levels(droplevels(CropO.data$new.name)),
       pch=CropO.sym, col=CropO.col, cex=.9, bty="n" )

legend(3.6, .7, legend=levels(droplevels(CropR.data$new.name)),
       pch=CropR.sym, col=CropR.col, pt.bg=ALLTHECOLORS[8], cex=.9, bty="n" )


dev.off()
