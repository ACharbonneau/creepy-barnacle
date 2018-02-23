require(ggplot2)
DisF <- read.csv("../OriginalData/Weed_Distance_FST.csv", header=T)

pdf(file="../figures/IBD.pdf", height=5, width=5)

correlation <- cor(DisF$FST, DisF$Distance )
corrp <- cor.test(DisF$FST, DisF$Distance )

ggplot(DisF, aes(Distance, FST)) +
         ggtitle("Non-Native Isolation by Distance") +
         annotate("text", x = 5000, y = .2, label = paste("R2=", round(correlation, 3), sep=" ")) +
         annotate("text", x = 5000, y = .19, label = paste("p=", round(corrp$p.value, 3), sep=" ")) +
         geom_point(size=3) +
         theme_minimal()
smooth_vals = lm(FST~Distance, DisF)

dev.off()
