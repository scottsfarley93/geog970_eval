# install.packages('FactoMineR')
# install.packages('fclust')
# install.packages('arules')
# install.packages('arulesViz')
# install.packages('ggfortify')
# install.packages('factoextra')
library(FactoMineR)
library(fclust)
library(arules)
library(arulesViz)
library(cluster)
library(vegan)
library (plyr)
library(ggplot2)
library(ggfortify)
library(factoextra)

source("./input.data.R")


### Generate test Data 
student.dat <- generateTheoreticalData(2000, "Student", "./../../models/student.model.csv")
research.dat <- generateTheoreticalData(2000, "Research", "./../../models/research.model.csv")


## do a principle components analysis
dat <- rbind(research.dat, student.dat)
## randomzie rows
dat <- dat[sample(nrow(dat)),]
dat.noclass <- dat[, -which(names(dat) == "class")] ## remove labels

doScale = T
if (doScale){
  dat.noclass <- scale(dat.noclass)
}

dat.pca <- prcomp(dat.noclass)
autoplot(dat.pca, data=dat, colour='class',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3) + ggtitle("Priciple Components")

pctExplained <- dat.pca$sdev^2 / sum(dat.pca$sdev^2)

plot(pctExplained, type='l') ## percentage variance explained by each PC
fviz_pca_contrib(dat.pca, axes=1)
fviz_pca_contrib(dat.pca, axes=2)

## do fuzzy clustering
k = 2
clusters <- fanny(dat.noclass, k, memb.exp = 1.1)

fviz_cluster(clusters)

plot(clusters, which=1)
plot(clusters, which=2, main=paste("Silhouette Plot of k=", 2))

mem <- data.frame(clusters$membership)
mem$class <- as.factor(dat$class)
names(mem) <- c("C1", "C2", "class")


ggplot(mem, aes(x = C1, y=C2, color=C1)) + geom_point()




