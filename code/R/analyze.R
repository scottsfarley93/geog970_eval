# install.packages('FactoMineR')
# install.packages('fclust')
# install.packages('arules')
# install.packages('arulesViz')
library(FactoMineR)
library(fclust)
library(arules)
library(arulesViz)
library(cluster)
library(vegan)
library (plyr)

# dat <- read.csv("./../../data/data.csv")
# 
# boolNames <- c("hasAuthor", "hasOrganization", "hasTitle", "hasDescription", "usedTaxonIDNumber", 
#                "configdata.state.openSite", "configdata.state.layout.westPanelIsOpen", "configdata.state.layout.southPanelIsOpen", 
#                "configdata.state.layout.eastPanelIsOpen", "configdata.state.doSearch")
# 
# realNames <- c("configdata.state.map.zoom", "configdata.state.map.pitch", "configdata.state.map.center.lng", 
#                "configdata.state.map.center.lat", "configdata.state.map.bearing", "configdata.config.timer.totalElapsed",
#                "configdata.config.timer.loadElapsed")
# 
# 
# dat.bool <- dat[boolNames]
# 
# for (col in boolNames){
#   dat.bool[,col] <- as.logical(dat.bool[,col])
#   dat.bool[, col][is.na(dat.bool[, col])] <- FALSE
# }
# 
# dat.real <- dat[realNames]
# 
# for (col in realNames){
#   dat.real[,col] <- as.numeric(dat.real[,col])
#   dat.real[, col][is.na(dat.real[, col])] <- NaN
# }
# 
# dat.real <- scale(dat.real)
# 
# dat.join <- cbind(dat.real, dat.bool)



### Generate test Data 

## assign probabilities to test data

## generate vectors of true/false 
## from probability models corresponding to potential classes 

## For boolean layout data 
## compute probability of 1 success in 1 trial with probability p 
## repeat N times for N test 'maps'
N <- 100
trials <- 1
student <- list()
student$hasAuthor <- as.logical(rbinom(N, trials, 0.9))
student$hasOrganization <- as.logical(rbinom(N, trials, 0.7))
student$hasTitle <- as.logical(rbinom(N, trials, 0.6))
student$hasDescription <- as.logical(rbinom(N, trials, 0.4))
student$usedTaxonIDNumber <- as.logical(rbinom(N, trials, 0.9)) ## more likely to browse than search
student$configdata.state.openSite <- as.logical(rbinom(N, trials, 0.2)) ## not likely to identify specific sites 
student$configdata.state.layout.westPanelIsOpen <- as.logical(rbinom(N, trials, 0.2)) ## not likely to identify specific sites
student$configdata.state.layout.southPanelIsOpen <- as.logical(rbinom(N, trials, 0.8)) ## not likely to change default, and provides context
student$configdata.state.layout.eastPanelIsOpen <- as.logical(rbinom(N, trials, 0.8)) ## not likely to change default
student$configdata.state.doSearch <- as.logical(rbinom(N, trials, 0.9)) ##student's unlikely to share without data on map

## for continuous data 
studentZoom.mean <- 3 ## continental level zoom 
studentZoom.sd <- 2 
student$configdata.state.map.zoom <- rgamma(N, studentZoom.mean, studentZoom.sd)
## don't do pitch and bearing 
## don't do geographic center

studentTimeTotal.mean <- 3*60 ## minutes 
studentTimeTotal.sd <- 2*60 ## minutes 
student$configdata.config.timer.totalElapsed <- rgamma(N, studentTimeTotal.mean, studentTimeTotal.sd)

studentTimeData.mean <- 2*60 
studentTimeData.sd <- 2*60
student$configdata.config.timer.loadElapsed <- rgamma(N, studentTimeData.mean, studentTimeData.sd)

## convert to data frame
student.dat <- do.call(cbind, lapply(student, data.frame, stringsAsFactors=FALSE))
names(student.dat) <- names(student)







research <- list()
research$hasAuthor <- as.logical(rbinom(N, trials, 0.95))
research$hasOrganization <- as.logical(rbinom(N, trials, 0.95))
research$hasTitle <- as.logical(rbinom(N, trials, 0.9)) ## likely to give title and description
research$hasDescription <- as.logical(rbinom(N, trials, 0.9)) 
research$usedTaxonIDNumber <- as.logical(rbinom(N, trials, 0.2)) ## more likely to search than browse
research$configdata.state.openSite <- as.logical(rbinom(N, trials, 0.4)) ## might open a specific site
research$configdata.state.layout.westPanelIsOpen <- as.logical(rbinom(N, trials, 0.4)) ## not likely to identify specific sites
research$configdata.state.layout.southPanelIsOpen <- as.logical(rbinom(N, trials, 0.4)) ## may close timeline panel to free open real estate
research$configdata.state.layout.eastPanelIsOpen <- as.logical(rbinom(N, trials, 0.65)) ## not likely to change default
research$configdata.state.doSearch <- as.logical(rbinom(N, trials, 0.85)) ##may share a map without any data to advertise the tool (?)


## for continuous data 
researchZoom.mean <- 6 ## higher zoom levels 
researchZoom.sd <- 2 ## focus on areas of personal interest 
research$configdata.state.map.zoom <- rgamma(N, researchZoom.mean, researchZoom.sd)
## don't do pitch and bearing 
## don't do geographic center

researchTimeTotal.mean <- 10*60 ## minutes --> profs spend longer
researchTimeTotal.sd <- 3*60 ## minutes 
research$configdata.config.timer.totalElapsed <- rgamma(N, researchTimeTotal.mean, researchTimeTotal.sd)

researchTimeData.mean <- 2*60 
researchTimeData.sd <- 2*60
research$configdata.config.timer.loadElapsed <- rgamma(N, researchTimeData.mean, researchTimeData.sd)

## to data frame
research.dat <- do.call(cbind, lapply(research, data.frame, stringsAsFactors=FALSE))
names(research.dat) <- names(research)


## make sure we know which classes are associated with which rows 
research.dat$class <- "Research"
student.dat$class <- "Student"

## combine the data into single df
dat <- rbind(research.dat, student.dat)

## randomzie rows
dat <- dat[sample(nrow(dat)),]

dat.noclass <- dat[, -which(names(dat) == "class")]

## conver to numerical matrix 
dat.num <- data.matrix(dat.noclass)

f <- fanny(dat.num, 2)

plot(f)

dat$cluster1 <- f$membership[, 1]
dat$cluster2 <- f$membership[, 2]

plot(dat$cluster1, dat$cluster2, col=as.factor(dat$class))


dat.pca <- prcomp(dat.noclass)

biplot(dat.pca,  col=c("transparent", 'green'), cex=0.1)

