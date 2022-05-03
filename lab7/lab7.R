rm(list=ls())

setwd("/Users/ericliao/Desktop/phD_courses/2022Spring_class/Advanced_Stat/lab/homework/lab7")

inFileName <- paste("prePostPhylum.txt", sep ="")

### Quesiton (1)
myT <-read.table(inFileName,header=TRUE,sep="\t")
numCols <- ncol(myT)
myColClasses <- c(rep("character",4), rep("numeric", numCols-4))
myT <-read.table(inFileName,header=TRUE,sep="\t",colClasses=myColClasses)

myTData<-myT[,5:10]

myPCA <- princomp(myTData, scale.=TRUE)

library(ggplot2)
library(ggfortify)

### Question (2)
autoplot(myPCA, data = myT, colour = 'genotype')
autoplot(myPCA, data = myT, colour = 'cage')
autoplot(myPCA, data = myT, colour = 'time')

### Question (3)
p1_cage <- anova(lm(myPCA$scores[,1] ~ myT$cage))$"Pr(>F)"[1]
p2_cage <- anova(lm(myPCA$scores[,2] ~ myT$cage))$"Pr(>F)"[1]
p1_genotype <- anova(lm(myPCA$scores[,1] ~ myT$genotype))$"Pr(>F)"[1]
p2_genotype <- anova(lm(myPCA$scores[,1] ~ myT$genotype))$"Pr(>F)"[1]
p1_time <- anova(lm(myPCA$scores[,1] ~ myT$time))$"Pr(>F)"[1]
p2_time <- anova(lm(myPCA$scores[,1] ~ myT$time))$"Pr(>F)"[1]

#create matrix with 4 columns
tab <- matrix(c(p1_cage, p2_cage, p1_genotype, p2_genotype, p1_time, p2_time), 
              ncol=2, byrow=TRUE)

#define column names and row names of matrix
colnames(tab) <- c('PCA1', 'PCA2')
rownames(tab) <- c('Cage', 'Genotype', 'Time (pre vs. post)')

#convert matrix to table 
tab <- as.table(tab)

tab

### from the table, the time (pre vs. post) is most associated with both
### the first PCA axis and the second PCA axis. from the PCA1, the cage seems 
### have NO effect on these data

post_dataset <- myT[myT$time == 'POST', ]

factors <- factor(post_dataset$cage)

for (i in 5:10){
  boxplot( as.numeric( post_dataset[, i]) ~ factors ) 
}

library(nlme)
pvaluesMixed <- vector()
pvalueGLS <- vector
coefs <- vector()
for (i in 5:10){
  bug <- post_dataset[,i]
  cage <- post_dataset$cage
  genotype <- post_dataset$genotype
  myFrame <- data.frame(bug, cage, genotype)
  mixedModel <- lme(bug ~ genotype, method="REML", random = ~1 | cage, data = myFrame)
  pvaluesMixed[i-4] <- unclass(summary(mixedModel))$tTable[2,5]
  
  GLS <- gls(bug ~ genotype, method="REML", correlation = corCompSymm(form = ~1 | cage), data=myFrame)
  # pvalueGLS[i-4] <- anova(GLS)$"p-value"[2]
  coefs[i-4] <- coef(GLS$modelStruct[1]$corStruct, unconstained=FALSE)[[1]]
}

adjusted_pvalues_mixed <- p.adjust(pvaluesMixed, method="BH")
hist(adjusted_pvalues_mixed, breaks = 10)

### The intraclass correlation coefficient for each phyla is 0.7814876,  0.9317935,
### 0.6834024, -1.6007796,  0.6252533 and 0.1683700.
### From the hist gram of adjusted p values, we can see there are some significantly 
### different for genotype in the mixed model at a 10% false discovery rate
> 
