rm(list=ls())

## Question 1
setwd("/Users/ericliao/Desktop/phD_courses/2022Spring_class/Advanced_Stat/lab/homework/lab5")
myTable <- read.table("cancerRisk.txt",header=TRUE,row.names = 1, sep = "\t")

## plot in log10-log10
plot(log10(myTable$CumulativeCellDivisions), log10(myTable$Lifetime_cancer_risk))

linearModle <- lm(log10(myTable$Lifetime_cancer_risk) ~ log10(myTable$CumulativeCellDivisions))

abline(linearModle, col="red")

Y <- log10(myTable$Lifetime_cancer_risk)
X <- log10(myTable$CumulativeCellDivisions)



r_2 <- cor(Y,X) * cor(Y,X)

summary(linearModle)
## from the summary, we can get # R-square is 0.6463011, #p-value is 5.124e-08

plot(linearModle)
# from the qq plot, we can see the assumptions of constant variance and normal distribution 
# of the residues is reasonable for this model


## Question 2
rm(list=ls())

setwd("/Users/ericliao/Desktop/phD_courses/2022Spring_class/Advanced_Stat/lab/homework/lab5")
BMI_Data <- read.table("BMI_Data.txt",header=TRUE,sep = "\t")
control_Data <- read.table("caseControlData.txt",header=TRUE, sep = "\t")

key <- control_Data$studyId

key <- sub("case", "", key)
key <- sub("control", "", key)

newKey <- vector()

for(i in 1:length(key)){
  newKey[i] <- strsplit(key[[i]], "_")[[1]][1]
}

new_index <- vector()
for (i in control_data$studyId){
  if(grepl(i, "case", fixed = TRUE)){
    control_Data$studyId <- sub("case", "", i)
  }
}
  
control_Data$studyId <- newKey

data_combination <- merge(BMI_Data, control_Data, by="studyId")

pValueList <- vector()

data_combination$studyId <- NULL

for (i in 2:ncol(data_combination)){
  linearModle <- lm(data_combination$bmi ~ data_combination[,i])
  pValue <- anova(linearModle)$"Pr(>F)"[1]
  pValueList[i] <- pValue
}

plot(pValueList)
hist(pValueList)
## from the histgrams, it shows the results are uniform distirbuted, and it means the 
## microbial community appear to NOT be influencing body weight in this cohort

adjusted_pvalues <- p.adjust(pValueList, method="BH")

hist(adjusted_pvalues, xlim=c(0,1))

## fromt the histgram, we can see there is no significant p-value below 10%.


