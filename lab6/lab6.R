rm(list=ls())

setwd("/Users/ericliao/Desktop/phD_courses/2022Spring_class/Advanced_Stat/lab/homework/lab6")
dataCounts <- read.table("longitdunalRNASeqData/nc101_scaff_dataCounts.txt",header=TRUE,row.names = 1, sep="\t")

dataCounts <- dataCounts[apply(dataCounts, 1, median) > 5,]

dataCountsNorm <- dataCounts

for(i in 1:ncol(dataCounts)){
  colSum = sum(dataCounts[, i])
  dataCountsNorm[, i] <- dataCountsNorm[, i] / colSum
}


## reduced model with two parameters

reducedFactors <- c(2, 2, 2, 86, 86, 86, 128, 128, 128, 128, 128)
  
get_anova_pvalues <- function(dataframe, factors) {
  pvalueList <- vector()
  for(i in 1: nrow(dataframe)){
    pvalueList[i] <- anova(lm(as.numeric(dataframe[i,]) ~ factors))$"Pr(>F)"[1]
  }
  return(pvalueList)
}

pvalues <- get_anova_pvalues(dataCountsNorm, reducedFactors)

hist(pvalues)

adjusted_pvalues_anova <- p.adjust(pvalues, method="BH")

counts <- function(dataset) {
  newIndex <- vector()
  counts <- 0
  for(i in 1: length(dataset)){
    
    if(is.na(dataset[i])) 
      {print(i)} 
    else 
      {if(dataset[i] < 0.05){
        counts <- counts + 1
        newIndex <- append(newIndex, i)
    }} 
  }
  return(list(counts, newIndex))
 
}

resultsReduced <- counts(adjusted_pvalues_anova)


## from the result, the number of adjusted pvalues which are less than 0.05 is 448


## full model with three parameters
fullModelFactor <- c(2, 2, 2, 86, 86, 86, 128, 128, 128, 128, 128)

fullModelFactor <- factor(fullModelFactor)

pvalues <- get_anova_pvalues(dataCountsNorm, fullModelFactor)

hist(pvalues)

adjusted_pvalues_anova <- p.adjust(pvalues, method="BH")

resultsFull <- counts(adjusted_pvalues_anova)

## When I switch the factors to actual days, the number of adjusted pvalues which are less than 0.05 is 612


## compare full model and reduced model

compare_full_and_reduce_model <- function(dataframe, factorReduce, factorFull) {
  pvalueList <- vector()
  for(i in 1: nrow(dataframe)){
    
    row_data <- as.numeric(dataframe[i,])

    reduced_model <- lm(row_data ~ factorReduce)
    full_model <- lm(row_data ~ factorFull)
    
    reduced <- sum(residuals(reduced_model) ^ 2)
    full <- sum(residuals(full_model) ^ 2)
    
    f_statistic <- ((reduced - full)/(9-8)) / (full / 8)

    # print(f_statistic)
    pvalueList[i] <- pf(f_statistic, 1, 8, lower.tail = FALSE)
  }
  return(pvalueList)
}


pvalues <- compare_full_and_reduce_model(dataCountsNorm, reducedFactors, fullModelFactor)

hist(pvalues)

adjusted_pvalues_anova <- p.adjust(pvalues, method="BH")

ResultsComparedModels <- counts(adjusted_pvalues_anova)

###  When compare to models, the number of adjusted pvalues which are less than 0.05 is 51

### Question D:
pValuesOneWayAnova <- vector()
pValuesRegression <- vector()
pValueModelDiff <- vector()
index <- vector()

for( i in 1:nrow(dataCountsNorm)) 
{
  index[i] <- i
  row_data <- as.numeric(dataCountsNorm[i,])
  
  reduced_model <- lm(row_data ~ reducedFactors)
  full_model <- lm(row_data ~ fullModelFactor)
  
  pValuesRegression[i] <- anova(reduced_model)$"Pr(>F)"[1]
  pValuesOneWayAnova[i] <- anova(full_model)$"Pr(>F)"[1]
  
  reduced <- sum(residuals(reduced_model) ^ 2)
  full <- sum(residuals(full_model) ^ 2)
  
  f_statistic <- ((reduced - full)/(9-8)) / (full / 8)
  
  # print(f_statistic)
  pValueModelDiff[i] <- pf(f_statistic, 1, 8, lower.tail = FALSE)
  
}
myFrame <- data.frame( index, pValuesOneWayAnova,pValuesRegression,pValueModelDiff)

## A plot
cats <- factor( c( rep("day3",3),rep("week12",3),rep("week20",5)  ))
myFrame <- myFrame[ order(myFrame$pValuesOneWayAnova), ] 
boxplot( as.numeric( dataCountsNorm[ myFrame$index[1],]) ~ cats ) 

## B plot
cats <-  c( rep(2,3),rep(86,3),rep(128,5)  )
myFrame <- myFrame[ order(myFrame$pValuesRegression), ] 
boxplot( as.numeric( dataCountsNorm[ myFrame$index[1],]) ~ cats ) 

## C plot
cats <- factor( c( rep("day3",3),rep("week12",3),rep("week20",5)  ))
myFrame <- myFrame[ order(myFrame$pValueModelDiff), ] 
boxplot( as.numeric( dataCountsNorm[ myFrame$index[1],]) ~ cats ) 

### From the box plot, we can see the reduced model seems perfectly represent the
### most siginificant gene where the full model has a bigger diverse of the data


