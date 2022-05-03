rm(list=ls())

library("dplyr")
library("devtools")

setwd("/Users/ericliao/Desktop/phD_courses/2022Spring_class/Advanced_Stat/lab/homework/project")
myTable <- read.table("statsData.csv",header=TRUE,row.names = 1, sep = ",")

columnNameList <- colnames(myTable)

###### Perform ANoVA test on entire dataset to see if there is significant different between cells in different cancer stages
factors <- vector()

for(i in 1:length(columnNameList)){
  print(columnNameList[i])

  if (endsWith(columnNameList[i], "Reduction")){
    factors[i] <- 0
  }
  
  if (endsWith(columnNameList[i], "Basal")){
    factors[i] <- 1
  }
  
  if (endsWith(columnNameList[i], "DCIS")){
    factors[i] <- 2
  }
  
  if (endsWith(columnNameList[i], "LumA")){
    factors[i] <- 3
  }
  
  if (endsWith(columnNameList[i], "LumB")){
    factors[i] <- 4
  }
  
  if (endsWith(columnNameList[i], "HER2")){
    factors[i] <- 5
  }
}

get_anova_pvalues <- function(dataframe) {
  pvalueList <- vector()
  for(i in 1: nrow(dataframe)){
    pvalueList[i] <- anova(lm(factors ~ as.numeric(myTable[i,])))$"Pr(>F)"[1]
  }
  return(pvalueList)
}

pvalues_anova <- get_anova_pvalues(myTable)

hist(pvalues_anova, breaks = 100)

adjusted_pvalues_anova <- p.adjust(pvalueList_anova, method="BH")

hist(adjusted_pvalues_anova, breaks = 100)

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

count <- counts(adjusted_pvalues_anova)

###### perform t test between normal(Reduction) cell and cells in different cancer stages.

counts_list_correction <- vector()

# 1. Reduction(normal) vs. Basal
df_Reduction_Basal <- myTable %>% select(ends_with('Reduction') | ends_with('Basal'))

columnNameList <- colnames(df_Reduction_Basal)

get_ttest_pvalues <- function(dataframe,group_a_name, group_b_name) {
  pvalueList <- vector()
  x <- vector()
  y <- vector()
  for(m in 1: nrow(dataframe)){
    x <- dataframe[m,] %>% select(ends_with(group_a_name))
    y <- dataframe[m,] %>% select(ends_with(group_b_name))
    pvalueList[m] <- t.test(x, y)$p.value
  }
  return(pvalueList)
}

pvalue_basal_vs_reduction <- get_ttest_pvalues(df_Reduction_Basal,"Reduction", "Basal")

hist(pvalue_basal_vs_reduction, breaks = 20)

adjusted_pvalues_basal <- p.adjust(pvalue_basal_vs_reduction, method="BH")

counts_list_correction[1] <- counts(adjusted_pvalues_basal)[1]


# 2. Reduction(normal) vs. DCIS
df_Reduction_DCIS <- myTable %>% select(ends_with('Reduction') | ends_with('DCIS'))

columnNameList <- colnames(df_Reduction_DCIS)

pvalue_DCIS_vs_reduction <- get_ttest_pvalues(df_Reduction_DCIS,"Reduction", "DCIS")

hist(pvalue_DCIS_vs_reduction, breaks = 20)

adjusted_pvalues_DCIS <- p.adjust(pvalue_DCIS_vs_reduction, method="BH")

counts_list_correction[2] <- counts(adjusted_pvalues_DCIS)[1]


#3. Reduction(normal) vs. HER2
df_Reduction_HER2 <- myTable %>% select(ends_with('Reduction') | ends_with('HER2'))

columnNameList <- colnames(df_Reduction_HER2)

pvalue_HER2_vs_reduction <- get_ttest_pvalues(df_Reduction_HER2,"Reduction", "HER2")

hist(pvalue_HER2_vs_reduction, breaks = 20)

adjusted_pvalues_HER2 <- p.adjust(pvalue_HER2_vs_reduction, method="BH")

counts_list_correction[3] <- counts(adjusted_pvalues_HER2)[1]


#4. Reduction(normal) vs. LumA
df_Reduction_LumA <- myTable %>% select(ends_with('Reduction') | ends_with('LumA'))

columnNameList <- colnames(df_Reduction_LumA)

pvalue_LumA_vs_reduction <- get_ttest_pvalues(df_Reduction_LumA,"Reduction", "LumA")

hist(pvalue_LumA_vs_reduction, breaks = 20)

adjusted_pvalues_LumA <- p.adjust(pvalue_LumA_vs_reduction, method="BH")

counts_list_correction[4] <- counts(adjusted_pvalues_LumA)[1]


#5. Reduction(normal) vs. LumB
df_Reduction_LumB <- myTable %>% select(ends_with('Reduction') | ends_with('LumB'))

columnNameList <- colnames(df_Reduction_LumB)

pvalue_LumB_vs_reduction <- get_ttest_pvalues(df_Reduction_LumB,"Reduction", "LumB")

hist(pvalue_LumB_vs_reduction, breaks = 20)

adjusted_pvalue_LumB <- p.adjust(pvalue_LumB_vs_reduction, method="BH")

counts_list_correction[5] <- counts(adjusted_pvalue_LumB)[1]



## perform PCA to see if there is any separation between cells in different stages
myPca <- princomp(myTable, scale. = TRUE)

variance_explain_list <- myPca$sdev^2 / sum(myPca$sdev^2)

x_axis_title = paste("PC1: ", toString(format(round(variance_explain_list[1], 2), nsmall = 2)))

y_axis_title = paste("PC2: ", toString(format(round(variance_explain_list[2], 2), nsmall = 2)))

plot(myPca$scores[,1], myPca$scores[,2], col=1:5, cex=1.25,pch=19,xlab=x_axis_title, ylab=y_axis_title )


groupList <- c("Reduction", "Basal", "DCIS", "LumA", "LumB", "HER2")

legend("topright", legend = paste(groupList), col = 1:5, pch = 19, bty = "n", pt.cex = 1,cex=0.8)

