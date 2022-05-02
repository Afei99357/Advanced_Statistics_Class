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

pvalueList_anova <- get_anova_pvalues(myTable)

hist(pvalueList_anova)

adjusted_pvalues_anova <- p.adjust(pvalueList_anova, method="BH")

hist(adjusted_pvalues_anova, xlim=c(0,1))


###### perform t test between normal(Reduction) cell and cells in different cancer stages.

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

pvalueListBasal <- get_ttest_pvalues(df_Reduction_Basal,"Reduction", "Basal")

hist(pvalueListBasal)

adjusted_pvalues_basal <- p.adjust(pvalueListBasal, method="BH")



# 2. Reduction(normal) vs. DCIS
df_Reduction_DCIS <- myTable %>% select(ends_with('Reduction') | ends_with('DCIS'))

columnNameList <- colnames(df_Reduction_DCIS)

pvalueListDCIS <- get_ttest_pvalues(df_Reduction_DCIS,"Reduction", "DCIS")

hist(pvalueListDCIS)

adjusted_pvalues_DCIS <- p.adjust(pvalueListDCIS, method="BH")


#3. Reduction(normal) vs. HER2
df_Reduction_HER2 <- myTable %>% select(ends_with('Reduction') | ends_with('HER2'))

columnNameList <- colnames(df_Reduction_HER2)

pvalueListHER2 <- get_ttest_pvalues(df_Reduction_HER2,"Reduction", "HER2")

hist(pvalueListHER2)

adjusted_pvalues_HER2 <- p.adjust(pvalueListHER2, method="BH")


#4. Reduction(normal) vs. LumA
df_Reduction_LumA <- myTable %>% select(ends_with('Reduction') | ends_with('LumA'))

columnNameList <- colnames(df_Reduction_LumA)

pvalueListLumA <- get_ttest_pvalues(df_Reduction_LumA,"Reduction", "LumA")

hist(pvalueListLumA)

adjusted_pvalues_LumA <- p.adjust(pvalueListLumA, method="BH")


#5. Reduction(normal) vs. LumB
df_Reduction_LumB <- myTable %>% select(ends_with('Reduction') | ends_with('LumB'))

columnNameList <- colnames(df_Reduction_LumB)

pvalueListLumB <- get_ttest_pvalues(df_Reduction_LumB,"Reduction", "LumB")

hist(pvalueListLumB)

adjusted_pvalue_LumB <- p.adjust(pvalueListLumB, method="BH")



## perform PCA to see if there is any separation between cells in different stages
myPca <- princomp(myTable, center = TRUE,scale. = TRUE)

plot(myPca$scores[,1], myPca$scores[,2], col=1:5, cex=1.25,pch=19 )

groupList <- c("Reduction", "Basal", "DCIS", "LumA", "LumB", "HER2")

legend("topright", legend = paste(groupList), col = 1:5, pch = 19, bty = "n")


