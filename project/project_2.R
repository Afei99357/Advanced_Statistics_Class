rm(list=ls())

library("dplyr")
library("devtools")

setwd("/Users/ericliao/Desktop/phD_courses/2022Spring_class/Advanced_Stat/lab/homework/project")
myTable <- read.table("statsData_st000061_st000065.csv",header=TRUE,row.names = 1, sep = ",")

columnNameList <- colnames(myTable)

###### Perform ANoVA test on entire dataset to see if there is significant different between cells in different tissues
factors <- vector()

for(i in 1:length(columnNameList)){
  print(columnNameList[i])
  
  if (endsWith(columnNameList[i], "Human.Serum")){
    factors[i] <- 0
  }
  
  if (endsWith(columnNameList[i], "Visceral.Fat")){
    factors[i] <- 1
  }
  
  if (endsWith(columnNameList[i], "Subcutaenous.Fat")){
    factors[i] <- 2
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

# 1. Serum vs. Visceral Fat
df_serum_visceral <- myTable %>% select(ends_with('Human.Serum') | ends_with('Visceral.Fat'))

columnNameList <- colnames(df_serum_visceral)

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

pvalueListSerumVisceral <- get_ttest_pvalues(df_serum_visceral,"Human.Serum", "Visceral.Fat")

hist(pvalueListSerumVisceral)

adjusted_pvalues_serum_visceral_fat <- p.adjust(pvalueListSerumVisceral, method="BH")

hist(adjusted_pvalues_serum_visceral_fat, xlim=c(0,1))


# 2. Serum vs. Subcutaenous Fat
df_serum_subcutaenous<- myTable %>% select(ends_with('Human.Serum') | ends_with('Subcutaenous.Fat'))

columnNameList <- colnames(df_serum_subcutaenous)

pvalueListSerumSubcutaenous <- get_ttest_pvalues(df_serum_subcutaenous,"Human.Serum", "Subcutaenous.Fat")

hist(pvalueListSerumSubcutaenous)

adjusted_pvalues_serum_subcutaenous <- p.adjust(pvalueListSerumSubcutaenous, method="BH")

hist(adjusted_pvalues_serum_subcutaenous, xlim=c(0,1))


#3. Visceral Fat vs. Subcutaenous Fat
df_visceral_subcutaenous<- myTable %>% select(ends_with('Visceral.Fat') | ends_with('Subcutaenous.Fat'))

columnNameList <- colnames(df_visceral_subcutaenous)

pvalueListVisceralSubcutaenous <- get_ttest_pvalues(df_visceral_subcutaenous,"Visceral.Fat", "Subcutaenous.Fat")

hist(pvalueListVisceralSubcutaenous)

adjusted_pvalues_visceral_subcutaenous <- p.adjust(pvalueListVisceralSubcutaenous, method="BH")

hist(adjusted_pvalues_visceral_subcutaenous, xlim=c(0,1))


## perform PCA to see if there is any separation between cells in diferent stages
myPca <- prcomp(t(myTable), center = TRUE,scale. = TRUE)

tissue_type <- c("Human Serum", "Visceral Fat", "Subcutaenous Fat")

library(ggfortify)
pcaPlot <- autoplot(myPca, data = t(myTable), colour = factors)

print(pcaPlot)


