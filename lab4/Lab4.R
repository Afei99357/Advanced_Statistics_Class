rm(list=ls())

## Question 1
setwd("/Users/ericliao/Desktop/phD_courses/2022Spring_class/Advanced_Stat/lab/homework/lab4")
myTable <- read.table("nc101_scaff_dataCounts.txt",header=TRUE,row.names=1)

## Question 2
plot(log10(myTable$D2_01),log10(myTable$D2_02))

## Question 3
plot(log10(apply(myTable, 1, mean)), log10(apply(myTable, 1, var)))
lines(log10(apply(myTable, 1, var)),log10(apply(myTable, 1, var)), col="red")

## Question 4
contingencyTable <- data.frame(
  "Sequences in D2_01" = c(myTable$D2_01[1], apply(myTable,2,sum)[1]-myTable$D2_01[1]),
  "Sequences in D2_02" = c(myTable$D2_02[1], apply(myTable,2,sum)[2]-myTable$D2_02[1]),
  row.names = c("Assigined to NC101_00003", "Not assigned to NC101_00003"),
  stringsAsFactors = FALSE
)

test <- fisher.test(contingencyTable)

## pvalues is 1.670017e-11
test$p.value

## Question 5
pvaluesVector <- vector()

for (i in 1:nrow(myTable)) {
  print(i)
  contingencyTableGene <- data.frame(
    "Sequences in D2_01" = c(myTable$D2_01[i], apply(myTable,2,sum)[1]-myTable$D2_01[i]),
    "Sequences in D2_02" = c(myTable$D2_02[i], apply(myTable,2,sum)[2]-myTable$D2_02[i]),
    row.names = c("Assigined to current gene", "Not assigned to current gene"),
    stringsAsFactors = FALSE
  )
  
  testNew <- fisher.test(contingencyTableGene)
  pvaluesVector[i] <- testNew$p.val
}

## The p values are not uniformly distributed. I will not expect them to be uniformly
## distributed because the pvalues indicates a great number of the genes are not independent
hist(pvaluesVector, breaks = 50)

myTable <- myTable[(myTable$D2_01 + myTable$D2_02 > 50),]

pvaluesVectorNew <- vector()

for (i in 1:nrow(myTable)) {
  print(i)
  contingencyTableGene <- data.frame(
    "Sequences in D2_01" = c(myTable$D2_01[i], apply(myTable,2,sum)[1]-myTable$D2_01[i]),
    "Sequences in D2_02" = c(myTable$D2_02[i], apply(myTable,2,sum)[2]-myTable$D2_02[i]),
    row.names = c("Assigined to current gene", "Not assigned to current gene"),
    stringsAsFactors = FALSE
  )
  
  testNew <- fisher.test(contingencyTableGene)
  pvaluesVectorNew[i] <- testNew$p.val
}

## The p-value distribution change to more significant
hist(pvaluesVectorNew, breaks = 50)

## Question 6
myTable <- read.table("nc101_scaff_dataCounts.txt",header=TRUE,row.names=1)
myTable
myTable <- myTable +1

expectedFrequency <- myTable$D2_01[1] / apply(myTable,2,sum)[1]

poissonTest <- poisson.test(myTable$D2_02[1],apply(myTable,2,sum)[2],r=expectedFrequency)

#The p value is 1.139341e-13 
poissonTest$p.value

## Question 7
poissonPvalueVector <- vector()
for (i in 1:nrow(myTable)){
  print(i)
  expectedFrequencyNew <- myTable$D2_01[i] / apply(myTable,2,sum)[1]
  
  poissonTestNew <- poisson.test(myTable$D2_02[i],apply(myTable,2,sum)[2],r=expectedFrequencyNew)
  
  poissonPvalueVector[i] <- poissonTestNew$p.value
}

plot(log10(pvaluesVector), log10(poissonPvalueVector))










