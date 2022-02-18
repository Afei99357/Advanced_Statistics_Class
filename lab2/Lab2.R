rm(list=ls())

rolls<-c(2,3,2,6,3,5,6,2,6,6,2,6,6,2,3,6,6,6,5,6,6,5,6,6,6,6,6,4,6,3,3,3,6,6,5,6,6)

p_fair <- 0.99

p_loaded <-0.01

p_6_given_loaded <- 0.5

p_not_6_given_loaded <- 0.1

p_6_given_fair <- 1/6

p_not_6_given_fair <- 1/6

posterior_probabilities_loaded <- vector(length=length(rolls), mode="double")

posterior_probabilities_fair <- vector(length=length(rolls), mode="double")

for(i in 1:length(rolls)){
  if (rolls[i] !=6){

    p_x <- p_loaded * p_not_6_given_loaded + p_fair * p_not_6_given_fair
    p_loaded <- (p_not_6_given_loaded * p_loaded) / p_x
    p_fair <- (p_not_6_given_fair * p_fair) / p_x
    
    posterior_probabilities_loaded[i] <- p_loaded
    posterior_probabilities_fair[i] <- p_fair
  }
  if (rolls[i] == 6){

    p_6 <- p_loaded * p_6_given_loaded + p_fair * p_6_given_fair
    p_loaded <- (p_6_given_loaded * p_loaded) / p_6
    p_fair <- (p_6_given_fair * p_fair) / p_6
    
    posterior_probabilities_loaded[i] <- p_loaded
    posterior_probabilities_fair[i] <- p_fair
  }
}

# According to the plot, we need around 24/25 rolls to have the confidence that it will be loaded dice
plot(posterior_probabilities_loaded)

#####################################################
# part 2 questions
rm(list=ls())

getDataFromLikelihood <- function(likelihood, numPoints){
  d <- vector(mode='integer', length=numPoints);
  for(i in 1:numPoints){
    if (runif(1) <= likelihood[1]){
      d[i] <- 1;
    }
    else{
      d[i] <- 2;
    }
  }
  return (d);
}

# question 2(1)
calculateDiseaseTimes <- function(plotFlag){
  probDisease <- c(0.001, 0.999)
  likelihoodGivenDisease <- c(0.91, 0.09)
  likelihoodGivenNotDisease <- c(0.16, 0.84)
  diseaseData <- getDataFromLikelihood(likelihoodGivenDisease, 20)
  nonDiseaseData <- getDataFromLikelihood(likelihoodGivenNotDisease, 20)
  probDiseaseVals <- vector()
  times <- 0
  
  titleStr <- ""
  for (i in 1:length(diseaseData)){
    probDiseaseVals[i] <- probDisease[1];
    
    denom <- probDisease[1] * likelihoodGivenDisease[diseaseData[i]] + probDisease[2] * likelihoodGivenNotDisease[diseaseData[i]];
    
    probDisease[1] = probDisease[1] * likelihoodGivenDisease[diseaseData[i]] / denom;
    probDisease[2] = probDisease[2] * likelihoodGivenNotDisease[diseaseData[i]] / denom;
    
    probDiseaseVals
    titleStr <- paste(titleStr, diseaseData[i],sep="")
    if(plotFlag){
      plot(1:i, probDiseaseVals, main= titleStr, ylim=c(0,1), xlim=c(1,length(diseaseData) + 1))
      Sys.sleep(0.1)
    }
    
    if(probDisease[1] > 0.99999){
      times <- i
      return(times)
      break;
    }
  }
}

diseaseTime <- vector()

for(i in 1:10){
  diseaseTime[i] <- calculateDiseaseTimes(TRUE)
}

print(paste("need average around ", toString(mean(diseaseTime)) , " times to reach hospital's requirment."))


# question 2(2)
getDataFromNonDiseaseLikelihood <- function(likelihood, numPoints){
  d <- vector(mode='integer', length=numPoints);
  for(i in 1:numPoints){
    if (runif(1) <= likelihood[1]){
      d[i] <- 2;
    }
    else{
      d[i] <- 1;
    }
  }
  return (d);
}

calculateNonDiseaseTimes <- function(plotFlag){
  probDisease <- c(0.001, 0.999)
  likelihoodGivenDisease <- c(0.91, 0.09)
  likelihoodGivenNotDisease <- c(0.16, 0.84)
  
  nonDiseaseData <- getDataFromNonDiseaseLikelihood(likelihoodGivenNotDisease, 20)
  probDiseaseVals <- vector()
  times <- 0
  
  titleStr <- ""
  for (i in 1:length(nonDiseaseData)){
    probDiseaseVals[i] <- probDisease[1];
    
    denom <- probDisease[1] * likelihoodGivenDisease[nonDiseaseData[i]] + probDisease[2] * likelihoodGivenNotDisease[nonDiseaseData[i]];
    
    probDisease[1] = probDisease[1] * likelihoodGivenDisease[nonDiseaseData[i]] / denom;
    probDisease[2] = probDisease[2] * likelihoodGivenNotDisease[nonDiseaseData[i]] / denom;
    
    titleStr <- paste(titleStr, nonDiseaseData[i],sep="")
    if(plotFlag){
      plot(1:i, probDiseaseVals, main= titleStr, ylim=c(0,1), xlim=c(1,length(nonDiseaseData) + 1))
      Sys.sleep(0.1)
    }
    
    if(probDisease[1] > 0.99999){
      times <- i
      return(times)
      break;
    }
  }
}

nonDiseaseTime <- vector()

for(i in 1:10){
  nonDiseaseTime[i] <- calculateDiseaseTimes(TRUE)
}

print(paste("need average around ", toString(mean(nonDiseaseTime)) , " times to reach hospital's requirment."))

# 2 (3)
totalTimes = 0
averageDiseaseTimes = mean(diseaseTime)
averageNonDiseaseTimes = mean(nonDiseaseTime)

for(i in 1:1000000){
  if (runif(1) <= 0.001){
    totalTimes <- totalTimes + averageDiseaseTimes
  }
  else{
    totalTimes <- totalTimes + averageNonDiseaseTimes
  }
  if(i%%10000 == 0){
    print(i)
  }
  
}

totalMoney <- totalTimes * 1

#"Total money to run test on 1 million people will cost around:  13701016.9997916"
print(paste("Total money to run test on 1 million people will cost around: ", totalMoney))

