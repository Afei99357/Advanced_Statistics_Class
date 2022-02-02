rm(list=ls())

## Mean
loaded_dice_mean <- 0.1*1 + 0.1*2 + 0.1*3 + 0.1*4 + 0.1*5 + 0.5*6
loaded_dice_mean

## variance
loaded_dice_variance <- 0.1*(1-loaded_dice_mean)^2 + 0.1*(2-loaded_dice_mean)^2 + 0.1*(3-loaded_dice_mean)^2 + 0.1*(4-loaded_dice_mean)^2 + 0.1*(5-loaded_dice_mean)^2 + 0.5*(6-loaded_dice_mean)^2
loaded_dice_variance

## roll loaded dice function
rollLoadedDie <- function(rolling_times){
  rolls_result <- vector(length=rolling_times, mode="double")
  
  # for(i in 1:rolling_times){
  #   rolls_result[i] <- sample(1:6,size = 1, replace = TRUE, prob = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5))
  # }
  
  rolls_result <- sample(1:6,size = rolling_times, replace = TRUE, prob = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5))
  
  return (rolls_result)
}

## The rolls of the loaded die is not approximate a uniform distribution
hist(rollLoadedDie(9999999))

## Slide #58 of lecture #2 (modify)
trailSizes <- c(5, 10, 15, 20, 25, 30, 40, 50, 100, 200, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000, 100000)
means <- vector(mode="double", length=length(trailSizes))
variances <- vector(mode="double", length = length(trailSizes))

for (i in 1:length(trailSizes)){
  rolls <- vector(length = trailSizes[i], mode="double")
  
  for(j in 1:trailSizes[i]){
    rolls[j] <- sample(1:6,size = 1, prob = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5))
  }
  means[i] <- mean(rolls);
  variances[i] <- var(rolls)
}

# 1000 rolls appear to be necessary to get convergence on the expected values for the mean and variance
plot(log10(trailSizes), means)
lines(log10(trailSizes), rep(loaded_dice_mean, length(trailSizes)))

plot(log10(trailSizes), variances)
lines(log10(trailSizes), rep(loaded_dice_variance, length(trailSizes)))

