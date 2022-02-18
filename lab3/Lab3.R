########################### Question 1(A)
x <- seq(0,1, 0.01)
plot(x, dexp(x, rate =5) / 0.9932621)


########################### Question 1(B)
rm(list=ls())
piOld <- 0.5
numBreaks <- 1000

numIterations <- 500000
posteiorDist <- vector(length = numIterations)
gridPosteriorDist <- vector()
xVals <- seq(0,1, 0.001)

# grid approximation
i <- 1;
sum <- 0;
for( x in xVals )
{
  gridPosteriorDist[i] <-(dexp(piOld, rate=5) / 0.9932621) * dbinom( 14, 24, x)
  sum = sum + gridPosteriorDist[i];
  i <- i + 1;	
}

gridPosteriorDist <- gridPosteriorDist[1:(length(gridPosteriorDist)-1)]

for( i in 1:numIterations )
{
  pOld <-  (dexp(piOld, rate=5) / 0.9932621) * dbinom( 14, 24, piOld )
  
  piNew <- piOld + rnorm(1, 0, sd =0.01);
  
  if( piNew > 1) 
    piNew = 1;
  
  if( piNew < 0 ) 
    piNew =0;
  
  pNew <- (dexp(piNew, rate=5) / 0.9932621) * dbinom( 14, 24, piNew )
  
  ratio <- pNew / pOld
  
  if( ratio > 1 || ratio >= runif(1) ) 
    piOld = piNew;
  
  posteiorDist[i] = piOld;	
  
  if( i %% 100 == 0 )
  {	
    myHist <- hist(posteiorDist,breaks=seq(0,1,by=1/numBreaks),plot=FALSE)
    plot( myHist$mids, myHist$counts/i, main = paste("iteration", i), ylim = c(0, 0.01)) 
    dbetasum = sum(dbeta(myHist$mids, 40+14, 40+10))
    lines(myHist$mids, dbeta(myHist$mids, 40+14, 40+10)/dbetasum,col="red") 
    lines(myHist$mids, gridPosteriorDist/sum, col='blue') 
    Sys.sleep(.1)
  }
}


##########################  Question 1(C)
rm(list=ls())
piOld <- 0.5
numBreaks <- 1000


numIterations <- 500000
posteiorDist <- vector(length = numIterations)
gridPosteriorDist <- vector()
xVals <- seq(0,1, 0.001)

# grid approximation
i <- 1;
sum <- 0;
for( x in xVals )
{
  gridPosteriorDist[i] <-(dexp(piOld, rate=5) / 0.9932621) * dbinom( 583, 1000, x)
  sum = sum + gridPosteriorDist[i];
  i <- i + 1;	
}

gridPosteriorDist <- gridPosteriorDist[1:(length(gridPosteriorDist)-1)]

for( i in 1:numIterations )
{
  pOld <-  (dexp(piOld, rate=5) / 0.9932621) * dbinom( 583, 1000, piOld )
  
  piNew <- piOld + rnorm(1, 0, sd =0.01);
  
  if( piNew > 1) 
    piNew = 1;
  
  if( piNew < 0 ) 
    piNew =0;
  
  pNew <- (dexp(piNew, rate=5) / 0.9932621) * dbinom( 583, 1000, piNew )
  
  ratio <- pNew / pOld
  
  if( ratio > 1 || ratio >= runif(1) ) 
    piOld = piNew;
  
  posteiorDist[i] = piOld;	
  
  if( i %% 100 == 0 )
  {	
    myHist <- hist(posteiorDist,breaks=seq(0,1,by=1/numBreaks),plot=FALSE)
    plot( myHist$mids, myHist$counts/i, main = paste("iteration", i), ylim = c(0, 0.03)) 
    dbetasum = sum(dbeta(myHist$mids, 40+583, 40+417))
    lines(myHist$mids, dbeta(myHist$mids, 40+583, 40+417)/dbetasum,col="red") 
    lines(myHist$mids, gridPosteriorDist/sum, col='blue') 
    Sys.sleep(.1)
  }
}

