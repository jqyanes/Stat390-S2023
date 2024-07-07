#accessing tidyverse package from the library 
library(tidyverse)

#creating a function called my.bootstrapci with a function that uses input from dataset (vec0), 10000 n boot samples (nboot = 10000), and the significance level for the confidence interval (alpha = 0.1)  
my.bootstrapci <- function(vec0,nboot=10000,alpha=0.1) {
  #creating n0 by storing the length of the dataset vec0 in n0
  n0<-length(vec0)
  #creating mean0 by storing the mean of the dataset vec0 in mean0
  mean0<-mean(vec0)
  #creating sd0 storing the standard deviation of the dataset vec0 in sd0 by taking the squareroot of the variance of vec0
  sd0<-sqrt(var(vec0))
  #setting bootvec as NULL, initializing it the be an empty vector
  bootvec<-NULL
  #creating a loop that will run nboot (10,000) times for every 'i' value, which will be 1 to 10,000
  for( i in 1:nboot) {
    #creating vecb and making it equal to a sample from vec0. The sampling is being done with replacement (replace = T)  
    vecb<-sample(vec0,replace=T)
    #fixing for low n 
    while(sqrt(var(vec0)) == 0) {
      vecb<-sample(vec0,replace=T)
    }
    #to do without loop, extra cred:
      #replicate(nboot, sample(vec0, replace = TRUE))    
    #creating meanb and making it equal to the mean of vecb 
    meanb<-mean(vecb)
    #creating sdb by storing the standard deviation of vecb by taking the squareroot of the variance of vecb
    sdb<-sqrt(var(vecb))
    #updating the value of bootvec to the standardized difference of means
    bootvec<-c(bootvec,(meanb-mean0)/(sdb/sqrt(n0)))
  }
  #creating lq (lower quantile) by storing the 5th percentile of values of bootvec found using the quantile function
  lq<-quantile(bootvec,alpha/2)
  #creating lq (upper quantile) by storing the 95th percentile of values of bootvec found using the quantile function
  uq<-quantile(bootvec,1-alpha/2)
  #creating LB (lower bound for confidence interval). 
  LB<-mean0-(sd0/sqrt(n0))*uq
  #creating UB (lower bound for confidence interval). 
  UB<-mean0-(sd0/sqrt(n0))*lq
  #creating NLB (nonparametric lower bound for confidence interval). Calculated using normal aproximation. 
  NLB<-mean0-(sd0/sqrt(n0))*qt(1-alpha/2,n0-1)
  #creating NUB (nonparametric upper bound for confidence interval). Calculated using normal aproximation. 
  NUB<-mean0+(sd0/sqrt(n0))*qt(1-alpha/2,n0-1)
  #creating a list of bootstrapped confidence intervals and confience intervals calculated using normal approximation. 
  list(bootstrap.confidence.interval=c(LB,UB),normal.confidence.interval=c(NLB,NUB))
}

## $bootstrap.confidence.interval
##      95%       5% 
## 3.699942 7.282363 
## 
## $normal.confidence.interval
## [1] 3.744928 7.255072

#here, we are creating a function that takes mean value, sample size, number of simulations, and alpha as inputs. The default values are 3, 30, 1000, and 0.1, respectively
simfunc = function(mu.val=3,n=30,nsim=1000,a=0.1) {
  #setting cvec.boot as NULL, initializing it to be an empty vector
  cvec.boot<-NULL
  #setting cvec.norm as NULL, initializing it to be an empty vector
  cvec.norm<-NULL
  #%%%%%%%%%
  #calculating and storing population mean
  mulnorm<-(exp(mu.val+1/2))
  #$$$$$$$$$$$$$$
  #For loop: for every value (i) in n:sim, run the for loop 1 to nsim times
  for(i in 1:nsim) {
    #this states: if i is divisble by 10, return the the largest integer that is smaller than or equal to i/10 and print i
    #indicates how many simulation runs have passed (in intervals of 10)
    if((i/10)==floor(i/10)) {
      print(i)
    }
    #creating vec.sample and storing random numbers generated in a log-normal distribution with n amount of numbers and mean of mu. val
    vec.sample<-rlnorm(n,mu.val)    
    #creating boot.list and storing a bootstrapped confidence interval and normal confidence  interval made from vec.sample in it
    boot.list<-my.bootstrapci(vec.sample, alpha=a)
    #creating boot.conf and storing a bootstrapped confidence interval from boot.list dataframe in it
    boot.conf<-boot.list$bootstrap.confidence.interval
    #creating norm.conf and storing a bootstrapped normsl confidence interval from boot.list dataframe in it
    norm.conf<-boot.list$normal.confidence.interval    
    #calculates and stores the coverage of bootstrap confidence interval for the sample group
    cvec.boot<-c(cvec.boot,(boot.conf[1]<mulnorm)*(boot.conf[2]>mulnorm))    
    #calculates and stores the coverage of normal theory confidence interval for the sample group
    cvec.norm<-c(cvec.norm,(norm.conf[1]<mulnorm)*(norm.conf[2]>mulnorm))
  }  
  #lists the empirical coverage of the two confidence intervals
  list(boot.coverage=(sum(cvec.boot)/nsim),norm.coverage=(sum(cvec.norm)/nsim))
}

#x1 = simfunc(n = 3)
#x2 = simfunc(n = 10)
#x3 = simfunc(n = 30)
#x4 = simfunc(n = 100)
#x5 = simfunc(n = 3, a = 0.05)
#x6 = simfunc(n = 10, a = 0.05)
#x7 = simfunc(n = 30, a = 0.05)
#x8 = simfunc(n = 100, a = 0.05)
#results = data.frame (
#  n = c(3,10,30,100, 3, 10, 30, 100),
#  a = c(0.1,0.1,0.1,0.1,0.05,0.05,0.05,0.05),
#  boot = c(x1$boot.coverage, x2$boot.coverage, x3$boot.coverage, x4$boot.coverage, x5$boot.coverage, x6$boot.coverage, x7$boot.coverage, x8$boot.coverage),
#  norm = c(x1$norm.coverage, x2$norm.coverage, x3$norm.coverage, x4$norm.coverage, x5$norm.coverage, x6$norm.coverage, x7$norm.coverage, x8$norm.coverage)
#)
#write.csv(results, file = "Results.csv", row.names = F)
#print(results)

#The bootstrapped and normal both approach .9 as the sample size grows, bootstrap approaches faster intially
#They both go slower as they get closer to .9
#example table from above 
#n       a      boot    norm
#3      0.1     .765    .769
#10     0.1     .847    .785
#30     0.1     .882    .851
#100    0.1     .890    .886
