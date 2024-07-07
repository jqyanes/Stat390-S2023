#special.sample from xy.pck (provided by teacher)
special.sample = c(23.840599479756122, 4.3842774727366027, 5.0956688866002846, 2.3566543172090144, -23.949803213259941, 9.3509427290409803, 0.65251206234097481, 6.8365429651532175, 24.896640079698329, -8.7617700151623517, 25.814657481989627, 5.3152770334854722, 2.8398049478656984, -12.096616427505259, 4.397545337677002, 5.4355953823630578, 18.144806428903586, 1.9786876970902085, 16.530886256647232, -4.9056228227437755, 6.2044959254562855, -8.9589436515690117, 13.812075705778721, 9.6438114456832409, 1.0270954776654413, 29.053179855129486, 8.0951351039111614, 8.7905355421826243, 9.3145597372204065, 3.873737707734108, 8.6849553808569908, 4.1409478062881835, -16.871777369731905, -3.3649728987365961, 16.548116152686003, 1.3435630658641458, -6.5022478735991474, -8.9998905617268115, 22.518533006100256, -23.222754490905523, 2.9830907224918586, -14.888938206339123, -12.837568901053807, 1.953628201991169, 3.4240511415893384, 3.6827530963346362, -11.468025827420057, 12.27449047523451, 13.405985074580519, 29.737901833066353, 7.1429840913042426, -16.755755010152104, -13.138900882569185, -1.2381440894678235, 6.7195787066593766, 6.6611230992719683, 1.0132466223852241, -9.3566439929873972, 1.7830456765368581, -4.3367369263888618, 6.4618188533931971, 11.953485022483543, 5.2667434718459845, 8.0009208358824253, 1.5598284753420515, 0.86587769400678294, -3.3896422414109111, -13.331382337117436, 11.487637599416272, 0.89456674007777126, 12.273026674251872, 5.3552341060712934, 12.734275228855134, 0.35275325831025839, -0.62435123417526484, 5.1114548249170184, -5.647560053005936, -5.7916095364363205, 4.1071316817436179, 10.312835827049735, -4.8025107267768403, -14.866977098489048, 13.547340474602702, 12.075118191120149, 2.6460608680859949, -2.4452916979789734, -25.219439385641106, -0.80708191078156233, 1.6723475670441985, -1.0564956841990352, 8.1480371737852693, 8.2783658867701888, 1.8028011561703869, -23.964070327659641, 39.920698090685924, -11.726356807255987, 6.3345346059650183, 2.3614248326048255, 14.125069800344232, 4.207889268836146)

#accessing tidyverse package from the library
library(tidyverse)

#original bootstrap program from assignment 3
#function that calculates bootstrap and normal confidence intervals for a vector
my.bootstrapci = function(vec0, nboot = 10000, alpha = 0.05) {
  #storing vector's stats
  n0 = length(vec0)
  mean0 = mean(vec0)
  sd0 = sqrt(var(vec0))
  bootvec = NULL #initializing bootvec
  #creates a sample group out of vec0, of length vec0, nboot amount of times
  for( i in 1:nboot) {
    vecb = sample(vec0,replace=T) #sample group created
    #ensuring that sample/sample's sd is acceptable
    while((sqrt(var(vecb)))==0) {
      vecb = sample(vec0, replace = T)
    }
    #calculating and putting sample group's standardized difference in means into bootvec
    meanb = mean(vecb)
    sdb = sqrt(var(vecb))
    bootvec = c(bootvec,(meanb-mean0)/(sdb/sqrt(n0)))
  }
  lq = quantile(bootvec,alpha/2) #lower quartile
  uq = quantile(bootvec,1-alpha/2) #upper quartile
  LB = mean0-(sd0/sqrt(n0))*uq #lower bound of confidence interval
  UB = mean0-(sd0/sqrt(n0))*lq #upper bound
  NLB = mean0-(sd0/sqrt(n0))*qt(1-alpha/2,n0-1) #nonparametric lower bound
  NUB = mean0+(sd0/sqrt(n0))*qt(1-alpha/2,n0-1) #nonparametric upper bound
  #listing bootstrap and normal confidence intervals
  list(bootstrap.confidence.interval=c(LB,UB),normal.confidence.interval=c(NLB,NUB))
}



#function to estimate variance (used in my.bootstrapci.redo)
var.est = function(statfunc,vec0,nboot=100) {
  #storing vec0’s stats
  n0 = length(vec0)
  stat0 = statfunc(vec0)
  #initializing bootvec
  bootvec = NULL
  #creating a sample of vec0 and storing it in vecb
  for( i in 1:nboot) {
    vecb = sample(vec0,replace=T)
    #finding the statfunc of each entry in vecb
    statb = statfunc(vecb)
    #storing each statfunc in bootvec
    bootvec = c(bootvec,statb)
  }
  #listing vec0’s statfunc, mean of bootvec, and variance of bootvec
  list(stat0=stat0,bootmean=mean(bootvec),bootvar=var(bootvec))
}

#rewritten bootstrap function
#creating a function called my.bootstrapci.redo with a function that uses input from a statistical function (statfunc), a dataset (vec0), 10000 n boot samples (nboot = 10000), and the significance level for the confidence interval (alpha = 0.05)
my.bootstrapci.redo = function(statfunc,vec0,nboot=10000,alpha=0.05) {
  #creating n0 by storing the length of the dataset vec0 in n0
  n0 = length(vec0)
  #creating statfunc0 by storing the statfunc (mean, median, etc.) of the dataset vec0 in statfunc0
  statfunc0 = statfunc(vec0)
  #storing the standard error of vec0 in se0
  se0 = sqrt(var.est(statfunc,vec0,nboot=100)$bootvar)
  #setting bootvec as NULL, initializing it the be an empty vector
  bootvec = NULL
  #creating a loop that will run nboot (10,000) times for every 'i' value, which will be 1 to 10,000
  for( i in 1:nboot) {
    #creating vecb and making it equal to a sample from vec0. The sampling is being done with replacement (replace = T)
    vecb = sample(vec0,replace=T)
    #storing standard error of vecb in seb
    seb = sqrt(var.est(statfunc,vecb,nboot=100)$bootvar)
    #fixing for low n
    while(seb == 0 ) {
      vecb = sample(vec0,replace=T)
    }
    #creating statfuncb and making it equal to the statfunc of vecb
    statfuncb = statfunc(vecb)
    #updating the value of bootvec to the standardized difference of the statfunc values
    bootvec = c(bootvec,(statfuncb-statfunc0)/(seb/sqrt(n0)))
  }
  #creating lq (lower quantile) by storing the 5th percentile of values of bootvec found using the quantile function
  lq = quantile(bootvec,alpha/2)
  #creating lq (upper quantile) by storing the 95th percentile of values of bootvec found using the quantile function
  uq = quantile(bootvec,1-alpha/2)
  #creating LB (lower bound for confidence interval).
  LB = statfunc0-(se0/sqrt(n0))*uq
  #creating UB (lower bound for confidence interval).
  UB = statfunc0-(se0/sqrt(n0))*lq
  #creating NLB (nonparametric lower bound for confidence interval). Calculated using normal approximation.
  NLB = statfunc0-(se0/sqrt(n0))*qt(1-alpha/2,n0-1)
  #creating NUB (nonparametric upper bound for confidence interval). Calculated using normal approximation.
  NUB = statfunc0+(se0/sqrt(n0))*qt(1-alpha/2,n0-1)
  #creating a list of bootstrapped confidence intervals and confidence intervals calculated using normal approximation.
  list(bootstrap.confidence.interval=c(LB,UB),normal.confidence.interval=c(NLB,NUB))
}



#applying bootstrap functions (original and redo) to special.sample
my.bootstrapci(special.sample)
  # $bootstrap.confidence.interval
  #   97.5%        2.5%
  # 0.5456602    5.2211335
  # $normal.confidence.interval
  # [1] 0.5553075  5.2093548
my.bootstrapci.redo(median, special.sample)
  # $bootstrap.confidence.interval
  #  97.5%        2.5%
  # 1.942656    5.651595
  # $normal.confidence.interval
  # [1] 3.391492  3.715393
