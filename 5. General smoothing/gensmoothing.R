#loading NOAA.new data in
NOAA <- read.csv("/Users/ellawalmsley/Downloads/NOAAnew.csv") #change address as needed
#creating smoother.pck with objects and functions


#functions in smoother.pck provided by teacher, code has been slightly modified to include press statistics
smoother.pck <-
c("smoother.pck", "bin.mean", "gauss.mean", "gauss.reg", "gauss.mean.trunc",
"gauss.reg.trunc", "my.hat.w")

#creating bin.mean in r
#making a function that takes the argument x (input data), y (input data), nbin(number of bins), and xcol (which column has x values)
bin.mean <- function(x,y,nbin,xcol=2) {
  #ordering x values in ascending order
  o1<-order(x)
  #rearranging values in x in order
  x1<-x[o1]
  #rearranging values in x in order
  y1<-y[o1]
  #storing the range of values of x in r1
  r1<-range(x)
  #calculating the width of each bin and storing it in inc
  inc<-(r1[2]-r1[1])/nbin
  #assigning NULL value to yvec
  yvec<-NULL
  #assigning NULL value to smat
  smat<-NULL
  #creating a for loop that will run for each integer in nbin
  for(i in 1:nbin) {
    #calculating the lower boundary of a bin
    bin.low<-r1[1]+(i-1)*inc
    #calculating the upper boundary of a bin
    bin.high<-r1[1]+i*inc
    #indicates if x1 value is less than or equal to bin.low and then categorizes it as true/false
    I1<-x1>=bin.low
    #if i is less than nbin, indicates if x1 value is less than bin.high and then categorizes it as true/false
    if(i<nbin) {
      #indicates if x1 value is less than bin.high and then categorize it as true/false
      I2<-x1<bin.high
    }
    #else indicates if x1 value is less than or equal too bin.high + 200 and then categorize it as true/false
    else {
      I2<-x1<=(bin.high+200)
    }
    # indicates whether I1 or I2 fall into bins
    I3<-as.logical(I1*I2)
    #finding mean of y values in I3 and storing it in yval
    yval<-mean(y1[I3])
    #summing I3 and storing it in n1
    n1<-sum(I3)
    #assigning matdum a NULL value
    matdum<-NULL
    #creating a for loop that iterates for every integer in n1
    for(i in 1:n1) {
      #adding a row to matdum that is observations about density
      matdum<-rbind(matdum,I3*1/n1)
    }
    #adding matdum to smat matrix
    smat<-rbind(smat,matdum)
    #adding values to yvec
    yvec<-c(yvec,rep(yval,n1))
  }
  #defining n99 at length of x1
  n99<-length(x1)
  #calculating degrees of freedom
  dferror<-length(x1)-sum(diag(2*smat-smat%*%(t(smat))))
  #sum of squared errors
  delta1<-sum(diag(t(diag(n99)-smat)%*%(diag(n99)-smat)))
  #sum of squares in smat
  R<-t(diag(n99)-smat)%*%(diag(n99)-smat)
  #calculating measure of goodness of fit
  delta2<-2*sum(diag(R%*%R))
  #adding a line plot
  lines(x1,yvec,col=xcol)
  #storing y1 in ypred
  ypred<-y1
  #storing predcited values in ypred
  ypred<-smat%*%y1
  #calculating residuals
  resid<-y-ypred
  #calculating press statistics
  PRESS<-sum((resid/(1-diag(smat)))^2)
  #output of press statistics
  list(smat=smat,df=sum(diag(smat)),dferror=dferror,delta1=delta1,delta2=delta2,resid=resid,pred=ypred,press=PRESS)
}

gauss.mean <- function(x,y,lambda,xcol=3,do.plot=T) {
  o1<-order(x)
  x1<-x[o1]
  y1<-y[o1]
  r1<-range(x)
  smat<-NULL
  n1<-length(x1)
  for(i in 1:n1) {
    v1<-dnorm(x1,x1[i],lambda)
    v1<-v1/sum(v1)
    smat<-rbind(smat,v1)
  }
  yhat<-smat%*%y1
  if(do.plot) {
    lines(x1,yhat,col=xcol)
  }
  n99<-length(x1)
  dferror<-length(x1)-sum(diag(2*smat-smat%*%(t(smat))))
  delta1<-sum(diag(t(diag(n99)-smat)%*%(diag(n99)-smat)))
  R<-t(diag(n99)-smat)%*%(diag(n99)-smat)
  delta2<-2*sum(diag(R%*%R))
  resid<-y1-smat%*%y1
  ypred<-y1
  ypred[o1]<-smat%*%y1
  #calculating press statistics
  PRESS<-sum((resid/(1-diag(smat)))^2)
  #output of press statistics
  list(smat=smat,df=sum(diag(smat)),dferror=dferror,delta1=delta1,delta2=delta2,resid=resid,pred=ypred,press=PRESS)
}

gauss.reg <- function(x,y,lambda,xcol=4,do.plot=T) {
  o1<-order(x)
  x1<-x[o1]
  y1<-y[o1]
  r1<-range(x)
  smat<-NULL
  n1<-length(x1)
  for(i in 1:n1) {
    v1<-dnorm(x1,x1[i],lambda)
    v1<-v1/sum(v1)
    H1<-my.hat.w(x1,v1)
    smat<-rbind(smat,H1[i,])
  }
  yhat<-smat%*%y1
  if(do.plot) {
    lines(x1,yhat,col=xcol)
  }
  n99<-length(x1)
  dferror<-length(x1)-sum(diag(2*smat-smat%*%(t(smat))))
  delta1<-sum(diag(t(diag(n99)-smat)%*%(diag(n99)-smat)))
  R<-t(diag(n99)-smat)%*%(diag(n99)-smat)
  delta2<-2*sum(diag(R%*%R))
  resid<-y1-smat%*%y1
  ypred<-y1
  ypred[o1]<-smat%*%y1
  PRESS<-sum((resid/(1-diag(smat)))^2)
  list(smat=smat,df=sum(diag(smat)),dferror=dferror,delta1=delta1,delta2=delta2,resid=resid,pred=ypred,press=PRESS)
}

gauss.mean.trunc <- function(x,y,lambda,nnn,xcol=5,do.plot=T) {
  o1<-order(x)
  x1<-x[o1]
  y1<-y[o1]
  r1<-range(x)
  smat<-NULL
  n1<-length(x1)
  trunc.val<-n1-nnn
  for(i in 1:n1) {
    v1<-dnorm(x1,x1[i],lambda)
    o2<-order(v1)
    thresh<-v1[o2[trunc.val]]
    v1<-v1*(v1>thresh)
    v1<-v1/sum(v1)
    smat<-rbind(smat,v1)
  }
  yhat<-smat%*%y1
  if(do.plot) {
    lines(x1,yhat,col=xcol)
  }
  n99<-length(x1)
  dferror<-length(x1)-sum(diag(2*smat-smat%*%(t(smat))))
  delta1<-sum(diag(t(diag(n99)-smat)%*%(diag(n99)-smat)))
  R<-t(diag(n99)-smat)%*%(diag(n99)-smat)
  delta2<-2*sum(diag(R%*%R))
  resid<-y1-smat%*%y1
  ypred<-y1
  ypred[o1]<-smat%*%y1
  PRESS<-sum((resid/(1-diag(smat)))^2)
  list(smat=smat,df=sum(diag(smat)),dferror=dferror,delta1=delta1,delta2=delta2,resid=resid,pred=ypred,press=PRESS)
}

gauss.reg.trunc <- function(x,y,lambda,nnn,xcol=6,do.plot=T) {
  o1<-order(x)
  x1<-x[o1]
  y1<-y[o1]
  r1<-range(x)
  smat<-NULL
  n1<-length(x1)
  trunc.val<-n1-nnn
  for(i in 1:n1) {
    v1<-dnorm(x1,x1[i],lambda)
    o1<-order(v1)
    thresh<-v1[o1[trunc.val]]
    v1<-v1*(v1>thresh)
    v1<-v1/sum(v1)
    H1<-my.hat.w(x1,v1)
    smat<-rbind(smat,H1[i,])
  }
  yhat<-smat%*%y1
  if(do.plot) {
    lines(x1,yhat,col=xcol)
  }
  n99<-length(x1)
  dferror<-length(x1)-sum(diag(2*smat-smat%*%(t(smat))))
  delta1<-sum(diag(t(diag(n99)-smat)%*%(diag(n99)-smat)))
  R<-t(diag(n99)-smat)%*%(diag(n99)-smat)
  delta2<-2*sum(diag(R%*%R))
  resid<-y1-smat%*%y1
  ypred<-y1
  ypred[o1]<-smat%*%y1
  PRESS<-sum((resid/(1-diag(smat)))^2)
  list(smat=smat,df=sum(diag(smat)),dferror=dferror,delta1=delta1,delta2=delta2,resid=resid,pred=ypred,press=PRESS)
}
  
my.hat.w <- function(x,wt) {
  x1<-cbind(1,x)
  x1%*%solve(t(x1)%*%diag(wt)%*%x1)%*%t(x1)%*%(diag(wt))
}




#FOLLOWING CODE IS ORIGINAL (not provided by teacher)

#function to test press stats against each other
#only works with one-dimensional
greedy.random.search = function(func, data, xcol, ycol, theta) {
  press0 = func(data[,xcol], data[,ycol], theta)$press
  press00 = press0
  inc = 0
  theta0 = theta
  while(inc < 100) {
    epsilon = rnorm(1, 0, .01)
    theta1 = (theta + epsilon)
    press1 = func(data[,xcol], data[,ycol], theta1)$press
    if (press1<press0) {
      press0 = press1
      inc = 0
      theta = theta1
    }
    else {
      inc = (inc + 1)
    }
  }
  list(theta = theta, theta0 = theta0, press = press0, press0 = press00)
}

#two-dimensional greedy for gauss.mean.trunc/gauss.reg.trunc, as they include other variables like nnn and lambda
greedy.random.search.2d = function(func, data, xcol, ycol, theta, nnn) {
  press0 = func(data[,xcol], data[,ycol], theta, nnn)$press
  press00 = press0
  inc = 0
  theta0 = theta
  nnn0 = nnn
  while(inc<100) {
    epsilon = rnorm(1, 0, .01)
    epsilon1= sample(c(-1,0,1),1)
    theta1 = (theta + epsilon)
    nnn1= (nnn+epsilon1)
    press1 = func(data[,xcol], data[,ycol], theta1, nnn1)$press
    if(press1<press0) {
      press0 = press1
      inc = 0
      theta = theta1
      nnn=nnn1
    }
    else {
      inc = (inc + 1)
    }
  }
  list(theta = theta, theta0 = theta0, nnn = nnn, nnn0 = nnn0, press = press0, press0 = press00)
}

#discrete greedy for bin.mean
greedy.random.search.discrete = function(func, data, xcol, ycol, theta) {
  press0 = func(data[,xcol], data[,ycol], theta)$press
  press00 = press0
  inc = 0
  theta0 = theta
  while(inc<100) {
    epsilon = sample(c(-1,0,1),1)
    theta1 = (theta + epsilon)
    press1 = func(data[,xcol], data[,ycol], theta1)$press
    if(press1<press0) {
      press0 = press1
      inc = 0
      theta = theta1
    }
    else {
      inc = (inc + 1)
    }
  }
  list(theta = theta, theta0 = theta0, press = press0, press0 = press00)
}

#NOAA.new scatter plot (rate of billion dollar weather disasters vs. temperature rise)
plot(NOAA[,3],NOAA[,2],xlab="temperature rise", ylab = "rate of billion dollar weather disasters")
#calculating means using smoother.pck functions with assigned parameter values
dum <-bin.mean(NOAA[,3],NOAA[,2],6)
dum <-gauss.mean(NOAA[,3],NOAA[,2],.063)
dum <-gauss.reg(NOAA[,3],NOAA[,2],.078,do.plot=T)
dum <-gauss.mean.trunc(NOAA[,3],NOAA[,2],.063,20,do.plot=T)
dum <- gauss.reg.trunc(NOAA[,3],NOAA[,2],.08,17,do.plot=T)
#making smooth lines
lines(lowess(NOAA[,3],NOAA[,2]),col=7)
lines(smooth.spline(NOAA[,3],NOAA[,2]),col=8)

#finding improved parameter values using greedy random search
greedy.random.search(gauss.mean, NOAA, 3, 2, .063)
greedy.random.search(gauss.reg, NOAA, 3, 2, .078)
greedy.random.search.2d(gauss.mean.trunc, NOAA, 3, 2, .063, 20)
greedy.random.search.2d(gauss.reg.trunc, NOAA, 3, 2, .08, 17)
greedy.random.search.discrete(bin.mean, NOAA, 3, 2, 6)
#further testing greedy.random.search.discrete
greedy.random.search.discrete(bin.mean, NOAA, 3, 2, 5)
