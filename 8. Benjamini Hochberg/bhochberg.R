source("C:/Users/yanes/Downloads/R/renal.pck") #edit pathway as needed

#code taken from fungraphs.pck (provided by teacher)
fdr <- function(v1,Q, ind=F) {
  #v1 (vector of p-values) is ordered
  o1<-order(v1)
  #sorted v1 is stored in pvec
  pvec<-v1[o1]
  #number of tests (length of v1) is stored in m
  m<-length(v1)
  #qline is set according to independence
  qline<-Q*c(1:m)/m #if independent
  #if not independent, Q*c(1:m)/(m*(sum(1/c(1:m)))
  if(!ind) {
    c1<-sum(1/(c(1:m)))
    qline<-Q*c(1:m)/(m*c1)
  }
  #creates plot of points
  plot(c(c(1:m),c(1:m)),c(qline,pvec),type="n",xlab="ordering",ylab="pvalue")
  lines(c(1:m))
  lines(qline, col = "red") #qline is drawn in red
  points(c(1:m),pvec)
  #calculates Pstar (pmax) and identifies all P<= pmax
  dv<-pvec-qline
  I1<-(dv<0) #I1 is made up of all p-values below the line (less than qline)
  I0<-I1 #I0 set equal to I1
  if(sum(I0)>.5) { #tested at the .05 level
    pmax<-max(pvec[I1]) #pmax = the largest p-value below (less than) the qline
    #interesting p-values (p-values less than pmax) are stored in I2
    I2<-pvec<=pmax
    #points with interesting p-values are plotted in cyan
    points(c(1:m)[I2],pvec[I2],col="cyan")
    #lists interesting tests
    out<-list(interesting=o1[I2], ind=ind)
  }
  else { #if no interesting p-values
    #calculates posterior distribution
    vec<-qbeta(c(.5,.95,.99,.999),1,length(v1)+1)
    #lists .5 percentile, .95 percentile, and .999 percentile
    out<-list(q.5=vec[1],q.95=vec[2],q.99=vec[3],q.999=vec[4])
  }
  out
}

#testing code with vectors from renal.pck (provided by teacher)
fdr(psmall.renal, .1, T)
fdr(psmall.renal, .1, F)
fdr(plarge.renal, .1, T)
fdr(plarge.renal, .1, F)
