# Original code taken from fungraphs.pck (provided by teacher)

install.packages("tourr")
install.packages("ISLR")
install.packages("scatterplot3d")
library(tourr)
library(ISLR)
library(scatterplot3d)
source("~/Downloads/fungraphs.pck") #change address as needed
fungraphs.pck

peru.pck <- c("peru.pck", "movie.peru", "Peru", "my.terrain", "v0", "Peru1")
#creating movie.peru
#this function dictates that the depth of the visualization to be created is 350, the subset of depth nw is to be 200, and the dis.vec dictates the range of values
movie.peru <- function(depth=350,depth.nw=200, dis.vec=c(8:13)) {
  #this code creates a scatterplot that uses the second, third, and fourth columns with only axis and grid lines (no points)
  sctstr<-scatterplot3d(Peru[,c(2,3,4)],type="n")
  #this code creates IDmat and defines it as the 24th thru 27th columns in the data frame
  IDmat<-Peru[,24:27]
  #this code updates IDmat so that the second column of IDmat will be multiplied by two
  IDmat[,2]<-IDmat[,2]*2
  #this code updates IDmat so that the third column of IDmat will be multiplied by three
  IDmat[,3]<-IDmat[,3]*3
  #this code updates IDmat so that the fourth column of IDmat will be multiplied by four
  IDmat[,4]<-IDmat[,4]*4
  #this defines ztime in peru as the values in the first column in the peru matrix
  ztime<-Peru[,1]
  #This code creates a new variable called nlag and assigns it a value of 2. It will control the number of lags
  nlag=20
  #loop through time with window, id points pi
  nz<-length(ztime)-nlag
  #loops through for every integer from 1 to nz
  for(i in 1:nz) {
    #creates v9 variable that contains each integer from i to i+nlag
    v9<-c(i:(i+nlag))
    #generates
    icolvec<-apply(IDmat[v9,],1,sum)
    #this loop sets delay in movie
    time.count<-floor(1000*(ztime[i+nlag]-ztime[i+(nlag-1)]))
    for(j in 1:(2*time.count)) {
      v1<-rnorm(100)
      for(k in 1:100) {
        v2<-sample(v1,replace=T)
        mean(v2)
      }
    }
    #Perumat<-cbind(Peru[,c(1,2,3)],sqrt(Peru[,4]))
    sctstr<-scatterplot3d(Peru[,c(2,3,4)],type="n")
    #points are given by Peru[v9, c(2,3,4)], color of points given by icolvec, points are shown as vertical lines to x-y plane, line width is given by ((exp(Peru[v9,5]-4))))
    sctstr$points3d(Peru[v9,c(2,3,4)],col=icolvec,type="h",lwd=((exp(Peru[v9,5]-4))))
    #print(c(i,nz,ztime[i],ztime[nz]))
  }
  print("DONE")
}

movie.peru()

#this is the pairs function in r. It allows us to visualize the way multiple variables in a dataset interact with each other
pairs(mystery9)

#the butterfly effect is the idea that small action can have a big impact.
#In statistics, that looks like a "small change in one state of a nonlinear system resulting in large differences in a later state"
#when we ran pairs(mystery9), we got two graphs that looked like butterflies, which is the shape of the Lorenz attractor
