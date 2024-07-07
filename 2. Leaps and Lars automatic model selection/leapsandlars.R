
#LARS code, assignment 4: Ella, Shreya, Janine, Hiran

#installing the lars and ISLR packages as both are needed to run the lars function, ISLR needed for the dataset, leaps needed for leaps function
install.packages("ISLR")
install.packages("lars")
install.packages("leaps")

library(ISLR)
library(lars)
library(leaps)

#takes dataset Auto from ISLR (contains a list of vehicles and some details about them) and puts them into a matrix
#omits the ninth column (the names of the vehicles) as it contains strings, which will mess with leaps and lars
Auto.mat = as.matrix(Auto[,-9])

#separating Auto.mat vehicles into new matrices according to vehicles' country of origin, indicated by value in origin column
Auto.mat.japan = Auto.mat[Auto.mat[,8]==3,]   #Auto.mat data from japan (3 as origin)
Auto.mat.germany = Auto.mat[Auto.mat[,8]==2,]   #Auto.mat data from germany (2& as origin)
Auto.mat.usa = Auto.mat[Auto.mat[,8]==1,]   #Auto.mat data from usa (1 as origin)

#creating a scatterplot matrix 
pairs(Auto.mat)

#creating a matrix called mpg that stores the values from the first column of Auto.mat (the miles per gallon for each vehicle)
mpg = Auto.mat[,1]
#storing mpg column from each country matrix 
mpg.japan = Auto.mat.japan[,1]
mpg.germany = Auto.mat.germany[,1]
mpg.usa = Auto.mat.usa[,1]
#removing mpg column from each matrix
Auto.mat = Auto.mat[,-1]
Auto.mat.japan = Auto.mat.japan[,-1]
Auto.mat.germany = Auto.mat.germany[,-1]
Auto.mat.usa = Auto.mat.usa[,-1]
#removing origin from each country matrix
Auto.mat.japan = Auto.mat.japan[,-7]
Auto.mat.germany = Auto.mat.germany[,-7]
Auto.mat.usa = Auto.mat.usa[,-7]

matrix.2ndorder.make = function(x, only.quad = F) {
  x0 = x
  dimn = dimnames(x)[[2]] #extract the names of the variables
  num.col = length(x[1,]) # how many columns
  for(i in 1:num.col){
    # if we are doing all 2nd order
    if(!only.quad){
      for(j in i:num.col){
        x0 = cbind(x0,x[,i]*x[,j])
        dimn = c(dimn,paste(dimn[i],dimn[j],sep=""))
        #create interaction dimnames
      }
    }
    else{
      #in here only if doing only squared terms
      x0 = cbind(x0,x[,i]*x[,i])
      dimn = c(dimn,paste(dimn[i],"2",sep="")) # squared dimmension names
    }
  }
  dimnames(x0)[[2]] = dimn
  x0
}

#making 2nd order matrices
Auto.mat2nd = matrix.2ndorder.make(Auto.mat)
Auto.mat.usa2nd = matrix.2ndorder.make(Auto.mat.usa)
Auto.mat.germany2nd = matrix.2ndorder.make(Auto.mat.germany)
Auto.mat.japan2nd = matrix.2ndorder.make(Auto.mat.japan)

#used in lars.select, calculates the sum of the absolute values of its input
sumabs = function(v1){sum(abs(v1))}

#lars automatic model selector using both Cp and cross validation MSE
#creates a function that makes a matrix containing predictor values for the regression model, y, setting plot.it to false (no intercept term), and ncheck to 10. ncheck=iterations for checks
lars.select<-function(xmat,y,int=F,ncheck=10) {
  lasso.str<-lars(xmat,y,intercept=int)
  #LARS performs a LARS on xmat, y, and int and stores it in the lasso.str vector
  print(xmat)
  #this prints the matrix stored in the xmat variable
  cv.str<-cv.lars(xmat,y,plot.it=F,intercept=int)
  #cv.lars preforms a cross validation for LARS on the variables xmat and y. The output is stored in cv.str There will be no plot generated (plot.it= F) and the intercept printed will be equal to int
  o1<-order(cv.str$cv)
  #this sorts the cross-validation values from cv.str in ascending order
  mindex<-cv.str$index[o1][1]
  #this selects the first value from the index column in cv.str and stores it in mindex
  beta<-coef(lasso.str)
  #extracts coeffcients from lasso.str linear model and stores them in beta
  index0<-apply(beta,1,sumabs)
  #This takes the absolute sum of the regression coefficients for each variable in the lasso regression model in beta and saves the output to index0 object.
  index0<-index0/max(index0)
  #This divides the value of index0 over the max value of index0 and stores it in index0 
  o1<-order(abs(index0-mindex))
  #this takes the absolute value of index0-mindex and orders them ascendingly and stores it in o1
  I1<-(abs(index0-mindex)==min(abs(index0-mindex)))
  #compares the absolute values of index0-mindex to the minimum absolute value of the difference of index0 and mindex and stores whether they are equal or not in I1
  n1<-length(beta[,1])
  #assigns the length of the first index of beta to n1, which is equal to the number of rows in beta
  beta.out<-beta[I1,]
  #selects the I1 index from the beta matrix and stores it in beta out
  if(sum(abs(beta.out))==0){
    #this tests the condition: if the absolute value of sums of beta.out is equalto zero
    v1<-lasso.str$Cp
    #this selects the Cp in lasso.str and stores it in v1
    o2<-order(v1)
    #this sorts the values in v1 in ascending order
    beta.out<-beta[o1[1:ncheck],]
    #this selects the first ncheck values of o1 from the beta matrix and stores them in beta.out
  }
  Ind.out<-beta.out!=0
  #this stores the values of beta.out that don't equal zero in Ind.out
  outlist<-list(beta.out=beta.out,ind.out=Ind.out)
  #this makes a list of beta.out and ind.out values and stores them in outlist
  if(int){
    #this tests if there is an integer. The code below will run if there is
    Int.out1<-mean(y)-mean(Auto.mat%*%beta.out[i])
    #this computes the intercepts and stores them in int.out1
    outlist<-list(beta.out=beta.out,ind.out=Ind.out,int.out=Int.out1)
    #this makes a list of beta.out, ind.out, and int.out values and stores them in outlist, updating outlist 
  }       
  outlist
  #this prints the outlist 
}

#used in leaps.then.press
regpluspress <- function(x,y) {
  str <- lsfit(x,y)
  press <- sum((str$resid/(1-hat(x)))^2)
  str$press <- press
  str
}


#this is creating a leaps and bounds function under the data frame "leaps.then.press". The xmat is a matrix of predictor variables, the yvec is a vector of repsonse variable values, the ncheck = 10 means ten is the maximum number of variables to include in the model, print.ls is a value that says whether or not to print the output  
leaps.then.press<-function(xmat,yvec,ncheck=10,print.ls=F) { 
  #leaps is doing a subset selection for multiple linear regression. leaps.str will be a list that has the r-squared value, the Mallows' Cp value, and the number of values
  leaps.str<-leaps(xmat,yvec)
  #this selects the Mallow's Cp value from leaps.str and stores it in z1
  z1<-leaps.str$Cp
  #this orders all the values of z1 in ascending order and stores it in the 01 vector
  o1<-order(z1)
  #this selects the first ncheck (10) rows of the o1 vector and puts them in a matrix
  matwhich<-(leaps.str$which[o1,])[1:ncheck,]
  #This selects the first ten values of z1 and stores them in z2
  z2<-z1[o1][1:ncheck]
  #this makes a for loop that checks values of i for every value in ncheck 
  for(i in 1:ncheck){
    #regplupress runs a regression analysis of the ith row of the matwhich matrix of xmat and yvec and assigns it to ls.str0
    ls.str0<-regpluspress(xmat[,matwhich[i,]],yvec)
    #If (print.ls) is true, then ls.str0 will priny
    if(print.ls){
      ls.print(ls.str0)
    }
    #Prints i
    print(i)
    #Prints "press= value of press in ls.str0. The paste fucntion concatenates the string and number to make one cohesive line
    print(paste("Press=",ls.str0$press))
    #Stores ith index of matwhich in parvec
    parvec<-matwhich[i,]
    #sums values in parvec and stores them in npar
    npar<-sum(parvec)
    #Calculates the  mean prediction sum of squares error and prints MPSE value
    print(paste("MPSE=",ls.str0$press/(length(yvec)-(npar+1))))
    # prints the ith index of z2 and Cp= 
    print(paste("Cp=",z2[i]))
  }
}

#combines the two functions leaps.then.press and lars.select
#if leaps = F, then function runs lars
leapsorlars = function(xmat, y, leaps = F, int = F, ncheck = 10, print.ls = F) {
  if (leaps == T) {
  #limits xmat to 31 columns since leaps does not allow more than 31 variables
    if (ncol(xmat) > 31) {
      leaps.then.press(xmat[,1:31], y) 
    }
    else{
      leaps.then.press(xmat, y, ncheck, print.ls)
    }
  }
  else{
    lars.select(xmat, y, int, ncheck)
  }
}

leapsorlars(Auto.mat2nd, mpg) #runs lars for Auto.mat2nd 
leapsorlars(Auto.mat.japan2nd, mpg.japan) #runs lars for Auto.mat.japan2nd 
leapsorlars(Auto.mat.germany2nd, mpg.germany) #runs lars for Auto.mat.germany2nd 
leapsorlars(Auto.mat.usa2nd, mpg.usa) #runs lars for Auto.mat.usa2nd 
leapsorlars(Auto.mat2nd, mpg, T) #running leaps for Auto.mat2nd
leapsorlars(Auto.mat.japan2nd, mpg.japan, T) #running leaps for Auto.mat.japan2nd 
leapsorlars(Auto.mat.germany2nd, mpg.germany, T) #running leaps for Auto.mat.germany2nd 
leapsorlars(Auto.mat.usa2nd, mpg.usa) #running leaps for Auto.mat.usa2nd 

#Observation made: Beta horsepoweryear for Japan is 0, but for U.S and Germany, they are both around -3e-2 to -5e-5
#However, America is different when it comes to actual horsepower, because it was much lower than the other countries 
#on leaps then press, America has much higher CP values than the germany and japan 

