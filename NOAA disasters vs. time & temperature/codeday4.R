# Code NOT created by group, only comments
# Code taken from codeday4.pck (provided by teacher)
# Ella Walmsley (~ lines 0-200), Janine Yanes(~ lines 200-400), Shreya Ghosh (~ lines 400-600)

NOAA <- read.csv("NOAAnew.csv") #loading the dataset (make sure directory is correct)
head(NOAA) #heading/checking the dataset loaded correctly
#creating the codeday4.pck and loading bootstrap programs and guided user interfaces into it
codeday4.pck <-
c("codeday4.pck", "my.bootstrap1", "gui.bootstrap1", "my.bootstrap2",
"gui.bootstrap2", "my.bootstrap3", "gui.bootstrapxy", "my.dat.plot5",
"my.dat.plot5a")





#defining the my.bootstrap1 by loading a function into it
#creating a function that matrixes NOAA data, the ith column (second column of data) and jth column (third column of data), the name for the x axis, the name for the y axis, the name for the title, the color, square root = false, and the number of bootstraps being defined as 1,000
my.bootstrap1 <- function(mat=NOAA,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,nboot=10000) {
  #splitting into one column, two lines
  par(mfrow=c(1,2))
  #defining stat.out0 as function of my.datplot5
  stat.out0<-my.dat.plot5(mat,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=T,in.boot=F)
  #defining vector F0 as the F column in stat.out0
  F0<-stat.out0$F
  #defining vector resdi0 as residual value in stat.out0
  resid0<-stat.out0$resid.lin
  #defining bootvec vector (vector for bootstrap) as a null value
  bootvec<-NULL
  #defining vector y0 as prediction of values of stat,out0 as the smspline on the first column with respect to y
  y0<-predict(stat.out0$smstrlin,mat[,i])$y
  #defining vector matb with mat for loop for each value of i in the number of bootstrap iterations
  matb<-mat
  #the code will run nboot times, or 10000 times because that's what we set it to above
  for(i1 in 1:nboot) {
    #checking if floor(i1/500) is equal to (i1/500). If it is, we will print i1
    if(floor(i1/500)==(i1/500)){print(i1)}
    #creating vector residb by sampling resid0
    residb<-sample(resid0,replace=T)
    #creating vector Yb by adding y0 to residb
    Yb<-y0+residb
    #Yb is now saved as the j column in the matrix
    matb[,j]<-Yb
    #creating stat.outb from my.dat.plot5 function
    stat.outb<-my.dat.plot5(matb,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=F,in.boot=T)
    #creating bootvec vector by combining
    bootvec<-c(bootvec,stat.outb$F)
  }
  #finding the pval from the bootstrap and saving it to vector pvalboot
  pvalboot<-sum(bootvec>F0)/nboot
  #plotting the boxplot of the pvalue
  boxplot(bootvec)
  #defining stat,out0$pvalboot as pvalboot, adding new column
  stat.out0$pvalboot<-pvalboot
  #checking stat.out0
  stat.out0
}





# creating first graphic user interface with the bootstrap
# making a function that will run in the gui
gui.bootstrap1 <- function() {
  #accessing the tcltk package from library (GIU package)
  library(tcltk)
  #creating "inputs" from the function of the inputs that one can put into the GUI
  inputs <- function() {
    #creating x variable with NOAA string
    x <- tclVar("NOAA")
    #creating y variable with 1 string
    y <- tclVar("1")
    #creating z variable with 2 string
    z <- tclVar("2")
    #creating w variable with delta temp string
    w<-tclVar("\"delta temp\"")
    #creating wa variable with disasters string
    wa<-tclVar("\"disasters\"")
    #creating wb variable (main title) with "\"Disasters vs warming\"" string
    wb<-tclVar("\"Disasters vs warming\"")
    #color of visual
    zc<-tclVar("2")
    #creating qc title with F string
    qc<-tclVar("F")
    #creating rc variable with 10000 string
    rc<-tclVar("10000")
    #this is coding for the actual interface, ie prompts to enter data, spaces to enter data
    tt <- tktoplevel()
    #introducing prompt
    tkwm.title(tt,"Choose parameters for new function ")
    #creates an entry function that is associated with variable x (defined above)
    x.entry <- tkentry(tt, textvariable=x)
    #creates an entry function that is associated with variable y (defined above)
    y.entry <- tkentry(tt, textvariable=y)
    #creates an entry function that is associated with variable z (defined above)
    z.entry <- tkentry(tt, textvariable=z)
    #creates an entry function that is associated with variable w (defined above)
    w.entry<-tkentry(tt, textvariable=w)
    #creates an entry function that is associated with variable wa (defined above)
    wa.entry<-tkentry(tt,textvariable=wa)
    #creates an entry function that is associated with variable wb (defined above)
    wb.entry<-tkentry(tt,textvariable=wb)
    #creates an entry function that is associated with variable zc (defined above)
    zc.entry<-tkentry(tt,textvariable=zc)
    #creates an entry function that is associated with variable qc (defined above)
    qc.entry<-tkentry(tt,textvariable=qc)
    #creates an entry function that is associated with variable rc (defined above)
    rc.entry<-tkentry(tt,textvariable=rc)
    #making a reset function. If we reset, all entries functions with associated variables will be replaced with blank spaces
    reset <- function() {
      tclvalue(x)<-""
      tclvalue(y)<-""
      tclvalue(z)<-""
      tclvalue(w)<-""
      tclvalue(wa.entry)<-""
      tclvalue(wb.entry)<-""
      tclvalue(zc.entry)<-""
      tclvalue(qc.entry)<-""
      tclvalue(rc.entry)<-""
    }
    #this is where we can clear all the data we just entered in the gui by pressing the 'reset' button
    reset.but <- tkbutton(tt, text="Reset", command=reset)
    #by pressing submit, the following function will run
    submit <- function() {
      #saving values to variable x, updating it
      x <- tclvalue(x)
      y <- tclvalue(y)
      z <- tclvalue(z)
      w<-tclvalue(w)
      wa<-tclvalue(wa)
      wb<-tclvalue(wb)
      zc<-tclvalue(zc)
      qc<-tclvalue(qc)
      rc<-tclvalue(rc)
      #provides parent window and puts defined variables into parent window
      e <- parent.env(environment())
      e$x <- x
      e$y <- y
      e$z <- z
      e$w<-w
      e$wa<-wa
      e$wb<-wb
      e$zc<-zc
      e$qc<-qc
      e$rc<-rc
      tkdestroy(tt)
    }
    #the next lines six lines are what I refer to when I say "see above"
    #this gives us the specific text prompts/labeling for the gui and makes them run
    submit.but <- tkbutton(tt, text="start", command=submit)
    #text layout will span two columns in parent window
    tkgrid(tklabel(tt,text="Input data matrix"),columnspan=2)
    #this also gives us text prompts/labeling but it also gives us padding in pixels (so 10 pixels vertical space between the box and the parent window, 30 pixels of vertical space)
    tkgrid(tklabel(tt,text="data"), x.entry, pady=10, padx =30)
    #see above
    tkgrid(tklabel(tt,text="Input X column"),columnspan=2)
    tkgrid(tklabel(tt,text="i"), y.entry, pady=10, padx =30)
    #see above
    tkgrid(tklabel(tt,text="Input Y column"),columnspan=2)
    tkgrid(tklabel(tt,text="j"), z.entry, pady=10, padx =30)
    #see above
    tkgrid(tklabel(tt,text="Input xaxis label"),columnspan=2)
    tkgrid(tklabel(tt,text="Xaxis"), w.entry, pady=10, padx =30)
    #see above
    tkgrid(tklabel(tt,text="Input yaxis label"),columnspan=2)
    tkgrid(tklabel(tt,text="Yaxis"), wa.entry, pady=10, padx =30)
    #see above
    tkgrid(tklabel(tt,text="Input plot title"),columnspan=2)
    tkgrid(tklabel(tt,text="Plot title"), wb.entry, pady=10, padx =30)
    #see above
    tkgrid(tklabel(tt,text="Input color code"),columnspan=2)
    tkgrid(tklabel(tt,text="Color code"),zc.entry, pady=10, padx =30)
    #see above
    tkgrid(tklabel(tt,text="Input sqrt indicator"),columnspan=2)
    tkgrid(tklabel(tt,text="Sqrt=T"),qc.entry, pady=10, padx =30)
    #see above
    tkgrid(tklabel(tt,text="Input nboot"),columnspan=2)
    tkgrid(tklabel(tt,text="nboot"),rc.entry, pady=10, padx =30)
    #this will put the buttons in the parent window
    tkgrid(submit.but, reset.but)
    #this will give us the whole parent window and all her baby variables that we can write in
    tkwait.window(tt)
    return(c(x,y,z,w,wa,wb,zc,qc,rc))
  }
  #Now the function is being run and will give us the first bootstrap values by taking the inputs that we input into the gui
  predictor_para <- inputs()
  #printing out predictor_para
  print(predictor_para)
  #making a matrix from parsed predictor, evaluating it, and assigning it to mat/ind1/etc depending on the location of the called predictor_para value
  mat<-eval(parse(text=predictor_para[1]))
  ind1<-eval(parse(text=predictor_para[2]))
  ind2<-eval(parse(text=predictor_para[3]))
  xlab<-eval(parse(text=predictor_para[4]))
  ylab<-eval(parse(text=predictor_para[5]))
  maintitle<-eval(parse(text=predictor_para[6]))
  zcol<-eval(parse(text=predictor_para[7]))
  zsqrt<-eval(parse(text=predictor_para[8]))
  znboot<-eval(parse(text=predictor_para[9]))
  #CALLING bootstrap function
  my.bootstrap1(mat,ind1,ind2,xlab,ylab, maintitle,zcol,zsqrt,znboot)
}





#creating a function that takes the NOAA data and makes a matrix, the third and second columns, the name for the x axis, the name for the y axis, the name for the title, the color, square root = false, the number of bootstraps being defined as 10,000, computing the model prediction intervals, the confidence level = 95, and testing for the pivotal
my.bootstrap2 <- function(mat=NOAA,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,nboot=10000,pred.bound=T,conf.lev=.95,pivotal=T) {
  #function(mat=NOAA,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,do.plot=T,in.boot=T)
  #stat.out<-list(smstrmod=smstr,smstrlin=smstr.lin,resid.mod=resid1,resid.lin=resid2,dfmod=dfmod,dflin=dflin,F=Fstat,P=pvalue,n=n1)
  #splitting into one column, one lines
  par(mfrow=c(1,1))
  #very similar to my.bootstrap1
  stat.out0<-my.dat.plot5(mat,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=T,in.boot=F)
  resid0<-stat.out0$resid.mod
  bootmat<-NULL
  y0<-predict(stat.out0$smstrmod,mat[,i])$y
  matb<-mat
  for(i1 in 1:nboot) {
    if(floor(i1/500)==(i1/500)){print(i1)}
    residb<-sample(resid0,replace=T)
    Yb<-y0+residb
    matb[,j]<-Yb
    #print(matb)
    stat.outb<-my.dat.plot5(matb,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=F,in.boot=T)
    Ybp<-predict(stat.outb$smstrmod,matb[,i])$y
    if(pred.bound) {
      if(pivotal) {
        bootmat<-rbind(bootmat,stat.outb$resid.mod+Ybp-y0)
      }
      else {
        bootmat<-rbind(bootmat,stat.outb$resid.mod+Ybp)
      }
    }
    else {
      if(pivotal) {
        bootmat<-rbind(bootmat,Ybp-y0)
      }
      else {
        bootmat<-rbind(bootmat,Ybp)
      }
    }
  }
  #splitting into quantiles using calculated confidence levels
  alpha<-(1-conf.lev)/2
  my.quant<-function(x,a=alpha){quantile(x,c(a,1-a))}
  bounds<-apply(bootmat,2,my.quant)
  #if pivotal is true, execute
  if(pivotal) {
    #defining bounds updating the array
    bounds[1,]<-y0-bounds[1,]
    bounds[2,]<-y0-bounds[2,]
  }
  #saving ith column of mat to x
  x<-mat[,i]
  #if do.sqrt is true, execute
  if(do.sqrt) {
    x<-sqrt(x)
  }
  #plotting lines with bounds array and o1 (which are ordered x values) and making the color of the lines zcol +2
  o1<-order(x)
  lines(x[o1],bounds[1,o1],col=zcol+2)
  lines(x[o1],bounds[2,o1],col=zcol+2)
}





#building a GUI for my.bootstrap2
gui.bootstrap2 <- function() {
  library(tcltk) #loads tcltk package
  #,pred.bound=T,conf.lev=.95,pivotal=T
  inputs <- function() { #makes preset values for the inputs of my.bootstrap2
    x <- tclVar("NOAA") #mat = NOAA
    y <- tclVar("1") #i (X column) = 1
    z <- tclVar("2") #j (y column) = 2
    w<-tclVar("\"delta temp\"") #zxlab (xaxis label) = "delta temp"
    wa<-tclVar("\"disasters\"") #zylab (yaxis label) = "disasters"
    wb<-tclVar("\"Disasters vs warming\"") #zmain (plot title) = "Disasters vs warming"
    zc<-tclVar("2") #zcol (color code) = 2
    qc<-tclVar("F") #do.sqrt = F
    rc<-tclVar("10000") #nboot = 10000
    za<-tclVar("T") #pred.bound = T
    zb<-tclVar(".95") #conf.lev = .95
    wc<-tclVar("T") #pivotal = T
    tt <- tktoplevel() #creates a "top level" frame that entities/widgets can go in
    tkwm.title(tt,"Choose parameters for new function ") #makes the title that goes at the top of the pop-up window
    #makes boxes to type each input into, with presets already typed in
    x.entry <- tkentry(tt, textvariable=x) #Ex: creates a box where you type in the matrix you're using; NOAA is already typed in
    y.entry <- tkentry(tt, textvariable=y)
    z.entry <- tkentry(tt, textvariable=z)
    w.entry<-tkentry(tt, textvariable=w)
    wa.entry<-tkentry(tt,textvariable=wa)
    wb.entry<-tkentry(tt,textvariable=wb)
    zc.entry<-tkentry(tt,textvariable=zc)
    qc.entry<-tkentry(tt,textvariable=qc)
    rc.entry<-tkentry(tt,textvariable=rc)
    za.entry<-tkentry(tt,textvariable=za)
    zb.entry<-tkentry(tt,textvariable=zb)
    wc.entry<-tkentry(tt,textvariable=wc)
    reset <- function() { #clears all inputs
      tclvalue(x)<-""
      tclvalue(y)<-""
      tclvalue(z)<-""
      tclvalue(w)<-""
      tclvalue(wa)<-""
      tclvalue(wb)<-""
      tclvalue(zc)<-""
      tclvalue(qc)<-""
      tclvalue(rc)<-""
      tclvalue(za)<-""
      tclvalue(zb)<-""
      tclvalue(wc)<-""
    }
    reset.but <- tkbutton(tt, text="Reset", command=reset) #creates button for reset function
    submit <- function() { #enters all inputs
      x <- tclvalue(x)
      y <- tclvalue(y)
      z <- tclvalue(z)
      w<-tclvalue(w)
      wa<-tclvalue(wa)
      wb<-tclvalue(wb)
      zc<-tclvalue(zc)
      qc<-tclvalue(qc)
      rc<-tclvalue(rc)
      za<-tclvalue(za)
      zb<-tclvalue(zb)
      wc<-tclvalue(wc)
      e <- parent.env(environment()) #sets enclosing environment
      #puts inputs into environment
      e$x <- x
      e$y <- y
      e$z <- z
      e$w<-w
      e$wa<-wa
      e$wb<-wb
      e$zc<-zc
      e$qc<-qc
      e$rc<-rc
      e$za<-za
      e$zb<-zb
      e$wc<-wc
      tkdestroy(tt) #destroys/closes the pop-up window
    }
    submit.but <- tkbutton(tt, text="start", command=submit) #creates button for submit function
    #creates subtitles/instructions for each input box and places them
    tkgrid(tklabel(tt,text="Input data matrix"),columnspan=2)
    tkgrid(tklabel(tt,text="data"), x.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Input X column"),columnspan=2)
    tkgrid(tklabel(tt,text="i"), y.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Input Y column"),columnspan=2)
    tkgrid(tklabel(tt,text="j"), z.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Input xaxis label"),columnspan=2)
    tkgrid(tklabel(tt,text="Xaxis"), w.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Input yaxis label"),columnspan=2)
    tkgrid(tklabel(tt,text="Yaxis"), wa.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Input plot title"),columnspan=2)
    tkgrid(tklabel(tt,text="Plot title"), wb.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Input color code"),columnspan=2)
    tkgrid(tklabel(tt,text="Color code"),zc.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Input sqrt indicator"),columnspan=2)
    tkgrid(tklabel(tt,text="Sqrt=T"),qc.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Input nboot"),columnspan=2)
    tkgrid(tklabel(tt,text="nboot"),rc.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Prediction Bound?"),columnspan=2)
    tkgrid(tklabel(tt,text="Pbound=T"),za.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Conf.level"),columnspan=2)
    tkgrid(tklabel(tt,text="Confidence"),zb.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Pivotal=T"),columnspan=2)
    tkgrid(tklabel(tt,text="Pivotal"),wc.entry, pady=10, padx =30)
    #places the submit and reset buttons
    tkgrid(submit.but, reset.but)
    tkwait.window(tt)
    return(c(x,y,z,w,wa,wb,zc,qc,rc,za,zb,wc)) #puts input values in a vector
  }
  #Now run the function like:
  predictor_para <- inputs()
  print(predictor_para) #prints the input vector
  #separates each input in the vector into a variable
  mat<-eval(parse(text=predictor_para[1]))
  ind1<-eval(parse(text=predictor_para[2]))
  ind2<-eval(parse(text=predictor_para[3]))
  xlab<-eval(parse(text=predictor_para[4]))
  ylab<-eval(parse(text=predictor_para[5]))
  maintitle<-eval(parse(text=predictor_para[6]))
  zcol<-eval(parse(text=predictor_para[7]))
  zsqrt<-eval(parse(text=predictor_para[8]))
  znboot<-eval(parse(text=predictor_para[9]))
  zpred<-eval(parse(text=predictor_para[10]))
  zconf<-eval(parse(text=predictor_para[11]))
  zpivot<-eval(parse(text=predictor_para[12]))
  #runs my.bootstrap2 with the inputs that were typed into the GUI
  my.bootstrap2(mat,ind1,ind2,xlab,ylab, maintitle,zcol,zsqrt,znboot,zpred,zconf,zpivot)
}





#calculates and draws XY bootstrap confidence intervals
my.bootstrap3 <- function(mat=NOAA,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,nboot=10000,pred.bound=T,conf.lev=.95,pivotal=T) {
  par(mfrow=c(1,1)) #splits data into one row, one column
  stat.out0<-my.dat.plot5(mat,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=T,in.boot=F)
  bootmat<-NULL
  y0<-predict(stat.out0$smstrmod,mat[,i])$y
  matb<-mat
  nm<-length(matb[,1])
  #samples nboot amount of times, defines dataframe matb, calculates y as smooth plus resampled residuals and saves smooth interpolated to all X
  for(i1 in 1:nboot) {
    if(floor(i1/500)==(i1/500)){print(i1)} #prints amount of samples so far in intervals of 500
    zed<-sample(nm,replace=T)
    matb<-mat[zed,]
    #print(matb)
    stat.outb<-my.dat.plot5a(matb,mat,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=F,in.boot=T)
    Ybp<-predict(stat.outb$smstrmod,mat[,i])$y
    if(pred.bound) {
      if(pivotal) {
        bootmat<-rbind(bootmat,stat.outb$resid.mod+Ybp-y0)
      }
      else {
        bootmat<-rbind(bootmat,stat.outb$resid.mod+Ybp)
      }
    }
    else {
      if(pivotal) {
        bootmat<-rbind(bootmat,Ybp-y0)
      }
      else {
        bootmat<-rbind(bootmat,Ybp)
      }
    }
  }
  #finds/sets bounds
  alpha<-(1-conf.lev)/2
  my.quant<-function(x,a=alpha){quantile(x,c(a,1-a))}
  bounds<-apply(bootmat,2,my.quant)
  #flips bounds if pivotal
  if(pivotal) {
    bounds[1,]<-y0-bounds[1,]
    bounds[2,]<-y0-bounds[2,]
  }
  #takes the square root of mat[,i] (a column of the matrix that was input) if do.sqrt is true
  x<-mat[,i]
  if(do.sqrt) {
    x<-sqrt(x)
  }
  #draws XY bootstrap confidence intervals
  o1<-order(x)
  lines(x[o1],bounds[1,o1],col=zcol+2)
  lines(x[o1],bounds[2,o1],col=zcol+2)
}





#creates GUI for my.bootstrap3
gui.bootstrapxy <- function() {
  library(tcltk) #loads tcltk package
  #,pred.bound=T,conf.lev=.95,pivotal=T
  inputs <- function() { #makes preset values for the inputs of my.bootstrap3
    x <- tclVar("NOAA")
    y <- tclVar("1")
    z <- tclVar("2")
    w <- tclVar("\"delta temp\"")
    wa <- tclVar("\"disasters\"")
    wb <- tclVar("\"Disasters vs warming\"")
    zc <- tclVar("2")
    qc <- tclVar("F")
    rc <- tclVar("10000")
    za <- tclVar("T")
    zb<-tclVar(".95")
    wc<-tclVar("T")
    #creating the names for the variables
    tt <- tktoplevel()
    #creating the widgets to display the variables
    tkwm.title(tt,"Choose parameters for new function ")
    #creating the title
    x.entry <- tkentry(tt, textvariable=x)
    y.entry <- tkentry(tt, textvariable=y)
    z.entry <- tkentry(tt, textvariable=z)
    w.entry<-tkentry(tt, textvariable=w)
    wa.entry<-tkentry(tt,textvariable=wa)
    wb.entry<-tkentry(tt,textvariable=wb)
    zc.entry<-tkentry(tt,textvariable=zc)
    qc.entry<-tkentry(tt,textvariable=qc)
    rc.entry<-tkentry(tt,textvariable=rc)
    za.entry<-tkentry(tt,textvariable=za)
    zb.entry<-tkentry(tt,textvariable=zb)
    wc.entry<-tkentry(tt,textvariable=wc)
    #for all the tkentrys, inputing single line text strings into the variables
    #function for clearing inputs (tclvalues)
    reset <- function() {
      tclvalue(x)<-""
      tclvalue(y)<-""
      tclvalue(z)<-""
      tclvalue(w)<-""
      tclvalue(wa.entry)<-""
      tclvalue(wb.entry)<-""
      tclvalue(zc.entry)<-""
      tclvalue(qc.entry)<-""
      tclvalue(rc.entry)<-""
      tclvalue(za.entry)<-""
      tclvalue(zb.entry)<-""
      tclvalue(wc.entry)<-""
    }
    #the reset function resets all the tclvalues back to null and erases what was there before, if reset button was pressed
    reset.but <- tkbutton(tt, text="Reset", command=reset)
    #function for submitting inputs (tclvalues)
    submit <- function() {
      x <- tclvalue(x)
      y <- tclvalue(y)
      z <- tclvalue(z)
      w<-tclvalue(w)
      wa<-tclvalue(wa)
      wb<-tclvalue(wb)
      zc<-tclvalue(zc)
      qc<-tclvalue(qc)
      rc<-tclvalue(rc)
      za<-tclvalue(za)
      zb<-tclvalue(zb)
      wc<-tclvalue(wc)
      e <- parent.env(environment())
      e$x <- x
      e$y <- y
      e$z <- z
      e$w<-w
      e$wa<-wa
      e$wb<-wb
      e$zc<-zc
      e$qc<-qc
      e$rc<-rc
      e$za<-za
      e$zb<-zb
      e$wc<-wc
      tkdestroy(tt)
    }
    #the entire above bracket is the start function
    submit.but <- tkbutton(tt, text="start", command=submit)
    #the submit button that comes after starting
    tkgrid(tklabel(tt,text="Input data matrix"),columnspan=2)
    tkgrid(tklabel(tt,text="data"), x.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Input X column"),columnspan=2)
    tkgrid(tklabel(tt,text="i"), y.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Input Y column"),columnspan=2)
    tkgrid(tklabel(tt,text="j"), z.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Input xaxis label"),columnspan=2)
    tkgrid(tklabel(tt,text="Xaxis"), w.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Input yaxis label"),columnspan=2)
    tkgrid(tklabel(tt,text="Yaxis"), wa.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Input plot title"),columnspan=2)
    tkgrid(tklabel(tt,text="Plot title"), wb.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Input color code"),columnspan=2)
    tkgrid(tklabel(tt,text="Color code"),zc.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Input sqrt indicator"),columnspan=2)
    tkgrid(tklabel(tt,text="Sqrt=T"),qc.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Input nboot"),columnspan=2)
    tkgrid(tklabel(tt,text="nboot"),rc.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Prediction Bound?"),columnspan=2)
    tkgrid(tklabel(tt,text="Pbound=T"),za.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Conf.level"),columnspan=2)
    tkgrid(tklabel(tt,text="Confidence"),zb.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Pivotal=T"),columnspan=2)
    tkgrid(tklabel(tt,text="Pivotal"),wc.entry, pady=10, padx =30)
    #all the tkgrids organize the placement of the values with their corresponding variables and widgets.
    tkgrid(submit.but, reset.but)
    #the submit and reset button for the tkgrid
    tkwait.window(tt)
    return(c(x,y,z,w,wa,wb,zc,qc,rc,za,zb,wc))
  }
  #Now run the function like:
  predictor_para <- inputs()
  print(predictor_para)
  mat<-eval(parse(text=predictor_para[1]))
  ind1<-eval(parse(text=predictor_para[2]))
  ind2<-eval(parse(text=predictor_para[3]))
  xlab<-eval(parse(text=predictor_para[4]))
  ylab<-eval(parse(text=predictor_para[5]))
  maintitle<-eval(parse(text=predictor_para[6]))
  zcol<-eval(parse(text=predictor_para[7]))
  zsqrt<-eval(parse(text=predictor_para[8]))
  znboot<-eval(parse(text=predictor_para[9]))
  zpred<-eval(parse(text=predictor_para[10]))
  zconf<-eval(parse(text=predictor_para[11]))
  zpivot<-eval(parse(text=predictor_para[12]))
  #Parse all the input data because it comes in as a string and we want to split it up to evaluate it otherwise
  my.bootstrap3(mat,ind1,ind2,xlab,ylab, maintitle,zcol,zsqrt,znboot,zpred,zconf,zpivot)
  #set up the bootstrap three with all its properties (x axis label, y axis label, title, etc) which was found from parsing the data above
}





my.dat.plot5 <-
function(mat=NOAA,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,do.plot=T,in.boot=T){
if(in.boot){
do.sqrt<-F
}
#if in.boot is true, do not take the square root
if(!do.sqrt){
smstr<-smooth.spline(mat[,i],mat[,j])
smstr.lin<-smooth.spline(mat[,i],mat[,j],df=2)
#if you don’t take the square root, make the line using smooth spline and two degrees of
freedom
if(do.plot){
plot(mat[,i],mat[,j],xlab=zxlab,ylab=zylab,main=zmain)
lines(smstr,col=zcol)
lines(smstr.lin,col=(zcol+1))
} #plot the smooth spline line
resid1<-mat[,j]-predict(smstr,mat[,i])$y
resid2<-mat[,j]-predict(smstr.lin,mat[,i])$y
}else{
smstr<-smooth.spline(mat[,i],sqrt(mat[,j]))
smstr.lin<-smooth.spline(mat[,i],sqrt(mat[,j]),df=2)
#if you are taking the square root, create the line with smooth spline and two degrees of
freedom, but with the square root of mat[,j] instead of regular mat[,j]
if(do.plot){
plot(mat[,i],sqrt(mat[,j]),xlab=zxlab,ylab=zylab,main=zmain)
lines(smstr,col=zcol)
lines(smstr.lin,col=(zcol+1))
} #plot the smooth splined line
resid1<-sqrt(mat[,j])-predict(smstr,mat[,i])$y
resid2<-sqrt(mat[,j])-predict(smstr.lin,mat[,i])$y
}
dfmod<-smstr$df
dflin<-2
ssmod<-sum(resid1^2)
sslin<-sum(resid2^2)
numss<-sslin-ssmod
n1<-length(mat[,j])
Fstat<-(numss/(dfmod-dflin))/(ssmod/(n1-dfmod))
pvalue<-1-pf(Fstat,dfmod-dflin,n1-dfmod)
stat.out<-list(smstrmod=smstr,smstrlin=smstr.lin,resid.mod=resid1,resid.lin=resid2,dfmod=dfmod
,dflin=dflin,F=Fstat,P=pvalue,n=n1)
} #do the F test of the data using the smooth spline line and calculate the P-value using the test





my.dat.plot5a <- function(mat=NOAA,mat0,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,do.plot=T,in.boot=T) {
  if(in.boot) {
    do.sqrt<-F
  } #if in.boot is true then don’t take the square root
  if(!do.sqrt) {
    smstr<-smooth.spline(mat[,i],mat[,j])
    pstr<-predict(smstr,mat0[,i])
    smstr$x<-pstr$x
    smstr$y<-pstr$y
    smstr.lin<-smooth.spline(mat[,i],mat[,j],df=2)
    #if you don’t take the square root then create a line with a smooth spline and two degrees of freedom
    if(do.plot) {
      plot(mat[,i],mat[,j],xlab=zxlab,ylab=zylab,main=zmain)
      lines(smstr,col=zcol)
      lines(smstr.lin,col=(zcol+1))
    } #plot the smooth spline line
    resid1<-mat[,j]-predict(smstr,mat[,i])$y
    resid2<-mat[,j]-predict(smstr.lin,mat[,i])$y
  }
  else {
    smstr<-smooth.spline(mat[,i],sqrt(mat[,j]))
    pstr<-predict(smstr,mat0[,i])
    smstr$x<-pstr$x
    smstr$y<-pstr$y
    smstr.lin<-smooth.spline(mat[,i],sqrt(mat[,j]),df=2)
    #if you are taking the square root, create the line with smooth spline and two degrees of freedom, but with the square root of mat[,j] instead of regular mat[,j]
    if(do.plot) {
      plot(mat[,i],sqrt(mat[,j]),xlab=zxlab,ylab=zylab,main=zmain)
      lines(smstr,col=zcol)
      lines(smstr.lin,col=(zcol+1))
    } #plot the smooth spline line
    resid1<-sqrt(mat[,j])-predict(smstr,mat[,i])$y
    resid2<-sqrt(mat[,j])-predict(smstr.lin,mat[,i])$y
  }
  dfmod<-smstr$df
  dflin<-2
  ssmod<-sum(resid1^2)
  sslin<-sum(resid2^2)
  numss<-sslin-ssmod
  n1<-length(mat[,j])
  Fstat<-(numss/(dfmod-dflin))/(ssmod/(n1-dfmod))
  pvalue<-pf(Fstat,dfmod-dflin,n1-dfmod)
  stat.out<-list(smstrmod=smstr,smstrlin=smstr.lin,resid.mod=resid1,resid.lin=resid2,dfmod=dfmod,dflin=dflin,F=Fstat,P=pvalue,n=n1)
} #do the F test of the data using the smooth spline line and calculate the P-value using the test
