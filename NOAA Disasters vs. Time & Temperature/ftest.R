#Group: Ella Walmsley, Shreya Ghosh, Janine Yanes
ftest = function(mat = NOAAnew,i = 3,j = 2,zxlab = "Change in Temperature",zylab = "Number of Disasters",zmain,zcol = 2,do.sqrt = F) {
  if(!do.sqrt) {
    zmain = "NOAAnew (square root not taken)"
    plot(mat[,i],mat[,j],xlab = zxlab,ylab = zylab,main = zmain)
    #lines(smooth.spline(mat[,i],mat[,j]),col=zcol)
    smstr = smooth.spline(mat[,i],mat[,j])
    smstr.lin = smooth.spline(mat[,i],mat[,j],df=2)
    lines(smstr,col=zcol)
    lines(smstr.lin,col=(zcol+1))
    resid1 = mat[,j]-predict(smstr,mat[,i])$y
    resid2 = mat[,j]-predict(smstr.lin,mat[,i])$y
  }
  else {
    zmain = "NOAAnew (square root taken)"
    plot(mat[,i],sqrt(mat[,j]),xlab = zxlab,ylab = zylab,main = zmain)
    #lines(smooth.spline(mat[,i],sqrt(mat[,j])),col=zcol)
    smstr = smooth.spline(mat[,i],sqrt(mat[,j]))
    smstr.lin = smooth.spline(mat[,i],sqrt(mat[,j]),df=2)
    lines(smstr,col=zcol)
    lines(smstr.lin,col=(zcol+1))
    resid1 = sqrt(mat[,j])-predict(smstr,mat[,i])$y
    resid2 = sqrt(mat[,j])-predict(smstr.lin,mat[,i])$y
  }
  #calculating the rest of the values needed for f-test, completing f-test
  dfmod = smstr$df
  dflin = 2
  ssmod = sum(resid1^2)
  sslin = sum(resid2^2)
  numss = sslin-ssmod
  n1 = length(mat[,j])
  Fstat = (numss/(dfmod-dflin))/(ssmod/(n1-dfmod))
  pvalue = 1-pf(Fstat,dfmod-dflin,n1-dfmod)
  print(paste("p-value of F-test is", as.character(pvalue)))
}

#running f-test function
ftest()
ftest(do.sqrt = T)
