# Assignment details


  Rewrite the bootstrap program you wrote for "Bootstrapping means assessing accuracy."
  
  * Where you are taking the mean, put in a user provided statistical function.
  * Every place you take a standard deviation, use a different bootstrap program to calculate the standard error of your statistic instead. So inside the bootstrap loop in the main program, you call a different bootstrap program to estimate variance (if you want a hint, look at bootstrap.exp in xy.pck).

Apply both the original bootstrap confidence estimate of the mean, and your new bootstrap using the median as the estimate of location to the data set special.sample in xy.pck.

Provide the 95% confidence intervals.
