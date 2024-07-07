# Assignment details

Build a function which:
  * takes as input a data frame, and the two elements of a frame that you want to model
    and chooses whether to take square root or not.
  * Plot the data fit with a smoothing spline with the default cross validation, vs the smoothing spline with 2 degrees of freedom (linear fit).
  * Test the difference of fit using the F test (Normal theory based)
  
Advanced part (Lab time) (Can use codeday4.pck, and comment it. But figure out some of the weird stuff, like Y is do.sqrt turned off inside the bootstrap?)
  * Construct Pvalue using linear fit residual bootstrap (Why is this appropriate for testing linearity? Jow do you get Pvalue?)
    Then build bootstrap confidence intervals for the smooth
  * Look at XY bootstrap vs residual (Why is XY bootstrap such a mess for these confidence intervals?)
