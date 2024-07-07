# Assignment details

Build a confidence interval estimator for the mean based on the bootstrap (use 10000 =nboot).

Build a simulator that draws n samples form a lognormal distribution (rlnorm) and builds both the central limit theorem based confidence interval, and compares it to the coverage rate for the bootstrap (confidence interval based on the the bootstrap program) (1000 simulation runs minimum).

Now build the bootstrap estimator into a simulator which compares how well the pivotal bootstrap confidence interval covers the true value vs a normal theory confidence interval.

Run it for lognormal (nsim=1000) for n = 3, 10, 30, 100, alpha = .1 and alpha = .05 (8 runs total) (Fix the program for small n?)

Compare the coverage of the 2 confidence intervals with nominal coverage and write up the results with a table.

Do NOT use boot package in R.
