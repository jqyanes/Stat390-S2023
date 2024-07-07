# Assignment details


Build both:

A Leaps automatic model selector using Cp, and PRESS
* Best k models using Cp
* From those k calculate best PRESS

And a lars automatic model selector using both Cp and cross validation MSE
* Best k models using Cp
* Best k using cross validated MSE
* Combine and choose 1

Combine the two in a single function

Compare predictions of each operating on Full 2nd order set of variables for Auto data, and on each country of origin separately with full 2nd order set of variables.   
* Calculate actual predictions X%*%lars.beta, and X%\*%leaps.beta

And compare actual regression coefficients across countries to see if you can understand any country differences (anything interesting in you can pick up on what is going on in the different countries)
