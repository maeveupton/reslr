# reslr
 
`reslr` is a package designed to account for measurement errors within commonly used models (e.g. linear regression, change-point regression, Integrated Gaussian process regression, splines, generalised additive models) for examining data in time and space. The package has the ability to utilise data derived from paleoenvironmental reconstructions such as examining relative sea level (RSL) over time and space. 

There are a variety of different statistical model types available to the user, all within a Bayesian Framework, and they uses Gibbs sampling and Markov Chain Monte Carlo (MCMC) algoritms via the Just Another Gibbs Sample (JAGS) software. To download the JAGS package use this [link](https://sourceforge.net/projects/mcmc-jags/). 

To use the `reslr` package install it using the following method:
```{r}
devtools::install_github("maeveupton/reslr")
```
The package is not yet on CRAN.

Next, load the package with
```{r}
library(reslr)
```
Check out the user manuals via:
```{r}
vignette('reslr')
```
