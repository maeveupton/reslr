# reslr

<!-- badges: start -->
<a href = "https://maeveupton.github.io/reslr/"><img src="https://raw.githubusercontent.com/maeveupton/reslr/master/badge/reslr_badge.png" width="240" height="276" align="right" />
<!-- badges: end -->

 
`reslr` is a package designed to perform Bayesian modelling of relative sea level data. We provide a range of different statistical models which include: linear regression, change-point regression, integrated Gaussian process regression, splines, generalised additive models. In addition, we account for measurement uncertainty in multiple dimensions which is a common when examining relative sea-level data. The package has a unifying framework for loading data, fitting models, and summarising relative sea level (RSL) change over time and space. The output plots provide sea level curves and corresponding rates of change with appropriate consideration of uncertainty. An example dataset is included in the package which contains proxy reconstructed RSL records from the Atlantic Coast of North America. Also, the user can select to include instrumental data from tide gauges using the Permanent Service for Mean Sea Level [online database](https://psmsl.org/). 
 
Markov Chain Monte Carlo (MCMC) algorithms via the Just Another Gibbs Sample (JAGS) software is used for our Bayesian statistical models. Before using the `reslr` package, the user must download the JAGS package using this [link](https://sourceforge.net/projects/mcmc-jags/). 

To examine the full vignette check out:
[reslr](https://maeveupton.github.io/reslr/articles/reslr.html)

For a quick start use:
[Quick start](https://maeveupton.github.io/reslr/articles/quick_start.html)
 
For a more detailed discussion on advanced options within the `reslr` package examine:
[Advanced](https://maeveupton.github.io/reslr/articles/advanced_reslr.html)


# Installing the reslr package

To install from CRAN

```{r}
install.packages("reslr")
```


To install from Github

```{r}
# install.packages("devtools")
devtools::install_github("maeveupton/reslr")
```

Next, load the package with

```{r}
library(reslr)
```

