starve is an R package for analyzing spatio-temporal point-referenced data, a data format typical of ecological research surveys.



# Installing the Package

You can install the package using

```R
devtools::install_github("lawlerem/starve", build_vignettes = TRUE)
```

Once installed, the package vignette can be viewed by opening R and running

```R
vignette("starve-tour", package = "starve")
```

This vignette acts as an in-depth reference for working with the starve package.


# Guiding Design Principles

## Simple Interface

With a simple interface, users of the package can spend more of their time and energy on learning from their data and less on how to code the steps of the data analysis.
The starve package wraps its functionality in four main functions:

  - `strv_prepare()` takes a model formula and a data.frame and pre-processes the data to create a model object which is then used in the other three functions.
  - `strv_fit()` performs maximum likelihood inference on a model object, obtaining parameter estimates and standard errors.
  - `strv_predict()` uses a model object to predict at user-defined locations and times.
  - `strv_simulate()` simulates a new dataset from a model object.

One aspect of the package that some users may not be familiar with is the use of S4 classes, which we use for a model object (among other things).
For the user, S4 classes work similarly to lists but instead of using the `$` symbol to access parts of the list you use functions to access part of an S4 class.
For example to view the parameter estimates after running `strv_fit()`, you would use

```R
parameters(x)
```

instead of

```R
x$parameters
```

Full details for exploring a model object are given in the package vignette.



## Native Use of Spatial Data Formats

R has a rich spatial and spatio-temporal data ecosystem, see the CRAN Task views on [Analysis of Spatial Data](https://cran.r-project.org/web/views/Spatial.html) and [Handling and Analyzing Spatio-Temporal Data](https://cran.r-project.org/web/views/SpatioTemporal.html).
The starve package directly accepts the standard "simple features" spatial data format as implemented in the sf package, and also integrates use of the stars package for model predictions.
This allows users to bypass many of the data wrangling steps that can be involved when analyzing spatio-temporal data, leading to a streamlined workflow.



## Computational Efficiency

The starve package uses a variety of techniques to make analysis computationally efficient, which has traditionally been the main limiting factor for analysis of spatio-temporal data.

  - It is built using [TMB](https://github.com/kaskr/adcomp) which uses the combination of the C++ language, the Laplace approximation, and automatic differentiation for quickly evaluating marginal likelihoods for mixed effects models.
  - The model implemented in the package uses a nearest-neighbour Gaussian process to model spatial variation in the data, which replaces the computationally expensive task of inverting a single large covariance matrix with the relatively cheap task of inverting many small covariance matrices.
  - The package takes advantage of the spatio-temporal structure and the fact that the variance of conditional Gaussian distributions do not depend on the value conditioned upon to cache covariance calculations from one year to the next.
