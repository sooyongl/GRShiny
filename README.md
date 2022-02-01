
# GRShiny <img src='https://raw.githubusercontent.com/sooyongl/GRShiny/main/man/figures/hextile.png?raw=true' align="right" height="30" />

## Overview

Simulation and analysis of graded response data with different types of
estimator. Also, interactive shiny application is provided with graphics
for characteristic and information curves.

## Install

Install the latest release from CRAN:

``` r
devtools::install_github("sooyongl/GRShiny")
```

The documentation is available at
[here](https://sooyongl.github.io/GRShiny/).

## GRM data simulation

### Item parameters for graded response model

``` r
item_pars <- genIRTpar(nitem = 10,
                       ncat = 3,
                       nfac = 1)
```

### Individual true latent traits

``` r
true_theta <- genTheta(nsample = 500,
                       nfac = 1)
```

### GRM data

``` r
grm_dt <- genData(eta = true_theta,
                  ipar = item_pars)
```

## GRM data simulation

### Generate lavaan syntax

``` r
lav_syn <- genLavSyn(dat = grm_dt, nfac = 1)
```

### Conduct GRM with two different estimators

``` r
runGRM(dat = grm_dt, lav.syntax = lav_syn,
       estimator = "WL")

runGRM(dat = grm_dt, lav.syntax = lav_syn,
       estimator = "ML")
```

## Launch app

``` r
startGRshiny()
```
