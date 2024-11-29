
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PaRe

<!-- badges: start -->

[![Lifecycle:experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
![R-CMD-check](https://github.com/darwin-eu-dev/PaRe/actions/workflows/R-CMD-check.yaml/badge.svg)
[![CRAN](https://www.r-pkg.org/badges/version/PaRe)](https://CRAN.R-project.org/package=PaRe)

<!-- badges: end -->

PaRe (**Pa**ckage **Re**viewer) is the successor of the
DependencyReviewer package. PaRe reviews other packages during code
review and has the following features:

1.  What dependencies are used, and what functions are used of that
    dependency.
2.  The quality of the code style using lintr.
3.  Code complexity, using the *cyclomatic complexity* scores.
4.  How internally defined functions interact with one another, and
    visualizing this in a diagram.
5.  Fetching locations of defined functions in R-files.
6.  Checking dependencies against user a defined white list.
7.  Count lines of code for different languages by default: R, C++, SQL,
    and Java.
8.  Make a standardized HTML-report exploring the before mentioned
    features.

## Installation <a name="Installation"></a>

You can install the development version of PaRe like so:

``` r
install.packages("remotes")
remotes::install_github("darwin-eu-dev/PaRe")
```

## Latest changes:

1.  Using R6 objects
2.  Generalized function input to use R6 objects
3.  Minor efficiency changes
4.  Major vignette updates
