
<!-- README.md is generated from README.Rmd. Please edit that file -->

# esteem.overview

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of esteem.overview is to explore data extractions from Quadrige
database.

## Installation

You can install the development version of esteem.overview from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("FlorenceMounier/esteem.overview")
#> Using GitHub PAT from the git credential store.
#> Skipping install of 'esteem.overview' from a github remote, the SHA1 (7a8bbc37) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

## About

This README has been compiled on the 2025-07-04 14:11:39.086113

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading esteem.overview
#> ── R CMD check results ───────────────────────── esteem.overview 0.0.0.9000 ────
#> Duration: 29.5s
#> 
#> ❯ checking for missing documentation entries ... WARNING
#>   Jeux de données non documentés :
#>     'data_benthos' 'data_contamination' 'data_phyto' 'data_pomet'
#>   All user-level objects in a package should have documentation entries.
#>   See chapter 'Writing R documentation files' in the 'Writing R
#>   Extensions' manual.
#> 
#> ❯ checking for unstated dependencies in examples ... WARNING
#>   aucun fichier source n'est trouvé
#> 
#> 0 errors ✔ | 2 warnings ✖ | 0 notes ✔
#> Error: R CMD check found WARNINGs
```
