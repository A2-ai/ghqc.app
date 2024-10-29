
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ghqc.app <a href="https://github.com/a2-ai/ghqc.app/"><img src="man/figures/logo.png" align="right" height="139" alt="ghqc.app website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/A2-ai/ghqc.app/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/A2-ai/ghqc.app/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of the ghqc ecosystem is to simplify, standardize, and improve
traceability of the QC process through the use of shiny apps which
create GitHub Issues and Milestones for your project. ghqc.app is
intended to be used with the [ghqc](https://github.com/a2-ai/ghqc)
package, which will aid in the setup and launching of the shiny apps.
The ghqc.app package has 3 main applications/functions:

1.  GHQC_ASSIGN_APP: This app will help you to assign a set of files,
    checklists, and reviewers to a GitHub Milestone to kick-off the QC
    process.

2.  GHQC_RESOLVE_APP: After each cycle of QC review, this app will help
    you to communicate your resolution of a comment to the reviewer
    through file differences

3.  GHQC_RECORD_APP: At the end of the QC review, this app will help you
    to create a record, in the form of a pdf, to be saved to show the
    results of the QC review

## Installation

You can install the development version of ghqc.app from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("a2-ai/ghqc.app")
```

## Interacting with ghqc

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ghqc.app)
## basic example code
```
