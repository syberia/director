# Director [![Build Status](https://travis-ci.org/robertzk/director.svg?branch=master)](https://travis-ci.org/robertzk/director) [![Coverage Status](https://coveralls.io/repos/robertzk/director/badge.png)](https://coveralls.io/r/robertzk/director)

Due to its interactive nature, R tends to produce project structures
that are a collection of loosely organized scripts. With the exception
of [package development](http://r-pkgs.had.co.nz/r.html), there is no
consensus in the R community as to best practices for project structure.
Packages are well suited for abstract functions and features. However,
they are poor for coordinating code that is very domain-specific.
For example, the specific munging for a fixed data set belongs in an R script
and not within a package. 

Director aims to partially solve the problem of project structure in R
by re-defining R scripts as "resources" and allowing the developer to focus
only on the critical components.

# Installation

Director is not yet available on CRAN (as of March 1, 2015).
To install the latest development build directly from Github,
run the following in the R console.

```r
if (!require(devtools)) install.packages('devtools')
devtools::install_github('robertzk/director'); library(director)
```

# Creating a director

```r
d <- director('~/your/R/projects/directory')
```

You can now look for all files in your project using `d$find()`.

