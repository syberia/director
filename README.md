Director [![Build Status](https://travis-ci.org/robertzk/director.svg?branch=master)](https://travis-ci.org/robertzk/director) [![Coverage Status](https://coveralls.io/repos/robertzk/director/badge.png)](https://coveralls.io/r/robertzk/director)
========

Director is an R package for easily managing R scripts in a directory and treating them as "resources."
To install, run the following in the R console.

```r
if (!require(devtools)) install.packages('devtools')
devtools::install_github('robertzk/director'); library(director)
```

Creating a director
======

```r
d <- director('~/your/R/projects/directory')
```

You can now look for all files in your project using `d$find()`.

