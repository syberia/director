#' Managing and Tracking Files in R Projects.
#'
#' The director package is responsible for managing and loading resources in
#' some fixed directory structure. It has support for tracking changes between
#' consecutive loads of resources (so that we can tell if a script was modified
#' since we last ran it) and defining parsers that allow us to generalize from
#' the pernicious simple linear execution that is common to R.
#'
#' @docType package
#' @name director
#' @import digest
NULL
