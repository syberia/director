#' Define a search pattern for use with the find method on a director.
#'
#' A search pattern is one of the following:
#'
#'   \describe{
#'     \item{exact}{ match. The strings must match exactly this value.}
#'     \item{partial}{ match. The strings which contain this string as
#'        a substring will be matched.}
#'     \item{wildcard}{ match. Fuzzy matching like in the ctrl+p plugin
#'        for vim. If the pattern is "abc", it will be translated to the
#'        regular expression ".*a.*b.*c.*", that is, any characters followed
#'        by an 'a' followed by any characters followed by a 'b' followed by
#'        any characters followed by a 'c' followed by any characters (e.g.,
#'        "fabulous cake"). Note that wildcard match is case insensitive.}
#'      \item{regex}{ match. Apply a regular expression filter to the
#'        set of strings.}
#'   }
#' 
#' @param pattern character. The pattern to search for.
#' @param method character. The search pattern method, one of "exact",
#'    "partial", or "wildcard".
#' @note Patterns can be combined using the \code{|} and \code{&} operators.
#' @examples
#' \dontrun{
#'   d$find(search_pattern("this/file", "exact"))
#'   # If d is a director object, the above will find exactly the resource
#'   # "this/file".
#' 
#'   d$find(search_pattern("this", "partial"))
#'   # The above will find any resource containing "this" as a substring.
#'
#'   d$find(search_pattern("this", "wildcard"))
#'   # The above will find any resource containing the consecutive letters
#'   # "this" separated by arbitrary strings.
#'
#'   d$find(search_pattern("foobar", "partial") | search_pattern("root", "exact"))
#'   # The above will find any resource with the substring "foobar" or having
#'   # exactly the name "root".
#' }
search_pattern <- function(pattern, method) {
  msg <- function(x) {
    stop("Search ", deparse(substitute(x)) ," must be of type character; ",
         "instead I got a ", class(x)[1])
  }

  if (!is.character(method)) { msg(method) }
  if (!is.character(pattern)) { msg(pattern) }

  ## A search pattern is a method for filtering a set of strings that is highly
  ## composable. For example, if we have `c("foobar", "barbaz" "bazbux")`,
  ## we can use the pattern `search_pattern("bar", "partial")` to select the
  ## first two, since they have the substring "bar".
  ##
  ## We can apply `and` and `or` operations to search patterns to mix and match
  ## them. For example,
  ##   `search_pattern("bar", "partial") & search_pattern("baz", "wildcard")`
  ## will match strings that contain the substring "bar", as well as the
  ## characters "b", "a", and "z" separated by arbitrary strings (e.g.,
  ## "BAzaR"). 
  search_pattern_(pattern, tolower(method))
}

search_pattern_ <- function(pattern, method) {
  ## We use a recursive solution: either the pattern or the method can be
  ## a "search pattern join" (the `&` and `|` operation described above).
  ## In this case, we just return the join.
  if (is.search_pattern_join(pattern)) { pattern }
  else if (is.search_pattern_join(method)) { method }
  else if (length(pattern) > 1) {
    ## If there is more than one pattern specified, we treat this as an OR
    ## condition: either pattern 1, or pattern 2, etc.
    Reduce(function(x, y) {
      search_pattern_(x, method) | search_pattern_(y, method)
    }, pattern)
  } else if (length(method) > 1) {
    ## If there is more than one method specified, this is also an OR condition.
    ## This situation is rare, since we don't often want to say "match this
    ## as a wildcard or as a regex".
    Reduce(function(x, y) {
      search_pattern_(pattern, x) | search_pattern_(pattern, y)
    }, method)
  } else {
    `verify_search_pattern_method!`(method)
    ## We use an S3 class to track information about the pattern (the string
    ## to match and the method).
    as.search_pattern(list(pattern = pattern, method = method))
  }
}

`verify_search_pattern_method!` <- function(method) {
  ## `getNamespace` is a base R function that allows us to grab the namespace
  ## of the director package. To understand the difference between a package
  ## environment and package namespace, see Suraj Gupta's wonderful guide
  ## on [how R searches and finds stuff](http://blog.obeautifulcode.com/R/How-R-Searches-And-Finds-Stuff/).
  ## 
  ## Instead of hardcoding all the pattern methods we support like "exact"
  ## and "wildcard", we look into this package's namespace and see if there
  ## is an "apply_pattern.exact" or "apply_pattern.wildcard" function. If
  ## someone wants to implement a new pattern method, they only need to
  ## define an "apply_pattern.new_method" function below, which is cleaner.
  ok <- exists(paste0("apply_pattern.", method), envir = getNamespace("director"))
  if (!ok) { stop("Invalid search pattern.") }
}

search_pattern_join <- function(pattern1, pattern2, type) {
  stopifnot(identical(type, "and") || identical(type, "or"))
  ## An S3 object that tracks an `&` or `|` condition on patterns.
  as.search_pattern(structure(list(pattern1, pattern2, type = type),
                              class = c("search_pattern_join")))
}

as.search_pattern <- function(x) {
  ## Remember that when changing classes, the class should be prepended
  ## rather than appended, since R's S3 mechanism looks left-to-right for
  ## S3 methods.
  class(x) <- c(class(x), "search_pattern")
  x
}

is.search_pattern <- function(x) { is(x, "search_pattern") }
is.atomic_search_pattern <- function(x) {
  ## An atomic search pattern is one that has not been joined using
  ## the `&` or `|` operators.
  is.search_pattern(x) && !is.search_pattern_join(x)
}
is.search_pattern_join <- function(x) { is(x, "search_pattern_join") }

## This funky looking notation says "implement the `|` operator for the
## "search_pattern" S3 class.
`|.search_pattern` <- function(e1, e2) {
  stopifnot(is(e2, "search_pattern"))

  search_pattern_join(e1, e2, type = "or")
}

`&.search_pattern` <- function(e1, e2) {
  stopifnot(is(e2, "search_pattern"))

  search_pattern_join(e1, e2, type = "and")
}

#' Apply a pattern filter to a character vector.
#' 
#' @param pattern search_pattern.
#' @param strings character. The strings to filter down.
apply_pattern <- function(pattern, strings) {
  if (is.atomic_search_pattern(pattern)) {
    ## First we apply the pattern's method as an S3 class. For example,
    ## a wildcard pattern would get the "wildcard" class.
    class(pattern) <- c(pattern$method, class(pattern)) 

    ## R's `UseMethod` function *dispatches* an S3 generic. This means
    ## that we will call `apply_pattern.wildcard` on the `pattern`
    ## object without having to figure out that is the appropriate method.
    UseMethod("apply_pattern", object = pattern)
  } else if (is.search_pattern_join(pattern)) {
    operand <- if (pattern$type == "and") { intersect } else { union }
    ## `Recall` is an R shortcut for "recursively call this function", i.e.,
    ## `apply_pattern(...)`.
    operand(Recall(pattern[[1]], strings), Recall(pattern[[2]], strings))
  } else { stop("Invalid pattern") }
}

apply_pattern.exact <- function(pattern, strings) {
  ## An exact match is just a single string that matches on the nose.
  if (any(pattern$pattern == strings)) { pattern$pattern }
  else { character(0) }
}

apply_pattern.wildcard <- function(pattern, strings) {
  ## First, replace all regex special characters with the correct backslashed
  ## version. I wish I could say I knew how many backslashes are necessary
  ## but it was trial and error. ;)
  pattern <- gsub("([]./\\*+()])", "\\\\\\1", pattern$pattern)

  ## The only regex special characters we allow in wildcards are `^` and `$`
  ## to mark beginning and ends of strings. The rest gets replaced with a
  ## `.*` prefix. For example, "^abc" would be come "^.*a.*b.*c".
  pattern <- gsub("([^\\$^])", ".*\\1", pattern) # turn this into ctrl+p
    
  ## But of course "^.*a" is just "a"! So we turn that special sequence into
  ## just "^".
  pattern <- gsub("^.*", "^", pattern, fixed = TRUE)

  ## By default, wildcards matching is case insensitive, since it will be used
  ## to filter on file names, and we rarely have file collisions based on case
  ## (and when you do you should think of a better file name instead!).
  grep(pattern, strings, value = TRUE, ignore.case = TRUE)
}

apply_pattern.partial <- function(pattern, strings) {
  ## Just a plain substring match.
  grep(pattern$pattern, strings, fixed = TRUE, value = TRUE)
}

apply_pattern.regex <- function(pattern, strings) {
  ## Just a plain regex match.
  grep(pattern$pattern, strings, value = TRUE)
}

apply_pattern.idempotence <- function(pattern, strings) {
  # TODO: (RK) Consider the string "."
  ## For an overview of idempotence, see the documentation on the director
  ## exists method.
  ##
  ## The idempotent pattern finds the helpers in a set of filenames and
  ## strips them. For example, `c("foo.R", "bar/bar.R", "bar/baz.R")` would
  ## be reduced to just `c("foo.R", "bar/bar.R")` (note that this pattern
  ## is not just a filter and has side effects).
  ## 
  ## Grab the indices of those files whose base name is the same as their
  ## enclosing directory name (for example, "foo/bar/bar.R").
  idempotent <- vapply(strings, function(x) basename(x) == basename(dirname(x)), logical(1))

  ## What are the actual directory names? (for example, "foo/bar")
  idem_dirs  <- dirname(strings[idempotent])

  ## Helper files are the files in the idem_dirs computed above who do not
  ## share their name with the parent directory. We need to find the indices
  ## of these files in our strings.
  helpers <- vapply(strings, function(x) dirname(x) %in% idem_dirs, logical(1))

  ## Now replace the idempotent files with their directory names. In director,
  ## the name of an idempotent resource is sans the basename
  ## (for example, "foo/bar" rather than "foo/bar/bar.R").
  strings[idempotent] <- idem_dirs
    
  ## Strip the helper files but keep the idempotent resources. Note that
  ## since the idempotent files, like "foo/bar/bar.R", are within an 
  ## idempotent directory, like "foo/bar", they will be marked as TRUE
  ## in the `helpers` vector.
  strings[!helpers | idempotent]
}

