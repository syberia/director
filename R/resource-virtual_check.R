#' Mark a resource as virtual.
#'
#' Virtual resources are those that are not recorded as a .R file. Instead,
#' the resource's value must be computed using a preprocessor.
#'
#' For example, imagine we have a directory of resources where some of the
#' resources have been re-factored into a package. We would still like to be
#' able to turn objects from that package into proper resources, but they
#' may no longer be encoded as files in the Syberia project.
#'
#' Instead, we could define a preprocessor that looks for those special values
#' and uses the package objects instead.
#'
#' When parsing a resource, the local \code{virtual} is injected for use in
#' the preprocessor which corresponds to whether the resource seems
#' non-existent to the director (i.e., has no supporting .R file).
#'
#' @name virtual resource
#' @aliases virtual_check
#' @param object active_resource. See \code{\link{active_resource}}.
#' @param ... additional parameters to pass to the next layer in the resource
#'    parsing tower.
#' @param virtual_check.skip logical. Whether or not to skip the virtual
#'    check entirely. Generally only used by internal calls.
#' @seealso \code{\link{active_resource}}, \code{\link{tower}}
#' @return The parsed resource.
#' @note The parameters must be named \code{object} and \code{...} due to
#'    this method's inclusion in a \code{\link{tower}}.
#' @examples
#' \dontrun{
#'   # We will use the example of a syberia project.
#'   # See github.com/robertzk/syberia.
#'
#'   # lib/mungebits has imputer.R and no other files, but the package
#'   # syberiaMungebits has more mungebits. We can define the following
#'   # preprocessor.
#'
#'   #=== config/routes.R ===
#'   list(
#'     "lib/mungebits" = "mungebits"
#'   )
#'
#'   #=== lib/controllers/mungebits.R ===
#'   preprocessor <- function(resource, virtual) { 
#'     mungebit <- basename(resource) # lib/mungebits/discretizer becomes discretizer
#'     if (virtual) {
#'       if (exists(mungebit, envir = getNamespace("syberiaMungebits"), inherits = FALSE)) {
#'          # The function exists in the syberiaMungebits package.
#'          get(mungebit, envir = getNamespace("syberiaMungebits")))))
#'        } else {
#'          stop("No mungebit called ", sQuote(resource))
#'        }
#'     } else {
#'       source() # Source the mungebit file as usual
#'     }
#'   }
#'
#'   # Construct the mungebit parser as usual.
#'   function(output) { mungebits::mungebit(output$train, output$predict) }
#'
#'   #=== R console ===
#'   d <- syberia_project("/some/dir")
#'   d$resource("lib/mungebits/imputer") # Will use lib/mungebits/imputer.R
#'   d$resource("lib/mungebits/discretizer") # Will use syberiaMungebits::discretizer
#' }
virtual_check <- function(object, ..., virtual_check.skip = FALSE) {
  if (isTRUE(virtual_check.skip)) { return(yield()) }

  director <- object$resource$director

  ## An object is considered to be "virtual" if it has no corresponding file,
  ## that is, the director object cannot find a resource by that name.
  virtual  <- !director$exists(object$resource$name)
    
  ## Since `object$injects` is an environment, this is equivalent to
  ## `object$injects$virtual <- virtual`, but this notation is clearer as it
  ## is stylistically similar to the later layers in the resource parsing tower
  ## where multiple values are injected simultaneously.
  object$injects %<<% list(virtual = virtual)
  

  ## If a resource is virtual but has no preprocessor, we cannot possibly
  ## parse it, as the default preprocessor is simply sourcing the file 
  ## corresponding to the resource.
  if (virtual && !director$has_preprocessor(object$resource$name)) {
    project_name <- director$project_name()
    stop(sprintf("Cannot find resource %s in%s project %s.",
      sQuote(crayon::red(object$resource$name)),
      if (nzchar(project_name)) paste0(" ", project_name) else "",
      sQuote(crayon::blue(director$root()))))
  }

  ## See tower.R for an explanation of `yield`.
  yield()
}

