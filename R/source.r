# We overwrite the base source function to allow us to keep track
# of whether or not files loaded in a syberia directory have been modified.
#' Overwrite built-in source function.
#' @name source
# TODO: (RK) Re-investigate this. Deprecated for now.
.source <- function(filename, ...) {
  filename <- normalizePath(filename)
  root <- syberia_root()
  if (substring(filename, 1, nchar(root)) == root &&
      identical(get_cache('runtime/executing'), TRUE)) {
    # We are running a syberia resource
    # TODO: (RK) Maybe just need to compare mtime for this..
    resource <- syberia_resource(filename, root, ...)
    if (resource$modified) set_cache(TRUE, 'runtime/any_modified')
    list(value = resource$value(), invisible = TRUE)
  } else {
    env <- as.environment(list(source = source))
    parent.env(env) <- parent.frame()
    base::source(filename, env, ...)
  }
}

