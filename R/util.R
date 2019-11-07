`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}


substitute_ <- function(expr, env) {
  eval(substitute(substitute(y, env), list(y = expr)))
}


vnapply <- function(X, FUN, ...) {
  vapply(X, FUN, numeric(1), ...)
}


xlerate_file <- function(name) {
  system.file(name, package = "xlerate", mustWork = TRUE)
}


set_names <- function(x, nms) {
  names(x) <- nms
  x
}
