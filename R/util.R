`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}


substitute_ <- function(expr, env) {
  eval(substitute(substitute(y, env), list(y = expr)))
}


vnapply <- function(X, FUN, ...) {
  vapply(X, FUN, numeric(1), ...)
}


vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}
