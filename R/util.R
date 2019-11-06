`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}


substitute_ <- function(expr, env) {
  eval(substitute(substitute(y, env), list(y = expr)))
}
