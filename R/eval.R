## *Lots* of hard-to-get-right things here:
##
## * Treatment of DIV/0 errors etc
## * Lots of functions to get working
## * Ranges of cells
na_zero <- function(x) {
  if (is.na(x)) 0 else x
}


EXCEL <- new.env(parent = emptyenv())
EXCEL$`(` <- `(`


EXCEL$`+` <- function(a, b) {
  if (missing(b)) {
    return(a)
  }
  na_zero(a) + na_zero(b)
}


EXCEL$`-` <- function(a, b) {
  if (missing(b)) {
    return(-na_zero(a))
  }
  na_zero(a) - na_zero(b)
}


EXCEL$`*` <- function(a, b) {
  na_zero(a) * na_zero(b)
}


EXCEL$`/` <- function(a, b) {
  na_zero(a) / na_zero(b)
}


compute <- function(exprs, check = TRUE) {
  state <- new.env(parent = EXCEL)
  for (x in exprs) {
    if (is.null(x$formula)) {
      state[[x$name]] <- x$value
    } else {
      state[[x$name]] <- eval(x$formula, state)
      if (check && !isTRUE(all.equal(x$value, state[[x$name]]))) {
        stop("Found inconsistency in calculation")
      }
    }
  }
  state
}
