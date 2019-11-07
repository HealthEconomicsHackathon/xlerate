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


compute <- function(exprs, inputs = NULL, check = FALSE) {
  state <- new.env(parent = EXCEL)
  if (!is.null(inputs)) {
    check <- FALSE
    list2env(as.list(inputs), state)
    exprs <- exprs[!(names(exprs) %in% names(inputs))]
  }
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


## Doing the simplification here will be hard because we don't have a
## nice way of shaping the outputs, but that will be required more
## generally.
run <- function(obj, check = TRUE) {
  res <- compute(obj$exprs, check)
  vapply(obj$outputs, get0, numeric(1), res)
}
