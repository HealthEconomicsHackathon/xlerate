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
EXCEL$`c` <- `c`


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


EXCEL$`SUM` <- function(...) {
  args <- list(...)
  sum(unlist(args, FALSE, FALSE))
}


compute <- function(exprs, inputs = NULL, check = FALSE, verbose = FALSE) {
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
      value <- eval(x$formula, state)
      if (check && !isTRUE(all.equal(x$value, value))) {
        stop("Found inconsistency in calculation computing ", x$name)
      }
      if (verbose) {
        message(sprintf("%s: %s => %s", x$name, deparse(x$formula), value))
      }
      state[[x$name]] <- value
    }
  }
  state
}
