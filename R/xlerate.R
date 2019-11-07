##' Create a reference to inputs or outputs
##' @title Create a reference to inputs or outputs
##'
##' @param ref A character vector of cell references (can be ranges)
##'
##' @param sheet The sheet number to be used wherever a non-sheet
##'   scoped reference is used.
##'
##' @param label Optional list of row/column offsets to locate labels
##' @export
xlerate_ref <- function(ref, sheet, label = NULL) {
  list(ref = ref, sheet = sheet, label = label)
}


##' Create a function from an excel workbook
##' @title Create a function from an excel workbook
##'
##' @param path Path to the xlsx file
##'
##' @param inputs A \code{\link{xlerate_ref}} object with input references
##'
##' @param outputs  A \code{\link{xlerate_ref}} object with output references
##'
##' @param check Logical, indicating if the workbook should be
##'   recomputed to check that we agree with Excel
##'
##' @export
xlerate <- function(path, inputs, outputs, check = TRUE) {
  w <- rexcel::rexcel_read_workbook(path, progress = FALSE)

  inputs <- add_labels(inputs, w)
  outputs <- add_labels(outputs, w)

  obj <- process(outputs$cells, w)

  ## It would be good to come up with a matrix of input-by-node here
  ## for computing the required invalidated paths
  if (check) {
    compute(obj$exprs, NULL, TRUE)
  }

  ret <- function(x) {
    if (length(x) == 0) {
      x <- NULL
    } else if (is.null(names(x))) {
      stopifnot(length(x) == nrow(inputs$cells$label))
      names(x) <- inputs$cells$name
    } else {
      i <- match(names(x), inputs$cells$label)
      stopifnot(!any(is.na(i)))
      names(x) <- inputs$cells$name[i]
    }
    res <- compute(obj$exprs, x, FALSE)
    vnapply(obj$outputs, get0, res)
  }

  input_values <- cell_value(inputs$cells, w)
  attr(ret, "input") <- setNames(input_values, inputs$cells$label)
  class(ret) <- c("xlerate", "function")

  ret
}


##' @export
print.xlerate <- function(x, ...) {
  cat("<an xlerate object>\n")
  invisible(x)
}


add_labels <- function(x, w) {
  x$cells <- cell_ref(x$ref, w, x$sheet, check_range = TRUE)
  if (!is.null(x$label)) {
    x$cells$label <- cell_nearby(x$cells, w, x$label)
  }
  x
}
