RE_REF <- "(([A-Za-z]+!)?[A-Z]+[0-9]+)"
RE_REF_SHEET <-  "(.*)!.*"

## Need a simple parse - this needs a lot of work, but converts the
## cell references into valid R variables first.  That's needed for
## the cases that are references into other sheets.
parse_formula <- function(x) {
  ## TODO: this is not enough - we should do this much better and as
  ## it is it will not cope with absolute references
  stopifnot(length(x) == 1)
  x <- gsub(RE_REF, "`\\1`", x)
  parse(text = x)[[1]]
}


cell_ref <- function(ref, w, default) {
  addr <- cellranger::as.cell_addr_v(ref, strict = FALSE)
  ret <- data.frame(row = addr$row, col = addr$col)
  ret$sheet <- rep(default, length(ref))

  i <- grepl(RE_REF_SHEET, ref)
  if (any(i)) {
    nm <- sub(RE_REF_SHEET, "\\1", ref[i])
    j <- match(tolower(nm), tolower(w$names))
    stopifnot(!any(is.na(j)))
    ret$sheet[i] <- j
  }

  ret
}


process <- function(ref, sheet, path) {
  w <- rexcel::rexcel_read_workbook(path, progress = FALSE)
  sheet <- 2L
  seen <- lapply(w$sheets, function(x) array(FALSE, dim(x$lookup2)))

  traverse <- list(list(sheet = sheet, ref = ref))
  exprs <- list()

  while (length(traverse) > 0) {
    this <- traverse[[1]]
    traverse <- traverse[-1]

    ref <- this$ref
    sheet <- this$sheet

    x <- cell_ref(ref, w, sheet)
    if (seen[[x$sheet]][x$row, x$col]) {
      next
    }
    seen[[x$sheet]][x$row, x$col] <- TRUE

    s <- w$sheets[[x$sheet]]
    f <- parse_formula(s$cells$formula[s$lookup[x$row, x$col]])
    v <- s$cells$value[s$lookup[x$row, x$col]]
    stopifnot(length(v) == 1)
    vars <- all.vars(f)

    res <- list(sheet = x$sheet,
                row = x$row,
                col = x$col,
                formula = f,
                value = v[[1]],
                deps = vars)
    exprs <- c(exprs, setNames(list(res), ref))
    if (length(vars) > 0L) {
      vars_ref <- cell_ref(vars, w, x$sheet)
      for (v in split(vars_ref, vars_ref$sheet)) {
        s <- v$sheet[[1]]
        extra <- vars[!seen[[s]][cbind(v$row, v$col)]]
        traverse <- c(traverse,
                      Map(list, ref = extra, sheet = rep(s, length(extra))))
      }
    }
  }

  exprs
}


topological_sort <- function(tree) {

}
