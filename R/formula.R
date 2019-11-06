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
  ret$ref <- ref

  i <- grepl(RE_REF_SHEET, ref)
  if (any(i)) {
    nm <- sub(RE_REF_SHEET, "\\1", ref[i])
    j <- match(tolower(nm), tolower(w$names))
    stopifnot(!any(is.na(j)))
    ret$sheet[i] <- j
    ret$ref[i] <- paste0(cellranger::num_to_letter(ret$col[i]), ret$row[i])
  }

  ret$name <- sprintf("s%d.%s", ret$sheet, ret$ref)

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
    if (identical(f, NA)) {
      f <- NULL
    }
    value <- s$cells$value[s$lookup[x$row, x$col]]
    stopifnot(length(value) == 1)

    vars <- all.vars(f)
    if (length(vars) > 0L) {
      deps <- cell_ref(vars, w, x$sheet)
      subs <- setNames(lapply(deps$name, as.name), vars)
      f <- substitute_(f, subs)
      for (v in split(deps, deps$sheet)) {
        s <- v$sheet[[1]]
        extra <- vars[!seen[[s]][cbind(v$row, v$col)]]
        traverse <- c(traverse,
                      Map(list, ref = extra, sheet = rep(s, length(extra))))
      }
    } else {
      deps <- NULL
    }
    res <- c(x, list(formula = f, value = value[[1]], deps = deps))
    exprs <- c(exprs, setNames(list(res), x$name))
  }

  i <- topological_order(lapply(exprs, function(x) x$deps$name))
  exprs[i]
}


## This algorithm comes from here:
## http://blog.jupo.org/2012/04/06/topological-sorting-acyclic-directed-graphs/
## and assumes that the graph is expressed as a *named* list.  The
## daughters of an element are its dependencies.
topological_order <- function(graph) {
  m <- matrix(FALSE, length(graph), length(graph))
  for (i in seq_along(graph)) {
    m[, i] <- unname(names(graph) %in% graph[[i]])
  }

  pending <- rep(TRUE, length(graph))
  graph_sorted <- integer(0)
  while (any(pending)) {
    i <- which(pending)[colSums(m[, pending, drop = FALSE]) == 0]
    if (length(i) > 0L) {
      graph_sorted <- c(graph_sorted, i)
      pending[i] <- FALSE
      m[i, ] <- FALSE
    } else {
      f <- function(i) {
        sprintf("\t%s: depends on %s",
                names(graph)[[i]], paste(err[m[pending, i]], collapse = ", "))
      }
      err <- names(graph)[pending]
      detail <- paste(vcapply(which(pending), f), collapse = "\n")
      stop(sprintf("A cyclic dependency detected for %s:\n%s",
                   paste(names(graph)[pending], collapse = ", "),
                   detail), call. = FALSE)
    }
  }

  names(graph)[graph_sorted]
}
