RE_REF <- "(([A-Za-z]+!)?[A-Z]+[0-9]+)"
RE_REF_SHEET <-  "(.*)!.*"

## Need a simple parse - this needs a lot of work, but converts the
## cell references into valid R variables first.  That's needed for
## the cases that are references into other sheets.
parse_formula <- function(x) {
  if (identical(x, NA) || identical(x, "") || identical(x, NA_character_)) {
    return(NULL)
  }
  ## TODO: this is not enough - we should do this much better and as
  ## it is it will not cope with absolute references
  stopifnot(length(x) == 1)
  x <- gsub("$", "", x, fixed = TRUE)
  x <- gsub(RE_REF, "`\\1`", x)
  expr <- parse(text = x)[[1]]

  if (":" %in% all.names(expr)) {
    expr <- rewrite_ranges(expr)
  }

  expr
}


rewrite_ranges <- function(expr) {
  if (is.recursive(expr)) {
    if (identical(expr[[1]], quote(`:`))) {
      range <- sprintf("%s:%s", deparse(expr[[2]]), deparse(expr[[3]]))
      limits <- cellranger::as.cell_limits(range)
      row <- limits$ul[[1]]:limits$lr[[1]]
      col <- cellranger::num_to_letter(limits$ul[[2]]:limits$lr[[2]])
      cells <- expand.grid(row = row, col = col, stringsAsFactors = FALSE)
      expr <- as.call(c(list(quote(c)),
                        lapply(paste0(cells$col, cells$row), as.name)))
    } else {
      expr[] <- lapply(expr, rewrite_ranges)
    }
  }
  expr
}


cell_ref <- function(ref, w, default, check_range = FALSE) {
  if (check_range && any(grepl(":", ref))) {
    ## TODO: this is a hack, and has the bad effect of losing the
    ## shape of the references
    r <- ranges_to_coords(ref)
    ref <- paste0(cellranger::num_to_letter(r$col), r$row)
    i <- !is.na(r$sheet)
    ref[i] <- sprintf("%s!%s", r$sheet[i], ref)
  }
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
  ret$name_excel <- ret$ref

  ret
}


process <- function(outputs, w) {
  seen <- character(0)
  exprs <- list()

  output_names <- set_names(outputs$name, outputs$label)
  traverse <- outputs[!duplicated(outputs$name), , drop = FALSE]
  traverse$label <- NULL

  while (nrow(traverse) > 0) {
    x <- as.list(traverse[1, ])
    traverse <- traverse[-1, , drop = FALSE]
    seen <- c(seen, x$name)

    s <- w$sheets[[x$sheet]]
    f <- parse_formula(s$cells$formula[s$lookup[x$row, x$col]])

    value <- s$cells$value[s$lookup[x$row, x$col]]
    stopifnot(length(value) == 1)

    vars <- all.vars(f)
    if (length(vars) > 0L) {
      deps <- cell_ref(vars, w, x$sheet)
      subs <- set_names(lapply(deps$name, as.name), vars)
      f <- substitute_(f, subs)

      extra <- deps[!(deps$name %in% seen), , drop = FALSE]
      if (nrow(extra) > 0L) {
        traverse <- rbind(traverse, extra)
      }
    } else {
      deps <- NULL
    }
    res <- c(x, list(formula = f, value = value[[1]], deps = deps))
    exprs <- c(exprs, set_names(list(res), x$name))
  }

  i <- topological_order(lapply(exprs, function(x) x$deps$name))
  list(exprs = exprs[i],
       outputs = output_names)
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
    ## TODO: this is a terrible way of dealing with circular refs but
    ## they should not happen:
    stopifnot(length(i) > 0L)
    graph_sorted <- c(graph_sorted, i)
    pending[i] <- FALSE
    m[i, ] <- FALSE
  }

  names(graph)[graph_sorted]
}


range_to_coords <- function(x) {
  r <- cellranger::as.cell_limits(x)
  row <- r[[1]][1]:r[[2]][1]
  col <- r[[1]][2]:r[[2]][2]
  data.frame(row = row, col = col, sheet = r$sheet)
}


ranges_to_coords <- function(x) {
  do.call("rbind", lapply(x, range_to_coords))
}
