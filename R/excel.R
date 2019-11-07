cell_nearby <- function(r, w, offset) {
  r$row <- r$row + offset$row %||% 0L
  r$col <- r$col + offset$col %||% 0L
  cell_value(r, w)
}


cell_value <- function(r, w) {
  ret <- vector("list", nrow(r))
  for (i in unique(r$sheet)) {
    s <- w$sheets[[i]]
    j <- r$sheet == i
    k <- s$lookup[cbind(r$row[j], r$col[j])]
    ret[j] <- s$cells$value[k]
  }
  ret
}
