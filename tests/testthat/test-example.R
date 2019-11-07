context("example")


test_that("Example works", {
  path <- xlerate_file("example/tree.xlsx")

  inputs <- xlerate_ref(c("D3:D13", "D15:D16", "D18:D21"),
                        1, list(col = -1))
  outputs <- xlerate_ref(c("C40", "E34", "E50", "G29", "G38", "G45", "G54"),
                         2, list(row = 2))

  tree <- xlerate(path, inputs, outputs)
  res1 <- tree(NULL)
  res2 <- tree(c("pTST_pos" = 0.01))

  expect_equal(res1[["TST"]], 68.4475683453238)
  expect_equal(res2[["TST"]], 71.9069064748202)

  x <- vnapply(attr(tree, "input"), identity, USE.NAMES = FALSE)
  expect_identical(tree(x), res1)
  expect_identical(tree(c(0.01, x[-1])), res2)

  expect_output(print(tree), "<an xlerate object>")
})


test_that("markov example", {
  path <- xlerate_file("example/markov.xlsx")
  inputs <- xlerate_ref("B1:B10", 1, list(col = -1))
  outputs <- xlerate_ref(c("I25", "O24", "U25", "AA24"), 1, list(col = -1))
  markov <- xlerate(path, inputs, outputs)

  res1 <- markov(NULL)
  res2 <- markov(c("Prob state 1 >2" = 0.1))

  expect_equal(res1[["Lif years standard"]], 1021.06401534845)
  expect_equal(res2[["Lif years standard"]], 1028.93901534845)
})
