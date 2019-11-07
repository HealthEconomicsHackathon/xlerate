context("eval")


test_that("missing value behaviour correct for missing values", {
  expect_equal(eval(quote(NA + 1), EXCEL), 1)
  expect_equal(eval(quote(1 - NA), EXCEL), 1)
  expect_equal(eval(quote(1 * NA), EXCEL), 0)

  expect_equal(na_zero(NA), 0)
  expect_equal(na_zero(1), 1)
})


test_that("consistency of calculations", {
  exprs <- list(A1 = list(value = 1, name = "A1"),
                A2 = list(value = 2, name = "A2"),
                A3 = list(value = 4, name = "A3",
                          formula = quote(A1 + A2)))
  expect_error(compute(exprs, check = TRUE),
               "Found inconsistency in calculation")
  res <- compute(exprs, check = FALSE)
  expect_equal(res$A3, 3)
})


test_that("verbose", {
  exprs <- list(A1 = list(value = 1, name = "A1"),
                A2 = list(value = 2, name = "A2"),
                A3 = list(value = 3, name = "A3",
                          formula = quote(A1 + A2)))
  expect_message(
    compute(exprs, verbose = TRUE),
    "A3: A1 + A2 => 3",
    fixed = TRUE)
})
