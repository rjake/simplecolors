context("test-sc")

test_that("sc accepts multiple input", {
  x <- sc("brightblue2", "mutedpink4")
  expect_equal(length(x), 2)
})

test_that("sc accepts default names", {
  x <- sc("blue", "pink")
  expect_equal(length(x), 2)
  expect_equal(x, sc("blue3", "pink3"))
})


test_that("sc_as_list() works", {
  x <- sc_as_list("blue", "pink")
  expect_true(is.list(x))
  expect_equal(names(x), c("blue", "pink"))
})
