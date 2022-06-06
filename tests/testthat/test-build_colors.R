context("test-build_colors")

test_that("build_colors returns table", {
  x <- build_colors() %>% suppressWarnings()
  expect_equal(dim(x), c(176, 15))
})
