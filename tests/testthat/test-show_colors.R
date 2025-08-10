context("test-show_colors")

test_that("show_colors retuns gglot", {
  x <- show_colors(labels = TRUE)
  expect_true(ggplot2::is_ggplot(x))
  expect_equal(length(x$layers), 3)
})
