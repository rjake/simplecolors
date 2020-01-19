context("test-show_colors")

test_that("show_colors retuns gglot", {
  x <- show_colors(labels = TRUE)
  expect_equal(is(x), "gg")
  expect_equal(length(x$layers), 3)
})
