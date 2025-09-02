context("test-sc_across")

test_that("sc_across defaults to vector", {
  x <- sc_across(palette = "BO")
  expect_equal(class(x), "character")
  expect_equal(length(x), 2)
})


test_that("sc_across returns ggplot", {
  x <- sc_across(
    palette = "BO",
    return = "plot"
  )
  expect_true(ggplot2::is_ggplot(x))
})

test_that("sc_across plot in right order", {
  x <- sc_across(
    palette = "RO",
    return = "plot"
  )
  gg <- ggplot2::ggplot_build(x)$data[[1]]

  # not alphabetical
  expect_equal(gg$fill, sc("red", "orange"))
})


test_that("sc_across returns table", {
  x <- sc_across(
    palette = "BO",
    return = "table"
  )
  expect_true(is.data.frame(x))
  expect_equal(x$color_name, c("blue3", "orange3"))
})
