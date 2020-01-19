context("test-sc_within")

test_that("sc_within defaults to vector", {
  x <- sc_within(
    hue = "red",
    light = 1:2
  )
  expect_equal(class(x), "character")
  expect_equal(length(x), 2)
})


test_that("sc_within returns ggplot", {
  x <- sc_within(
    hue = "red",
    light = 1:2,
    return = "plot"
  )
  expect_equal(is(x), "gg")

})


test_that("sc_within returns table", {
  x <- sc_within(
    hue = "red",
    light = 1:2,
    return = "table"
  )
  expect_true(is.data.frame(x))
})


test_that("sc_* match sc_within", {
  expect_equal(sc_red(), sc_within("red"))
  expect_equal(sc_orange(), sc_within("orange"))
  expect_equal(sc_yellow(), sc_within("yellow"))
  expect_equal(sc_green(), sc_within("green"))
  expect_equal(sc_teal(), sc_within("teal"))
  expect_equal(sc_blue(), sc_within("blue"))
  expect_equal(sc_violet(), sc_within("violet"))
  expect_equal(sc_pink(), sc_within("pink"))
  expect_equal(sc_grey(), sc_within("grey"))
})
