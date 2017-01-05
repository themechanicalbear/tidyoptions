context("tidy_options")

test_that("tidy_options creates expected data.frame", {
  tidy_options("XLE", "data/raw_files", "data/volatility/vx.xle.daily.prices.RData", "output/")
  load("output/XLE.options.RData")
  load("data/options/test.options.RData")
  expect_equal(complete.data, test.data)

})