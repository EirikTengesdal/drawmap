testthat::test_that("specifying font family and font size works", {
  expect_equal(theme_map(font = "Minion 3", size = 12)$text$family, "Minion 3")
  expect_equal(theme_map(font = "Minion 3", size = 12)$text$size, 12)
})
