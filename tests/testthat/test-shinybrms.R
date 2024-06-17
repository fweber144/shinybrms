library(shinytest2)

test_that("shinybrms passes all shinytest2 tests", {
  skip_on_cran()
  shinytest2::test_app(system.file("shinybrms_app", package = "shinybrms"))
  expect_true(TRUE)
})
