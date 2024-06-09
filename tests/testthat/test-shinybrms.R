library(shinytest2)

test_that("shinybrms_app works", {
  skip_on_cran()
  test_app(system.file(package = "shinybrms", "shinybrms_app"))
})
