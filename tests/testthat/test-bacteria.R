library(shinytest)

test_that("Preparation of the Stan run for the \"bacteria\" example from the \"shinybrms\" vignette", {
  skip_on_cran()
  skip_if_not_installed("MASS")
  
  app_path <- system.file("shinybrms_app", package = "shinybrms")
  expect_pass(testApp(app_path, testnames = "bacteria-prep.R", compareImages = FALSE))
})
