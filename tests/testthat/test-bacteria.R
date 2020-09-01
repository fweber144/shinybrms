library(shinytest)

test_that("the Stan run for the \"bacteria\" example from the \"shinybrms\" vignette is prepared as expected", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("_R_CHECK_CRAN_INCOMING_"), ""))
  skip_if_not_installed("MASS")
  
  app_path <- system.file("shinybrms_app", package = "shinybrms")
  expect_pass(testApp(app_path, testnames = "bacteria-prep.R", compareImages = FALSE))
})
