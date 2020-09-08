library(shinytest)

test_that("Specification of custom priors", {
  skip_on_cran()
  skip_if_not_installed("MASS")
  
  shinytest::installDependencies()
  app_path <- system.file("shinybrms_app", package = "shinybrms")
  expect_pass(testApp(app_path, testnames = "priors.R", compareImages = FALSE))
})
