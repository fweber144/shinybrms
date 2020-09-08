library(shinytest)

test_that("Specification of interaction terms", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  
  shinytest::installDependencies()
  app_path <- system.file("shinybrms_app", package = "shinybrms")
  expect_pass(testApp(app_path, testnames = "interactions.R", compareImages = FALSE))
})