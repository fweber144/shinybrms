library(shinytest)

test_that(paste("The app does not crash when switching the dataset after having selected outcome and",
                "distributional family (but no predictors) for the older dataset."), {
  skip_on_cran()
  skip_if_not_installed("lme4")
  skip_if_not_installed("MASS")
  
  shinytest::installDependencies()
  app_path <- system.file("shinybrms_app", package = "shinybrms")
  expect_pass(testApp(app_path, testnames = "bugs_switchData_outcome.R", compareImages = FALSE))
})
