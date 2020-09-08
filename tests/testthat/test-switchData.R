library(shinytest)

test_that(
  paste("the app does not crash when switching the dataset after having selected outcome and",
        "distributional family (but no predictors) for the older dataset."),
  {
    skip_on_cran()
    skip_if_not_installed("lme4")
    skip_if_not_installed("MASS")
    
    shinytest::installDependencies()
    app_path <- system.file("shinybrms_app", package = "shinybrms")
    expect_pass(testApp(app_path, testnames = "switchData_outcome.R", compareImages = FALSE))
  }
)

test_that(
  paste("the app does not crash when switching to a dataset which differs from the older",
        "dataset only in the name of a predictor variable."),
  {
    skip_on_cran()
    
    shinytest::installDependencies()
    app_path <- system.file("shinybrms_app", package = "shinybrms")
    expect_pass(testApp(app_path, testnames = "switchData_pred.R", compareImages = FALSE))
  }
)
