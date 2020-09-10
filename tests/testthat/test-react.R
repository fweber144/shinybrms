library(shinytest)

test_that(
  paste("the app does not crash when switching the dataset after having selected",
        "outcome and distributional family (but no predictors) for the former dataset."),
  {
    skip_on_cran()
    skip_if_not_installed("lme4")
    skip_if_not_installed("MASS")
    
    shinytest::installDependencies()
    app_path <- system.file("shinybrms_app", package = "shinybrms")
    expect_pass(testApp(app_path, testnames = "switchData_outcome.R"))
  }
)

test_that(
  paste("the app does not crash when switching to a dataset which differs from",
        "the former dataset only in the name of a predictor variable."),
  {
    skip_on_cran()
    
    shinytest::installDependencies()
    app_path <- system.file("shinybrms_app", package = "shinybrms")
    expect_pass(testApp(app_path, testnames = "switchData_pred.R"))
  }
)

test_that("uploading another dataset (here: the same dataset again) resets the custom priors.", {
  skip_on_cran()
  
  shinytest::installDependencies()
  app_path <- system.file("shinybrms_app", package = "shinybrms")
  expect_pass(testApp(app_path, testnames = "switchData_same.R"))
})

test_that(
  paste("switching to a dataset which differs from the former dataset only in some values of the variables",
        "causes the default priors to be recomputed correctly and the custom priors to be reset."),
  {
    skip_on_cran()
    
    shinytest::installDependencies()
    app_path <- system.file("shinybrms_app", package = "shinybrms")
    expect_pass(testApp(app_path, testnames = "switchData_value.R"))
  }
)

test_that("clearing the outcome resets the priors.", {
  skip_on_cran()
  skip_if_not_installed("MASS")
  
  shinytest::installDependencies()
  app_path <- system.file("shinybrms_app", package = "shinybrms")
  expect_pass(testApp(app_path, testnames = "clearOutcome.R"))
})

test_that("clearing the distributional family resets the priors.", {
  skip_on_cran()
  skip_if_not_installed("MASS")
  
  shinytest::installDependencies()
  app_path <- system.file("shinybrms_app", package = "shinybrms")
  expect_pass(testApp(app_path, testnames = "clearFamily.R"))
})

test_that("clearing the example dataset resets the priors, the selected outcome, and the selected predictors.", {
  skip_on_cran()
  skip_if_not_installed("MASS")
  
  shinytest::installDependencies()
  app_path <- system.file("shinybrms_app", package = "shinybrms")
  expect_pass(testApp(app_path, testnames = "clearData.R"))
})
