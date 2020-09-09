library(shinytest)

test_that("Specification of custom priors", {
  skip_on_cran()
  skip_if_not_installed("MASS")
  
  shinytest::installDependencies()
  app_path <- system.file("shinybrms_app", package = "shinybrms")
  expect_pass(testApp(app_path, testnames = "priors.R", compareImages = FALSE))
})

test_that("the group for the prior is not automatically set to be empty when not necessary.", {
  skip_on_cran()
  skip_if_not_installed("MASS")
  
  shinytest::installDependencies()
  app_path <- system.file("shinybrms_app", package = "shinybrms")
  expect_pass(testApp(app_path, testnames = "priors_autoSetEmpty.R", compareImages = FALSE))
})
