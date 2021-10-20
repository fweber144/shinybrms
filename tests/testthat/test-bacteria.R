library(shinytest)

test_that("Results of the Stan run for the \"bacteria\" example from the \"shinybrms\" vignette on Linux", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  skip_if_not_installed("MASS")
  skip_if_not(identical(.Platform$OS.type, "unix"))
  
  shinytest::installDependencies()
  app_path <- system.file("shinybrms_app", package = "shinybrms")
  shinytest::expect_pass(shinytest::testApp(app_path, testnames = "bacteria_run_linux.R"))
})

test_that("Results of the Stan run for the \"bacteria\" example from the \"shinybrms\" vignette on Windows", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  skip_if_not_installed("MASS")
  skip_if_not(identical(.Platform$OS.type, "windows"))
  
  shinytest::installDependencies()
  app_path <- system.file("shinybrms_app", package = "shinybrms")
  shinytest::expect_pass(shinytest::testApp(app_path, testnames = "bacteria_run_windows.R"))
})
