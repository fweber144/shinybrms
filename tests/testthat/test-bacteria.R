library(shinytest)

test_that("Results of the Stan run for the \"bacteria\" example from the \"shinybrms\" vignette", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  skip_if_not_installed("MASS")
  skip_if_not(identical(.Platform$OS.type, "unix"))
  
  shinytest::installDependencies()
  app_path <- system.file("shinybrms_app", package = "shinybrms")
  # Suffix for the OS (one could simply use shinytest::osName() instead, but
  # that would require changing the names of the folders for the expected test
  # results):
  if (identical(Sys.info()["sysname"], setNames("Linux", "sysname"))) {
    os_suffix <- "linux"
  } else {
    os_suffix <- .Platform$OS.type
  }
  shinytest::expect_pass(shinytest::testApp(app_path, testnames = "bacteria_run.R", suffix = os_suffix))
})
