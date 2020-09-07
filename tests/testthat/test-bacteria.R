library(shinytest)

test_that("Preparation of the Stan run for the \"bacteria\" example from the \"shinybrms\" vignette", {
  skip_on_cran()
  skip_if_not_installed("MASS")
  
  shinytest::installDependencies()
  app_path <- system.file("shinybrms_app", package = "shinybrms")
  expect_pass(testApp(app_path, testnames = "bacteria-prep.R", compareImages = FALSE))
})

test_that("Results of the Stan run for the \"bacteria\" example from the \"shinybrms\" vignette on Windows", {
  skip_on_cran()
  skip_on_ci() # Safer than 'skip_on_travis()'.
  skip_on_covr()
  skip_if_not_installed("MASS")
  skip_if_not(identical(.Platform$OS.type, "windows")) # Shorter and safer than 'skip_on_os("linux"); skip_on_os("mac"); skip_on_os("solaris")'.
  
  shinytest::installDependencies()
  app_path <- system.file("shinybrms_app", package = "shinybrms")
  expect_pass(testApp(app_path, testnames = "bacteria-run-windows.R", compareImages = FALSE))
})
