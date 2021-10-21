library(shinytest)

# Note: In fact, it would be more desirable to specify argument `suffix` in the
# shinytest::testApp() calls below (and to have a single file
# "brmsfit_upload.R"), but this has the downside that the shinytests then cannot
# be run directly via RStudio's "Run Tests" button (in an opened shinytest
# file).

test_that("Upload of an existing `brmsfit` for the \"bacteria\" example on Linux", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  skip_if_not_installed("MASS")
  skip_if_not(identical(.Platform$OS.type, "unix"))
  
  shinytest::installDependencies()
  app_path <- system.file("shinybrms_app", package = "shinybrms")
  shinytest::expect_pass(shinytest::testApp(app_path, testnames = "brmsfit_upload_run_linux.R"))
})

test_that("Upload of an existing `brmsfit` for the \"bacteria\" example on Windows", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  skip_if_not_installed("MASS")
  skip_if_not(identical(.Platform$OS.type, "windows"))
  
  shinytest::installDependencies()
  app_path <- system.file("shinybrms_app", package = "shinybrms")
  shinytest::expect_pass(shinytest::testApp(app_path, testnames = "brmsfit_upload_run_windows.R"))
})
