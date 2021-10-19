library(shinytest)

test_that("Specification of offsets", {
  skip_on_cran()
  
  shinytest::installDependencies()
  app_path <- system.file("shinybrms_app", package = "shinybrms")
  shinytest::expect_pass(shinytest::testApp(app_path, testnames = "offsets.R"))
})
