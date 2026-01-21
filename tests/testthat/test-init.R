library(shinytest2)

test_that("initial Shiny values are consistent", {
  skip_on_cran()
  appdir <- system.file(package = "shinybrms", "shinybrms_app")
  local_app_support(appdir)
  app <- AppDriver$new(appdir, name = "init",
                       expect_values_screenshot_args = FALSE)
  
  app$expect_values()
  
  app$stop()
})
