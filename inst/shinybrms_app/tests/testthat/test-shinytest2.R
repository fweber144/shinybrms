library(shinytest2)

test_that("Initial Shiny values are consistent", {
  app <- AppDriver$new(expect_values_screenshot_args = FALSE)
  
  app$expect_values()
  
  app$stop()
})
