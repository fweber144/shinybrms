library(shinytest2)

test_that("clearing the outcome resets the priors.", {
  skip_on_cran()
  skip_if_not_installed("MASS")
  
  app <- AppDriver$new(expect_values_screenshot_args = FALSE)
  
  app$set_inputs(advOpts_cores = 2, wait_ = FALSE)
  
  app$set_inputs(navbar_ID = "Data")
  app$set_inputs(ex_da_sel = "bacteria")
  app$set_inputs(navbar_ID = "Likelihood")
  app$set_inputs(outc_sel = "y")
  app$set_inputs(dist_sel = "bernoulli")
  app$set_inputs(navbar_ID = "Prior")
  app$set_inputs(prior_class_sel = "Intercept")
  app$set_inputs(prior_text = "student_t(3, 0, 3)")
  app$set_inputs(prior_add = "click")
  app$expect_values()
  # A test for the bug fixed by commit 567d670f9ba478411aed758f1281f6b1e72ab417:
  app$set_inputs(navbar_ID = "Likelihood")
  app$set_inputs(outc_sel = "")
  app$set_inputs(navbar_ID = "Prior")
  app$expect_values()
  
  app$stop()
})
