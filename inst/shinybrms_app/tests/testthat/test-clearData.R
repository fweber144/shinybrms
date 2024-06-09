library(shinytest2)

test_that("clearing the example dataset resets the priors, the selected outcome, and the selected predictors.", {
  skip_on_cran()
  skip_if_not_installed("MASS")
  
  app <- AppDriver$new(expect_values_screenshot_args = FALSE)
  
  app$set_inputs(advOpts_cores = 2, wait_ = FALSE)
  
  app$set_inputs(navbar_ID = "Data")
  app$set_inputs(ex_da_sel = "bacteria")
  app$set_inputs(navbar_ID = "Likelihood")
  app$set_inputs(outc_sel = "y")
  app$set_inputs(dist_sel = "bernoulli")
  app$set_inputs(likelihood_navlist_ID = "Predictors")
  app$set_inputs(pred_mainPL_sel = c("week", "trt"))
  app$set_inputs(pred_mainGL_sel = "ID")
  app$set_inputs(pred_int_build = c("week", "trt"))
  app$set_inputs(pred_int_add = "click")
  app$set_inputs(navbar_ID = "Prior")
  app$set_inputs(prior_class_sel = "b")
  app$set_inputs(prior_text = "student_t(3, 0, 10)")
  app$set_inputs(prior_add = "click")
  app$expect_values()
  # A test for the bugs fixed by commits 32e2ca0b56124ee84ea56bcd21425e22425ad37c and
  # 01e0e1a9140c5d393b0e7117b2cb866660eba0a4:
  app$set_inputs(navbar_ID = "Data")
  app$set_inputs(ex_da_sel = "")
  app$set_inputs(navbar_ID = "Prior")
  app$set_inputs(navbar_ID = "Likelihood")
  app$set_inputs(likelihood_navlist_ID = "Outcome")
  app$expect_values()
  
  app$stop()
})
