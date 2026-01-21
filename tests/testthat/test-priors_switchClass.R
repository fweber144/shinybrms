library(shinytest2)

test_that("if necessary, the group for the prior is automatically reset when switching the class for the prior.", {
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
  app$set_inputs(pred_mainGL_sel = "ID")
  app$set_inputs(navbar_ID = "Prior")
  app$set_inputs(prior_class_sel = "sd")
  app$set_inputs(prior_group_sel = "ID")
  app$set_inputs(prior_text = "normal(0, 2.5)")
  app$set_inputs(prior_add = "click")
  app$expect_values()
  app$set_inputs(prior_class_sel = "Intercept")
  app$set_inputs(prior_text = "normal(0, 4)")
  app$set_inputs(prior_add = "click")
  app$expect_values()
  
  app$stop()
})
