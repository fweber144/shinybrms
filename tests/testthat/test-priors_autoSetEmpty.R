library(shinytest2)

test_that("the group for the prior is not automatically set to be empty when not necessary.", {
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
  app$set_inputs(pred_mainPL_sel = "week")
  app$set_inputs(pred_mainGL_sel = "ID")
  app$set_inputs(pred_int_build = c("week", "ID"))
  app$set_inputs(pred_int_add = "click")
  app$set_inputs(navbar_ID = "Prior")
  app$set_inputs(prior_class_sel = "sd")
  app$set_inputs(prior_group_sel = "ID")
  app$expect_values()
  # A test for the bug fixed by commit e484a6fdff61fa9eb60ae67a9b0a8bdcc932502c:
  app$set_inputs(prior_class_sel = "cor")
  app$expect_values()
  
  app$stop()
})
