library(shinytest2)

test_that("Migrated shinytest test: priors.R", {
  app <- AppDriver$new()
  
  app$set_inputs(advOpts_cores = 2, wait_ = FALSE)
  
  app$set_inputs(navbar_ID = "Data", ex_da_sel = "bacteria")
  app$set_inputs(navbar_ID = "Likelihood", outc_sel = "y", dist_sel = "bernoulli")
  app$set_inputs(likelihood_navlist_ID = "Predictors")
  app$set_inputs(pred_mainPL_sel = c("week", "trt"), pred_mainGL_sel = "ID")
  app$set_inputs(pred_int_build = c("week", "trt"), pred_int_add = "click")
  app$set_inputs(navbar_ID = "Prior")
  app$expect_values()
  # A test for the bug fixed by commit 4a03135b045b092c91b8d5939bea9965a6b9dc2d:
  app$set_inputs(prior_class_sel = "sd")
  app$set_inputs(prior_coef_sel = "Intercept")
  app$expect_values()
  app$set_inputs(prior_text = "student_t(3, 0, 3)", prior_add = "click")
  app$expect_values()
  # A test for the bug fixed by commit 11d32ca228f913f2d55dfbce6735a34e81d89639:
  app$set_inputs(prior_coef_sel = "")
  app$set_inputs(prior_text = "student_t(3, 0, 4)", prior_add = "click")
  app$expect_values()
  # The following two lines effectively set app$setInputs(prior_group_sel = "", values_ = FALSE):
  app$set_inputs(prior_class_sel = "")
  app$set_inputs(prior_class_sel = "sd")
  app$set_inputs(prior_text = "student_t(3, 0, 5)", prior_add = "click")
  app$expect_values()
  app$set_inputs(prior_reset = "click")
  app$expect_values()
  # A test for the bug fixed by commit 5fd4cf8dae2fe6399c2fe4d1a57c8632cc7db5ff:
  app$set_inputs(prior_coef_sel = "Intercept")
  app$set_inputs(prior_group_sel = "")
  app$set_inputs(prior_text = "student_t(3, 0, 6)", prior_add = "click")
  app$expect_values()
})
