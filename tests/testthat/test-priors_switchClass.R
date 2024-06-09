library(shinytest2)

test_that("Migrated shinytest test: priors_switchClass.R", {
  app <- AppDriver$new()

  app$set_inputs(advOpts_cores = 2, wait_ = FALSE)

  app$set_inputs(navbar_ID = "Data", ex_da_sel = "bacteria")
  app$set_inputs(navbar_ID = "Likelihood", outc_sel = "y", dist_sel = "bernoulli")
  app$set_inputs(likelihood_navlist_ID = "Predictors", pred_mainGL_sel = "ID")
  app$set_inputs(navbar_ID = "Prior", prior_class_sel = "sd")
  app$set_inputs(prior_group_sel = "ID")
  app$set_inputs(prior_text = "normal(0, 2.5, values_ = FALSE)",
    prior_add = "click")
  app$expect_values()
  app$set_inputs(prior_class_sel = "Intercept")
  app$set_inputs(prior_text = "normal(0, 4, values_ = FALSE)",
    prior_add = "click")
  app$expect_values()
})
