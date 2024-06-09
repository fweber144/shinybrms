library(shinytest2)

test_that("Migrated shinytest test: priors_autoSetEmpty.R", {
  app <- AppDriver$new()

  app$set_inputs(advOpts_cores = 2, wait_ = FALSE)

  app$set_inputs(navbar_ID = "Data", ex_da_sel = "bacteria")
  app$set_inputs(navbar_ID = "Likelihood", outc_sel = "y", dist_sel = "bernoulli")
  app$set_inputs(likelihood_navlist_ID = "Predictors", pred_mainPL_sel = "week",
    pred_mainGL_sel = "ID")
  app$set_inputs(pred_int_build = c("week", "ID"), pred_int_add = "click")
  app$set_inputs(navbar_ID = "Prior", prior_class_sel = "sd")
  app$set_inputs(prior_group_sel = "ID")
  app$expect_values()
  # A test for the bug fixed by commit e484a6fdff61fa9eb60ae67a9b0a8bdcc932502c:
  app$set_inputs(prior_class_sel = "cor")
  app$expect_values()
})
