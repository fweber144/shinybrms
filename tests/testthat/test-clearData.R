library(shinytest2)

test_that("Migrated shinytest test: clearData.R", {
  app <- AppDriver$new()

  app$set_inputs(advOpts_cores = 2, wait_ = FALSE)

  app$set_inputs(navbar_ID = "Data", ex_da_sel = "bacteria")
  app$set_inputs(navbar_ID = "Likelihood", outc_sel = "y", dist_sel = "bernoulli")
  app$set_inputs(likelihood_navlist_ID = "Predictors", pred_mainPL_sel = c("week",
    "trt"), pred_mainGL_sel = "ID")
  app$set_inputs(pred_int_build = c("week", "trt"), pred_int_add = "click")
  app$set_inputs(navbar_ID = "Prior", prior_class_sel = "b", prior_text = "student_t(3, 0, 10)",
    prior_add = "click")
  app$expect_values()
  # A test for the bugs fixed by commits 32e2ca0b56124ee84ea56bcd21425e22425ad37c and
  # 01e0e1a9140c5d393b0e7117b2cb866660eba0a4:
  app$set_inputs(navbar_ID = "Data", ex_da_sel = "")
  app$set_inputs(navbar_ID = "Prior")
  app$set_inputs(navbar_ID = "Likelihood")
  app$set_inputs(likelihood_navlist_ID = "Outcome")
  app$expect_values()
})
