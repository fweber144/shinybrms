library(shinytest2)

test_that("Migrated shinytest test: offsets.R", {
  app <- AppDriver$new()

  app$set_inputs(advOpts_cores = 2, wait_ = FALSE)

  app$set_inputs(navbar_ID = "Data", ex_da_sel = "epilepsy")
  app$set_inputs(navbar_ID = "Likelihood", outc_sel = "count",
    dist_sel = "negbinomial")
  app$set_inputs(likelihood_navlist_ID = "Predictors", pred_mainPL_sel = c("Trt",
    "zAge"), pred_mainGL_sel = c("patient", "visit"))
  app$set_inputs(pred_int_build = c("Trt", "zAge"), pred_int_add = "click")
  app$set_inputs(pred_int_build = c("Trt", "visit"), pred_int_add = "click")
  app$set_inputs(offs_sel = c("zBase", "Base"))
  app$set_inputs(likelihood_navlist_ID = "Formula preview")
  app$set_inputs(navbar_ID = "Prior")
  app$expect_values()
  app$set_inputs(navbar_ID = "Data", ex_da_sel = "")
  app$set_inputs(navbar_ID = "Likelihood")
  app$set_inputs(likelihood_navlist_ID = "Predictors")
  app$set_inputs(likelihood_navlist_ID = "Outcome")
  app$set_inputs(navbar_ID = "Prior")
  app$expect_values()
})
