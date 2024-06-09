library(shinytest2)

test_that(paste(
  "the app does not crash when switching the dataset after having selected",
  "outcome and distributional family (but no predictors) for the initial dataset."
), {
  skip_on_cran()
  skip_if_not_installed("lme4")
  skip_if_not_installed("MASS")
  
  app <- AppDriver$new(expect_values_screenshot_args = FALSE)
  
  app$set_inputs(advOpts_cores = 2, wait_ = FALSE)
  
  app$set_inputs(navbar_ID = "Data")
  app$set_inputs(ex_da_sel = "Arabidopsis")
  app$set_inputs(navbar_ID = "Likelihood")
  app$set_inputs(outc_sel = "total.fruits")
  app$set_inputs(dist_sel = "negbinomial")
  app$set_inputs(navbar_ID = "Data")
  app$expect_values()
  # A test for the bug fixed by commits cea0f49cc275636d4875c40f4410eb827bab9112 and
  # 69578f0170b4f4c48958bbf6f6fc1d33f7f7f626:
  app$set_inputs(ex_da_sel = "bacteria")
  app$expect_values()
  
  app$stop()
})
