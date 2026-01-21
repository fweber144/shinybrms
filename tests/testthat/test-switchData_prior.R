library(shinytest2)

test_that(paste(
  "the app does not crash and the default priors are reset when switching the dataset after",
  "having built a working model for the initial dataset."
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
  app$set_inputs(navbar_ID = "Prior")
  app$expect_values()
  app$set_inputs(navbar_ID = "Data")
  app$set_inputs(ex_da_sel = "bacteria")
  app$set_inputs(navbar_ID = "Prior")
  app$wait_for_idle()
  app$set_inputs(navbar_ID = "Data")
  app$expect_values()
  app$set_inputs(navbar_ID = "Prior")
  app$expect_values()
  
  app$stop()
})
