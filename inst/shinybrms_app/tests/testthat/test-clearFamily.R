library(shinytest2)

test_that("clearing the distributional family resets the priors.", {
  skip_on_cran()
  skip_if_not_installed("MASS")
  
  app <- AppDriver$new()
  
  app$set_inputs(advOpts_cores = 2, wait_ = FALSE)
  
  app$set_inputs(navbar_ID = "Data",
                 ex_da_sel = "bacteria")
  app$set_inputs(navbar_ID = "Likelihood",
                 outc_sel = "y",
                 dist_sel = "bernoulli")
  app$set_inputs(navbar_ID = "Prior",
                 prior_class_sel = "Intercept",
                 prior_text = "student_t(3, 0, 3)",
                 prior_add = "click")
  app$expect_values()
  app$set_inputs(navbar_ID = "Likelihood",
                 dist_sel = "")
  app$set_inputs(navbar_ID = "Prior")
  app$expect_values()
})
