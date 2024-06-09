library(shinytest2)

test_that(paste(
  "switching to a dataset which differs from the initial dataset only in some values of the variables",
  "causes the default priors to be recomputed correctly and the custom priors to be reset."
), {
  skip_on_cran()
  
  app <- AppDriver$new(expect_values_screenshot_args = FALSE)
  
  app$set_inputs(advOpts_cores = 2, wait_ = FALSE)
  
  app$set_inputs(navbar_ID = "Data")
  app$upload_file(data_upload = "switchData-data.csv")
  app$set_inputs(navbar_ID = "Likelihood")
  app$set_inputs(outc_sel = "y")
  app$set_inputs(dist_sel = "bernoulli")
  app$set_inputs(likelihood_navlist_ID = "Predictors")
  app$set_inputs(pred_mainPL_sel = "x1")
  app$set_inputs(navbar_ID = "Prior")
  app$set_inputs(prior_class_sel = "b")
  app$set_inputs(prior_text = "normal(0, 2.5)")
  app$set_inputs(prior_add = "click")
  app$expect_values(input = setdiff(lapply(app$get_values(), names)$input, "data_upload"),
                    output = TRUE,
                    export = TRUE)
  app$set_inputs(navbar_ID = "Data")
  app$upload_file(data_upload = "switchData_value-data.csv")
  app$expect_values(input = setdiff(lapply(app$get_values(), names)$input, "data_upload"),
                    output = TRUE,
                    export = TRUE)
  app$set_inputs(navbar_ID = "Prior")
  app$expect_values(input = setdiff(lapply(app$get_values(), names)$input, "data_upload"),
                    output = TRUE,
                    export = TRUE)
  app$set_inputs(navbar_ID = "Likelihood")
  app$set_inputs(likelihood_navlist_ID = "Outcome")
  app$expect_values(input = setdiff(lapply(app$get_values(), names)$input, "data_upload"),
                    output = TRUE,
                    export = TRUE)
  
  app$stop()
})
