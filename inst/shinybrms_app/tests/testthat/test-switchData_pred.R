library(shinytest2)

test_that(paste(
  "the app does not crash when switching to a dataset which differs from",
  "the initial dataset only in the name of a predictor variable."
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
  app$set_inputs(navbar_ID = "Data")
  app$expect_values(input = setdiff(lapply(app$get_values(), names)$input, "data_upload"),
                    output = TRUE,
                    export = TRUE)
  # A test for the bug fixed by commit 490a653dce15fd7351586097b6724e9aa6d71f4b:
  app$upload_file(data_upload = "switchData_pred-data.csv")
  app$expect_values(input = setdiff(lapply(app$get_values(), names)$input, "data_upload"),
                    output = TRUE,
                    export = TRUE)
  app$set_inputs(navbar_ID = "Likelihood")
  app$expect_values(input = setdiff(lapply(app$get_values(), names)$input, "data_upload"),
                    output = TRUE,
                    export = TRUE)
  
  app$stop()
})
