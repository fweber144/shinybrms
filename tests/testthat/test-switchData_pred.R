library(shinytest2)

test_that("Migrated shinytest test: switchData_pred.R", {
  app <- AppDriver$new()

  app$set_inputs(advOpts_cores = 2, wait_ = FALSE)

  app$set_inputs(navbar_ID = "Data")
  # app$upload_file(data_upload = "switchData-data.csv")
  app$set_inputs(navbar_ID = "Likelihood", outc_sel = "y", dist_sel = "bernoulli")
  app$set_inputs(likelihood_navlist_ID = "Predictors", pred_mainPL_sel = "x1")
  app$set_inputs(navbar_ID = "Data")
  app$expect_values(input = setdiff(lapply(app$get_values(), names)$input,
    "data_upload"), output = TRUE, export = TRUE)
  # A test for the bug fixed by commit 490a653dce15fd7351586097b6724e9aa6d71f4b:
  # app$upload_file(data_upload = "switchData_pred-data.csv")
  app$expect_values(input = setdiff(lapply(app$get_values(), names)$input,
    "data_upload"), output = TRUE, export = TRUE)
  app$set_inputs(navbar_ID = "Likelihood")
  app$expect_values(input = setdiff(lapply(app$get_values(), names)$input,
    "data_upload"), output = TRUE, export = TRUE)
})
