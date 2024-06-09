app <- ShinyDriver$new("../../")
app$snapshotInit("switchData_pred", screenshot = FALSE)

app$setInputs(advOpts_cores = 2, wait_ = FALSE, values_ = FALSE)

app$setInputs(navbar_ID = "Data", values_ = FALSE)
# app$upload_file(data_upload = "switchData-data.csv")
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "y",
              dist_sel = "bernoulli",
              values_ = FALSE)
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_mainPL_sel = "x1",
              values_ = FALSE)
app$setInputs(navbar_ID = "Data", values_ = FALSE)
app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "data_upload"),
                          output = TRUE,
                          export = TRUE))
# A test for the bug fixed by commit 490a653dce15fd7351586097b6724e9aa6d71f4b:
# app$upload_file(data_upload = "switchData_pred-data.csv")
app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "data_upload"),
                          output = TRUE,
                          export = TRUE))
app$setInputs(navbar_ID = "Likelihood", values_ = FALSE)
app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "data_upload"),
                          output = TRUE,
                          export = TRUE))
