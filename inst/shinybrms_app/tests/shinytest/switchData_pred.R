app <- ShinyDriver$new("../../")
app$snapshotInit("switchData_pred", screenshot = FALSE)

app$setInputs(navbar_ID = "Data")
app$uploadFile(file_upload = "switchData_pred-data.csv")
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "column1",
              dist_sel = "bernoulli")
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_mainNP_sel = "column2")
app$setInputs(navbar_ID = "Data")
app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "file_upload"),
                          output = TRUE,
                          export = TRUE))
# A test for the bug fixed by commit 490a653dce15fd7351586097b6724e9aa6d71f4b:
app$uploadFile(file_upload = "switchData_pred-data_mod.csv")
app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "file_upload"),
                          output = TRUE,
                          export = TRUE))
app$setInputs(navbar_ID = "Likelihood")
app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "file_upload"),
                          output = TRUE,
                          export = TRUE))
