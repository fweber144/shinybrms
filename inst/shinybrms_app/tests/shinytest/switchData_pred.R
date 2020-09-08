app <- ShinyDriver$new("../../")
app$snapshotInit("bugs_switchData_pred", screenshot = FALSE)

app$setInputs(navbar_ID = "Data")
app$uploadFile(file_upload = "data_bugs_switchData_pred.csv")
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "column1",
              dist_sel = "bernoulli")
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_mainNP_sel = "column2")
app$setInputs(navbar_ID = "Data")
app$snapshot()
# A test for the bug fixed by commit 490a653dce15fd7351586097b6724e9aa6d71f4b:
app$uploadFile(file_upload = "data_bugs_switchData_pred_mod.csv")
app$snapshot()
app$setInputs(navbar_ID = "Likelihood")
app$snapshot()
