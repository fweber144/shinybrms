app <- ShinyDriver$new("../../")
app$snapshotInit("switchData_value", screenshot = FALSE)

app$setInputs(navbar_ID = "Data")
app$uploadFile(file_upload = "switchData-data.csv")
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "y",
              dist_sel = "bernoulli")
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_mainNP_sel = "x1")
app$setInputs(navbar_ID = "Prior",
              prior_class_sel = "b")
app$setInputs(prior_text = "normal(0, 2.5)",
              prior_add = "click")
app$snapshot()
app$setInputs(navbar_ID = "Data")
app$uploadFile(file_upload = "switchData_value-data.csv")
app$snapshot()
app$setInputs(navbar_ID = "Prior")
app$snapshot()
app$setInputs(navbar_ID = "Likelihood")
app$setInputs(likelihood_navlist_ID = "Outcome")
app$snapshot()
