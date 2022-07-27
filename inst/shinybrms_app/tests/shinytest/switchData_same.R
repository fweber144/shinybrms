app <- ShinyDriver$new("../../")
app$snapshotInit("switchData_same", screenshot = FALSE)

app$setInputs(advOpts_cores = 2, wait_ = FALSE, values_ = FALSE)

app$setInputs(navbar_ID = "Data")
app$uploadFile(data_upload = "switchData-data.csv")
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "y",
              dist_sel = "bernoulli")
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_mainPL_sel = "x1")
app$setInputs(navbar_ID = "Prior",
              prior_class_sel = "b")
app$setInputs(prior_text = "normal(0, 2.5)",
              prior_add = "click")
app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "data_upload"),
                          output = TRUE,
                          export = TRUE))
app$setInputs(navbar_ID = "Data")
app$uploadFile(data_upload = "switchData-data.csv")
app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "data_upload"),
                          output = TRUE,
                          export = TRUE))
app$setInputs(navbar_ID = "Prior")
app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "data_upload"),
                          output = TRUE,
                          export = TRUE))
app$setInputs(navbar_ID = "Likelihood")
app$setInputs(likelihood_navlist_ID = "Outcome")
app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "data_upload"),
                          output = TRUE,
                          export = TRUE))
