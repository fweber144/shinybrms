app <- ShinyDriver$new("../../")
app$snapshotInit("switchData_value", screenshot = FALSE)

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
app$setInputs(navbar_ID = "Prior",
              prior_class_sel = "b",
              values_ = FALSE)
app$setInputs(prior_text = "normal(0, 2.5, values_ = FALSE)",
              prior_add = "click",
              values_ = FALSE)
app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "data_upload"),
                          output = TRUE,
                          export = TRUE))
app$setInputs(navbar_ID = "Data", values_ = FALSE)
# app$upload_file(data_upload = "switchData_value-data.csv")
app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "data_upload"),
                          output = TRUE,
                          export = TRUE))
app$setInputs(navbar_ID = "Prior", values_ = FALSE)
app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "data_upload"),
                          output = TRUE,
                          export = TRUE))
app$setInputs(navbar_ID = "Likelihood", values_ = FALSE)
app$setInputs(likelihood_navlist_ID = "Outcome", values_ = FALSE)
app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "data_upload"),
                          output = TRUE,
                          export = TRUE))
