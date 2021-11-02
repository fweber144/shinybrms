app <- ShinyDriver$new("../../")
app$snapshotInit("offsets", screenshot = FALSE)

app$setInputs(advOpts_cores = 2, wait_ = FALSE, values_ = FALSE)

app$setInputs(navbar_ID = "Data",
              ex_da_sel = "epilepsy")
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "count",
              dist_sel = "negbinomial")
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_mainCP_sel = c("Trt", "zAge"),
              pred_mainPP_sel = c("patient", "visit"))
app$setInputs(pred_int_build = c("Trt", "zAge"),
              pred_int_add = "click")
app$setInputs(pred_int_build = c("Trt", "visit"),
              pred_int_add = "click")
app$setInputs(offs_sel = c("zBase", "Base"))
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$setInputs(navbar_ID = "Prior")
app$snapshot()
app$setInputs(navbar_ID = "Data",
              ex_da_sel = "")
app$setInputs(navbar_ID = "Likelihood")
app$setInputs(likelihood_navlist_ID = "Predictors")
app$setInputs(likelihood_navlist_ID = "Outcome")
app$setInputs(navbar_ID = "Prior")
app$snapshot()
