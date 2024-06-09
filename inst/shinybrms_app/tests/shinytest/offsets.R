app <- ShinyDriver$new("../../")
app$snapshotInit("offsets", screenshot = FALSE)

app$setInputs(advOpts_cores = 2, wait_ = FALSE, values_ = FALSE)

app$setInputs(navbar_ID = "Data",
              ex_da_sel = "epilepsy",
              values_ = FALSE)
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "count",
              dist_sel = "negbinomial",
              values_ = FALSE)
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_mainPL_sel = c("Trt", "zAge"),
              pred_mainGL_sel = c("patient", "visit"),
              values_ = FALSE)
app$setInputs(pred_int_build = c("Trt", "zAge"),
              pred_int_add = "click",
              values_ = FALSE)
app$setInputs(pred_int_build = c("Trt", "visit"),
              pred_int_add = "click",
              values_ = FALSE)
app$setInputs(offs_sel = c("zBase", "Base"), values_ = FALSE)
app$setInputs(likelihood_navlist_ID = "Formula preview", values_ = FALSE)
app$setInputs(navbar_ID = "Prior", values_ = FALSE)
app$snapshot()
app$setInputs(navbar_ID = "Data",
              ex_da_sel = "",
              values_ = FALSE)
app$setInputs(navbar_ID = "Likelihood", values_ = FALSE)
app$setInputs(likelihood_navlist_ID = "Predictors", values_ = FALSE)
app$setInputs(likelihood_navlist_ID = "Outcome", values_ = FALSE)
app$setInputs(navbar_ID = "Prior", values_ = FALSE)
app$snapshot()
