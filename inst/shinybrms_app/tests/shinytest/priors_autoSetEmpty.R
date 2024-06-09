app <- ShinyDriver$new("../../")
app$snapshotInit("priors_autoSetEmpty", screenshot = FALSE)

app$setInputs(advOpts_cores = 2, wait_ = FALSE, values_ = FALSE)

app$setInputs(navbar_ID = "Data",
              ex_da_sel = "bacteria",
              values_ = FALSE)
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "y",
              dist_sel = "bernoulli",
              values_ = FALSE)
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_mainPL_sel = "week",
              pred_mainGL_sel = "ID",
              values_ = FALSE)
app$setInputs(pred_int_build = c("week", "ID"),
              pred_int_add = "click",
              values_ = FALSE)
app$setInputs(navbar_ID = "Prior",
              prior_class_sel = "sd",
              values_ = FALSE)
app$setInputs(prior_group_sel = "ID", values_ = FALSE)
app$snapshot()
# A test for the bug fixed by commit e484a6fdff61fa9eb60ae67a9b0a8bdcc932502c:
app$setInputs(prior_class_sel = "cor", values_ = FALSE)
app$snapshot()
