app <- ShinyDriver$new("../../")
app$snapshotInit("priors_autoSetEmpty", screenshot = FALSE)

app$setInputs(advOpts_cores = 2, wait_ = FALSE, values_ = FALSE)

app$setInputs(navbar_ID = "Data",
              ex_da_sel = "bacteria")
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "y",
              dist_sel = "bernoulli")
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_mainCP_sel = "week",
              pred_mainPP_sel = "ID")
app$setInputs(pred_int_build = c("week", "ID"),
              pred_int_add = "click")
app$setInputs(navbar_ID = "Prior",
              prior_class_sel = "sd")
app$setInputs(prior_group_sel = "ID")
app$snapshot()
# A test for the bug fixed by commit e484a6fdff61fa9eb60ae67a9b0a8bdcc932502c:
app$setInputs(prior_class_sel = "cor")
app$snapshot()
