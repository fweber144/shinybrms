app <- ShinyDriver$new("../../")
app$snapshotInit("priors_switchClass", screenshot = FALSE)

app$setInputs(advOpts_cores = 2, wait_ = FALSE, values_ = FALSE)

app$setInputs(navbar_ID = "Data",
              ex_da_sel = "bacteria",
              values_ = FALSE)
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "y",
              dist_sel = "bernoulli",
              values_ = FALSE)
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_mainGL_sel = "ID",
              values_ = FALSE)
app$setInputs(navbar_ID = "Prior",
              prior_class_sel = "sd",
              values_ = FALSE)
app$setInputs(prior_group_sel = "ID", values_ = FALSE)
app$setInputs(prior_text = "normal(0, 2.5, values_ = FALSE)",
              prior_add = "click",
              values_ = FALSE)
app$snapshot()
app$setInputs(prior_class_sel = "Intercept", values_ = FALSE)
app$setInputs(prior_text = "normal(0, 4, values_ = FALSE)",
              prior_add = "click",
              values_ = FALSE)
app$snapshot()
