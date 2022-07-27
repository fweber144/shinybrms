app <- ShinyDriver$new("../../")
app$snapshotInit("priors_switchClass", screenshot = FALSE)

app$setInputs(advOpts_cores = 2, wait_ = FALSE, values_ = FALSE)

app$setInputs(navbar_ID = "Data",
              ex_da_sel = "bacteria")
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "y",
              dist_sel = "bernoulli")
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_mainGL_sel = "ID")
app$setInputs(navbar_ID = "Prior",
              prior_class_sel = "sd")
app$setInputs(prior_group_sel = "ID")
app$setInputs(prior_text = "normal(0, 2.5)",
              prior_add = "click")
app$snapshot()
app$setInputs(prior_class_sel = "Intercept")
app$setInputs(prior_text = "normal(0, 4)",
              prior_add = "click")
app$snapshot()
