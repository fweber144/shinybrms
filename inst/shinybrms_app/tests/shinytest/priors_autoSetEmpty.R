app <- ShinyDriver$new("../../")
app$snapshotInit("priors_autoSetEmpty")

app$setInputs(navbar_ID = "Data",
              ex_da_sel = "bacteria")
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "y",
              dist_sel = "bernoulli")
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_mainNP_sel = "week",
              pred_mainPP_sel = "ID")
app$setInputs(pred_int_build = c("week", "ID"),
              pred_int_add = "click")
app$setInputs(navbar_ID = "Prior",
              prior_class_sel = "sd")
app$setInputs(prior_group_sel = "ID")
app$snapshot()
app$setInputs(prior_class_sel = "cor")
app$snapshot()
