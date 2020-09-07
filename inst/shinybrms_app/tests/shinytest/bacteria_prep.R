app <- ShinyDriver$new("../../")
app$snapshotInit("bacteria_prep", screenshot = FALSE)

app$setInputs(navbar_ID = "Data",
              ex_da_sel = "bacteria")
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "y",
              dist_sel = "bernoulli")
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$snapshot(items = list(output = c("formula_view")))
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_mainNP_sel = c("week", "trt"),
              pred_mainPP_sel = "ID")
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$snapshot(items = list(output = c("formula_view")))
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_int_build = c("week", "trt"),
              pred_int_add = "click")
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$setInputs(navbar_ID = "Prior",
              prior_class_sel = "b",
              prior_text = "student_t(3, 0, 10)",
              prior_add = "click")
app$setInputs(navbar_ID = "Posterior",
              show_advOpts = TRUE,
              advOpts_seed = 6789,
              advOpts_cores = 4)
app$snapshot()
