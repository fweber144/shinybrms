app <- ShinyDriver$new("../../")
app$snapshotInit("priors", screenshot = FALSE)

app$setInputs(navbar_ID = "Data",
              ex_da_sel = "bacteria")
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "y",
              dist_sel = "bernoulli")
app$setInputs(likelihood_navlist_ID = "Predictors")
app$setInputs(pred_mainNP_sel = c("week", "trt"),
              pred_mainPP_sel = "ID")
app$setInputs(pred_int_build = c("week", "trt"),
              pred_int_add = "click")
app$setInputs(navbar_ID = "Prior")
app$snapshot()
# A test for the bug fixed by commit 4a03135b045b092c91b8d5939bea9965a6b9dc2d:
app$setInputs(prior_class_sel = "sd")
app$setInputs(prior_coef_sel = "Intercept")
app$snapshot()
app$setInputs(prior_text = "student_t(3, 0, 3)",
              prior_add = "click")
app$snapshot()
# A test for the bug fixed by commit 11d32ca228f913f2d55dfbce6735a34e81d89639:
app$setInputs(prior_coef_sel = "")
app$snapshot()
app$setInputs(prior_class_sel = "")
app$setInputs(prior_class_sel = "sd")
app$snapshot()
app$setInputs(prior_reset = "click")
app$snapshot()
