app <- ShinyDriver$new("../../")
app$snapshotInit("clearFamily", screenshot = FALSE)

app$setInputs(advOpts_cores = 2, wait_ = FALSE, values_ = FALSE)

app$setInputs(navbar_ID = "Data",
              ex_da_sel = "bacteria")
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "y",
              dist_sel = "bernoulli")
app$setInputs(navbar_ID = "Prior",
              prior_class_sel = "Intercept",
              prior_text = "student_t(3, 0, 3)",
              prior_add = "click")
app$snapshot()
app$setInputs(navbar_ID = "Likelihood",
              dist_sel = "")
app$setInputs(navbar_ID = "Prior")
app$snapshot()
