app <- ShinyDriver$new("../../")
app$snapshotInit("clearFamily", screenshot = FALSE)

app$setInputs(advOpts_cores = 2, wait_ = FALSE, values_ = FALSE)

app$setInputs(navbar_ID = "Data",
              ex_da_sel = "bacteria",
              values_ = FALSE)
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "y",
              dist_sel = "bernoulli",
              values_ = FALSE)
app$setInputs(navbar_ID = "Prior",
              prior_class_sel = "Intercept",
              prior_text = "student_t(3, 0, 3)",
              prior_add = "click",
              values_ = FALSE)
app$snapshot()
app$setInputs(navbar_ID = "Likelihood",
              dist_sel = "",
              values_ = FALSE)
app$setInputs(navbar_ID = "Prior", values_ = FALSE)
app$snapshot()
