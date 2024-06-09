app <- ShinyDriver$new("../../")
app$snapshotInit("clearData", screenshot = FALSE)

app$setInputs(advOpts_cores = 2, wait_ = FALSE, values_ = FALSE)

app$setInputs(navbar_ID = "Data",
              ex_da_sel = "bacteria",
              values_ = FALSE)
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "y",
              dist_sel = "bernoulli",
              values_ = FALSE)
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_mainPL_sel = c("week", "trt"),
              pred_mainGL_sel = "ID",
              values_ = FALSE)
app$setInputs(pred_int_build = c("week", "trt"),
              pred_int_add = "click",
              values_ = FALSE)
app$setInputs(navbar_ID = "Prior",
              prior_class_sel = "b",
              prior_text = "student_t(3, 0, 10)",
              prior_add = "click",
              values_ = FALSE)
app$snapshot()
# A test for the bugs fixed by commits 32e2ca0b56124ee84ea56bcd21425e22425ad37c and
# 01e0e1a9140c5d393b0e7117b2cb866660eba0a4:
app$setInputs(navbar_ID = "Data",
              ex_da_sel = "",
              values_ = FALSE)
app$setInputs(navbar_ID = "Prior", values_ = FALSE)
app$setInputs(navbar_ID = "Likelihood", values_ = FALSE)
app$setInputs(likelihood_navlist_ID = "Outcome", values_ = FALSE)
app$snapshot()
