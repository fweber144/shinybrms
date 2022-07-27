app <- ShinyDriver$new("../../")
app$snapshotInit("clearData", screenshot = FALSE)

app$setInputs(advOpts_cores = 2, wait_ = FALSE, values_ = FALSE)

app$setInputs(navbar_ID = "Data",
              ex_da_sel = "bacteria")
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "y",
              dist_sel = "bernoulli")
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_mainPL_sel = c("week", "trt"),
              pred_mainGL_sel = "ID")
app$setInputs(pred_int_build = c("week", "trt"),
              pred_int_add = "click")
app$setInputs(navbar_ID = "Prior",
              prior_class_sel = "b",
              prior_text = "student_t(3, 0, 10)",
              prior_add = "click")
app$snapshot()
# A test for the bugs fixed by commits 32e2ca0b56124ee84ea56bcd21425e22425ad37c and
# 01e0e1a9140c5d393b0e7117b2cb866660eba0a4:
app$setInputs(navbar_ID = "Data",
              ex_da_sel = "")
app$setInputs(navbar_ID = "Prior")
app$setInputs(navbar_ID = "Likelihood")
app$setInputs(likelihood_navlist_ID = "Outcome")
app$snapshot()
