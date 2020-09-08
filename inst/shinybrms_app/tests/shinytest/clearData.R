app <- ShinyDriver$new("../../")
app$snapshotInit("clearData", screenshot = FALSE)

app$setInputs(navbar_ID = "Data",
              ex_da_sel = "bacteria")
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "y",
              dist_sel = "bernoulli")
app$setInputs(navbar_ID = "Prior")
app$snapshot()
app$setInputs(prior_class_sel = "Intercept",
              prior_text = "student_t(3, 0, 3)",
              prior_add = "click")
app$snapshot()
# A test for the bug fixed by commit 32e2ca0b56124ee84ea56bcd21425e22425ad37c:
app$setInputs(navbar_ID = "Data",
              ex_da_sel = "")
app$setInputs(navbar_ID = "Prior")
app$snapshot()
app$setInputs(navbar_ID = "Likelihood")
app$snapshot()
