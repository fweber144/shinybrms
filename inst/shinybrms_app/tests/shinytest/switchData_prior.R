app <- ShinyDriver$new("../../")
app$snapshotInit("switchData_prior", screenshot = FALSE)

app$setInputs(advOpts_cores = 2, wait_ = FALSE, values_ = FALSE)

app$setInputs(navbar_ID = "Data",
              ex_da_sel = "Arabidopsis",
              values_ = FALSE)
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "total.fruits",
              dist_sel = "negbinomial",
              values_ = FALSE)
app$setInputs(navbar_ID = "Prior", values_ = FALSE)
app$snapshot()
app$setInputs(navbar_ID = "Data",
              ex_da_sel = "bacteria",
              values_ = FALSE)
app$snapshot()
app$setInputs(navbar_ID = "Prior", values_ = FALSE)
app$snapshot()
