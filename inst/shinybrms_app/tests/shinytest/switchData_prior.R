app <- ShinyDriver$new("../../")
app$snapshotInit("switchData_prior", screenshot = FALSE)

app$setInputs(advOpts_cores = 2, wait_ = FALSE, values_ = FALSE)

app$setInputs(navbar_ID = "Data",
              ex_da_sel = "Arabidopsis")
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "total.fruits",
              dist_sel = "negbinomial")
app$setInputs(navbar_ID = "Prior")
app$snapshot()
app$setInputs(navbar_ID = "Data",
              ex_da_sel = "bacteria")
app$snapshot()
app$setInputs(navbar_ID = "Prior")
app$snapshot()
