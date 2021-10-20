app <- ShinyDriver$new("../../")
app$snapshotInit("switchData_outcome", screenshot = FALSE)

app$setInputs(advOpts_cores = 2, wait_ = FALSE, values_ = FALSE)

app$setInputs(navbar_ID = "Data",
              ex_da_sel = "Arabidopsis")
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "total.fruits",
              dist_sel = "negbinomial")
app$setInputs(navbar_ID = "Data")
app$snapshot()
# A test for the bug fixed by commits cea0f49cc275636d4875c40f4410eb827bab9112 and
# 69578f0170b4f4c48958bbf6f6fc1d33f7f7f626:
app$setInputs(ex_da_sel = "bacteria")
app$snapshot()
