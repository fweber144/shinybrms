app <- ShinyDriver$new("../../")
app$snapshotInit("bugs-switchdata-outcome", screenshot = FALSE)

app$setInputs(navbar_ID = "Data",
              ex_da_sel = "Arabidopsis")
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "total.fruits",
              dist_sel = "negbinomial")
app$setInputs(navbar_ID = "Data")
app$snapshot()
app$setInputs(ex_da_sel = "bacteria")
app$snapshot()
