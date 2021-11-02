app <- ShinyDriver$new("../../")
app$snapshotInit("interactions", screenshot = FALSE)

app$setInputs(advOpts_cores = 2, wait_ = FALSE, values_ = FALSE)

app$setInputs(navbar_ID = "Data",
              ex_da_sel = "Arabidopsis")
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "total.fruits",
              dist_sel = "negbinomial")
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$setInputs(likelihood_navlist_ID = "Predictors")
app$snapshot()
# A test for the bug fixed by commit 17ba0f6de57741b1f39157b10f31e28af6c5e52b:
app$setInputs(pred_int_add = "click")
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$snapshot()
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_mainCP_sel = c("reg", "rack", "nutrient", "amd", "status"),
              pred_mainPP_sel = c("popu", "gen"))
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$snapshot()
# A test for the bug fixed by commit 17ba0f6de57741b1f39157b10f31e28af6c5e52b:
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_int_add = "click")
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$snapshot()
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_int_build = c("reg", "nutrient"),
              pred_int_add = "click")
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$snapshot()
# A test for adding an already existing interaction term in a different order of the involved
# variables (fixed by commit 5f2242f244a5c68e25e1a89dc21e20c58872a1d0):
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_int_build = c("nutrient", "reg"),
              pred_int_add = "click")
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$snapshot()
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_int_build = c("reg", "nutrient", "popu"),
              pred_int_add = "click")
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$snapshot()
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_int_build = c("reg", "nutrient", "amd"),
              pred_int_add = "click")
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$snapshot()
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_int_build = c("popu", "gen"),
              pred_int_add = "click")
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$snapshot()
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_int_build = c("rack", "amd", "popu", "gen"),
              pred_int_add = "click")
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$snapshot()
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_int_build = c("rack", "popu", "gen"),
              pred_int_add = "click")
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$snapshot()
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_int_build = c("status", "popu"),
              pred_int_add = "click")
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$snapshot()
# A test for removing a variable whose main effect is involved in at least one interaction (fixed by
# commit b880b306cf6ad8a13cae235590c9a430c6c669c3):
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_mainCP_sel = c("rack", "nutrient", "amd", "status"))
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$snapshot()
# A test for re-adding the previously removed variable (requires to re-add all previously added
# interaction terms involving the previously removed variable, but here only one of those
# interaction terms is re-added):
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_mainCP_sel = c("rack", "nutrient", "amd", "status", "reg"))
app$setInputs(pred_int_build = c("reg", "nutrient"),
              pred_int_add = "click")
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$snapshot()
# A test for adding a variable at a specific position (here, "reg" at the beginning):
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_mainCP_sel = c("rack", "nutrient", "amd", "status"))
app$setInputs(pred_mainCP_sel = c("reg", "rack", "nutrient", "amd", "status"))
app$setInputs(pred_int_build = c("reg", "nutrient"),
              pred_int_add = "click")
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$snapshot()
# A test for removing an interaction term:
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_int_sel = c("popu<-->gen", "rack<-->popu<-->gen", "status<-->popu", "reg<-->nutrient"))
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$snapshot()
# A test for the bug fixed by commit 695e50dde2e31202034c1b17c103d19aecd43b72:
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_int_build = c("nutrient", "amd"),
              pred_int_add = "click")
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$snapshot()
