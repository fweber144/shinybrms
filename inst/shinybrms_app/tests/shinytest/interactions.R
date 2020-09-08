app <- ShinyDriver$new("../../")
app$snapshotInit("interactions", screenshot = FALSE)

app$setInputs(navbar_ID = "Data",
              ex_da_sel = "Arabidopsis")
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "total.fruits",
              dist_sel = "negbinomial")
app$setInputs(likelihood_navlist_ID = "Predictors")
app$snapshot()
app$setInputs(pred_int_add = "click")
app$snapshot()
app$setInputs(pred_mainNP_sel = c("reg", "rack", "nutrient", "amd", "status"),
              pred_mainPP_sel = c("popu", "gen"))
app$snapshot()
app$setInputs(pred_int_add = "click")
app$snapshot()
app$setInputs(pred_int_build = c("reg", "nutrient"),
              pred_int_add = "click")
app$snapshot()
app$setInputs(pred_int_build = c("nutrient", "reg"),
              pred_int_add = "click")
app$snapshot()
app$setInputs(pred_int_build = c("reg", "nutrient", "popu"),
              pred_int_add = "click")
app$snapshot()
app$setInputs(pred_int_build = c("reg", "nutrient", "amd"),
              pred_int_add = "click")
app$snapshot()
app$setInputs(pred_int_build = c("popu", "gen"),
              pred_int_add = "click")
app$snapshot()
app$setInputs(pred_int_build = c("rack", "amd", "popu", "gen"),
              pred_int_add = "click")
app$snapshot()
app$setInputs(pred_int_build = c("rack", "popu", "gen"),
              pred_int_add = "click")
app$snapshot()
app$setInputs(pred_int_build = c("status", "popu"),
              pred_int_add = "click")
app$snapshot()
app$setInputs(pred_mainNP_sel = c("rack", "nutrient", "amd", "status"))
app$snapshot()
app$setInputs(pred_mainNP_sel = c("rack", "nutrient", "amd", "status", "reg"))
app$setInputs(pred_int_build = c("reg", "nutrient"),
              pred_int_add = "click")
app$snapshot()
# A test for adding a variable at a specific position (here, "reg" at the beginning):
app$setInputs(pred_mainNP_sel = c("reg", "rack", "nutrient", "amd", "status"))
app$setInputs(pred_int_build = c("reg", "nutrient"),
              pred_int_add = "click")
app$snapshot()
# A test for removing an interaction term:
app$setInputs(pred_int_sel = c("popu<-->gen", "rack<-->popu<-->gen", "status<-->popu", "reg<-->nutrient"))
app$snapshot()
# A test for the bug fixed in commit 695e50dde2e31202034c1b17c103d19aecd43b72:
app$setInputs(pred_int_build = c("nutrient", "amd"),
              pred_int_add = "click")
app$snapshot()
