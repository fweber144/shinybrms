# Abbreviation (character) for the OS (one could simply use shinytest::osName()
# instead, but that gives "win" instead of "windows", so would require changing
# the names of the folders for the expected test results):
if (identical(Sys.info()["sysname"], setNames("Linux", "sysname"))) {
  os_chr <- "linux"
} else {
  os_chr <- .Platform$OS.type
}

app$setInputs(advOpts_cores = 2, wait_ = FALSE, values_ = FALSE)

app$setInputs(navbar_ID = "Posterior")
app$uploadFile(brmsfit_upload = file.path(paste0("bacteria_run_", os_chr, "-expected"),
                                          "bacteria_full.rds"))
app$setInputs(posterior_navlist_ID = "MCMC diagnostics",
              show_general_MCMC_tab = TRUE)
app$setInputs(posterior_navlist_ID = "Default summary")
app$setInputs(posterior_navlist_ID = "Custom summary",
              par_sel = "b_week",
              par_add = "click")
app$setInputs(cust_text = "`b_week` + ",
              par_sel = "b_week:trtdrug",
              par_add = "click")
app$setInputs(cust_act = "click")
app$setInputs(cust_text = "`b_week` + ",
              par_sel = "b_week:trtdrugP",
              par_add = "click")
app$setInputs(cust_name = "week for drugP",
              cust_act = "click")
app$setInputs(posterior_navlist_ID = "Conditional effects",
              term_sel = "week:trt")
app$setInputs(posterior_navlist_ID = "Launch <strong>shinystan</strong>")
app$snapshot(items = list(input = TRUE,
                          output = setdiff(app$listWidgets()$output, "fit_date"),
                          export = TRUE),
             filename = "post_full.json")
