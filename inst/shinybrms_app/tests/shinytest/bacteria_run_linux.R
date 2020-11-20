app <- ShinyDriver$new("../../")
app$snapshotInit("bacteria_run_linux", screenshot = FALSE)

app$setInputs(navbar_ID = "Data",
              ex_da_sel = "bacteria")
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "y",
              dist_sel = "bernoulli")
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$snapshot(items = list(output = c("formula_view")))
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_mainNP_sel = c("week", "trt"),
              pred_mainPP_sel = "ID")
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$snapshot(items = list(output = c("formula_view")))
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_int_build = c("week", "trt"),
              pred_int_add = "click")
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$setInputs(navbar_ID = "Prior",
              prior_class_sel = "b",
              prior_text = "student_t(3, 0, 4)",
              prior_add = "click")
app$setInputs(navbar_ID = "Posterior",
              show_advOpts = TRUE,
              advOpts_seed = 6789,
              advOpts_cores = 2)
app$snapshot()
app$setInputs(run_stan = "click", timeout_ = 1800000)
app$snapshot(items = list(input = TRUE,
                          output = setdiff(app$listWidgets()$output, "fit_date"),
                          export = TRUE))
app$setInputs(posterior_navlist_ID = "MCMC diagnostics",
              show_general_MCMC_tab = TRUE)
app$snapshot(items = list(input = TRUE,
                          output = setdiff(app$listWidgets()$output, "fit_date"),
                          export = TRUE))
app$setInputs(posterior_navlist_ID = "Summary")
app$snapshot(items = list(input = TRUE,
                          output = setdiff(app$listWidgets()$output, "fit_date"),
                          export = TRUE))
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
app$snapshot(items = list(input = TRUE,
                          output = setdiff(app$listWidgets()$output, "fit_date"),
                          export = TRUE))
app$setInputs(posterior_navlist_ID = "Conditional effects")
app$snapshot(items = list(input = TRUE,
                          output = setdiff(app$listWidgets()$output, "fit_date"),
                          export = TRUE))
app$setInputs(term_sel = "week:trt")
app$snapshot(items = list(input = TRUE,
                          output = setdiff(app$listWidgets()$output, "fit_date"),
                          export = TRUE))
