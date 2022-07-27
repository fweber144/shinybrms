# Full model --------------------------------------------------------------

## Preparation ------------------------------------------------------------

app$setInputs(navbar_ID = "Data",
              ex_da_sel = "bacteria")
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "y",
              dist_sel = "bernoulli")
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$snapshot(items = list(output = c("formula_view")))
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_mainPL_sel = c("week", "trt"),
              pred_mainGL_sel = "ID")
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
              advOpts_cores = 2,
              advOpts_chains = 2,
              advOpts_iter = 500,
              advOpts_adapt_delta = 0.8,
              advOpts_max_treedepth = 10,
              advOpts_save_warmup = FALSE)
app$snapshot(filename = "prep_full.json")

## Posterior --------------------------------------------------------------

app$setInputs(run_stan = "click", timeout_ = 1800000)
app$snapshotDownload(
  "stanout_download",
  filename = file.path("..", paste0(tst_prefix, "_full.rds"))
)
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

# No-week model -----------------------------------------------------------

if (getOption("sbtst.run_upd_extend", TRUE)) {
  ## Preparation ------------------------------------------------------------
  
  app$setInputs(navbar_ID = "Likelihood")
  app$setInputs(likelihood_navlist_ID = "Predictors",
                pred_mainPL_sel = "trt")
  app$setInputs(likelihood_navlist_ID = "Formula preview")
  app$setInputs(navbar_ID = "Prior",
                prior_add = "click")
  app$setInputs(navbar_ID = "Posterior", wait_ = FALSE, values_ = FALSE)
  app$setInputs(posterior_navlist_ID = "Run Stan") # , wait_ = FALSE, values_ = FALSE
  app$snapshot(items = list(input = TRUE,
                            output = setdiff(app$listWidgets()$output, "fit_date"),
                            export = TRUE),
               filename = "prep_noWeek.json")
  
  ## Posterior --------------------------------------------------------------
  
  app$setInputs(run_stan = "click", timeout_ = 1800000)
  app$snapshotDownload(
    "stanout_download",
    filename = file.path("..", paste0(tst_prefix, "_noWeek.rds"))
  )
  app$setInputs(posterior_navlist_ID = "MCMC diagnostics")
  app$setInputs(posterior_navlist_ID = "Default summary")
  app$setInputs(posterior_navlist_ID = "Custom summary")
  app$setInputs(posterior_navlist_ID = "Conditional effects",
                term_sel = "trt")
  app$setInputs(posterior_navlist_ID = "Launch <strong>shinystan</strong>")
  app$snapshot(items = list(input = TRUE,
                            output = setdiff(app$listWidgets()$output, "fit_date"),
                            export = TRUE),
               filename = "post_noWeek.json")
}

# No-interaction model ----------------------------------------------------

## Preparation ------------------------------------------------------------

app$setInputs(navbar_ID = "Likelihood")
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_mainPL_sel = c("week", "trt"))
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$setInputs(navbar_ID = "Prior",
              prior_add = "click")
app$setInputs(navbar_ID = "Posterior", wait_ = FALSE, values_ = FALSE)
app$setInputs(posterior_navlist_ID = "Run Stan") # , wait_ = FALSE, values_ = FALSE
app$snapshot(items = list(input = TRUE,
                          output = setdiff(app$listWidgets()$output, "fit_date"),
                          export = TRUE),
             filename = "prep_noInt.json")

## Posterior --------------------------------------------------------------

app$setInputs(run_stan = "click", timeout_ = 1800000)
app$snapshotDownload(
  "stanout_download",
  filename = file.path("..", paste0(tst_prefix, "_noInt.rds"))
)
app$setInputs(posterior_navlist_ID = "MCMC diagnostics")
app$setInputs(posterior_navlist_ID = "Default summary")
app$setInputs(posterior_navlist_ID = "Custom summary",
              par_sel = "b_week",
              par_add = "click")
app$setInputs(cust_text = "abs(`b_week`)",
              cust_name = "absolute week effect",
              cust_act = "click")
app$setInputs(posterior_navlist_ID = "Conditional effects",
              term_sel = "week")
app$setInputs(posterior_navlist_ID = "Launch <strong>shinystan</strong>")
app$snapshot(items = list(input = TRUE,
                          output = setdiff(app$listWidgets()$output, "fit_date"),
                          export = TRUE),
             filename = "post_noInt.json")

# Upload full model -------------------------------------------------------

app$setInputs(posterior_navlist_ID = "Run Stan") # , wait_ = FALSE, values_ = FALSE
app$uploadFile(brmsfit_upload = file.path(paste0(tst_prefix, "_full.rds")))
app$setInputs(posterior_navlist_ID = "MCMC diagnostics")
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
### Needed because `brmsfit_upload`'s element `"size"` is highly sensitive to
### even small changes (like the date stored in the `brmsfit`, etc.); note that
### app$getValue() does not work here:
bfit_upld <- app$getAllValues(input = "brmsfit_upload",
                              output = FALSE,
                              export = FALSE)$input$brmsfit_upload
bfit_upld <- bfit_upld[, setdiff(names(bfit_upld), "size"), drop = FALSE]
stopifnot(identical(
  bfit_upld, data.frame(name = paste0(tst_prefix, "_full.rds"), type = "", datapath = "0.rds")
))
app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "brmsfit_upload"),###
                          output = setdiff(app$listWidgets()$output, "fit_date"),
                          export = TRUE),
             filename = "post_full_upload.json")

# Empty model -------------------------------------------------------------

## Preparation ------------------------------------------------------------

app$setInputs(navbar_ID = "Likelihood")
app$setInputs(likelihood_navlist_ID = "Predictors",
              pred_mainPL_sel = character(0))
app$setInputs(likelihood_navlist_ID = "Formula preview")
app$setInputs(navbar_ID = "Prior")
app$setInputs(navbar_ID = "Posterior", wait_ = FALSE, values_ = FALSE)
app$setInputs(posterior_navlist_ID = "Run Stan") # , wait_ = FALSE, values_ = FALSE
app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "brmsfit_upload"),
                          output = setdiff(app$listWidgets()$output, "fit_date"),
                          export = TRUE),
             filename = "prep_empty.json")

## Posterior --------------------------------------------------------------

app$setInputs(run_stan = "click", timeout_ = 1800000)
app$snapshotDownload(
  "stanout_download",
  filename = file.path("..", paste0(tst_prefix, "_empty.rds"))
)
app$setInputs(posterior_navlist_ID = "MCMC diagnostics")
app$setInputs(posterior_navlist_ID = "Default summary")
app$setInputs(posterior_navlist_ID = "Custom summary",
              par_sel = "b_Intercept",
              par_add = "click")
app$setInputs(cust_text = "`b_Intercept`^2",
              cust_name = "square intercept",
              cust_act = "click")
app$setInputs(posterior_navlist_ID = "Conditional effects")
app$setInputs(posterior_navlist_ID = "Launch <strong>shinystan</strong>")
app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "brmsfit_upload"),
                          output = setdiff(app$listWidgets()$output, "fit_date"),
                          export = TRUE),
             filename = "post_empty.json")
