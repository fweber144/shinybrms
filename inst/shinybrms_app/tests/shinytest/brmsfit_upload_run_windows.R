app <- ShinyDriver$new("../../", options = list(
  shinybrms.allow_upd = getOption("shinybrms.allow_upd", TRUE),
  brms.backend = getOption("brms.backend", "rstan")
))

app$snapshotInit("brmsfit_upload_run_windows", screenshot = FALSE)

app$setInputs(advOpts_cores = 2, wait_ = FALSE, values_ = FALSE)

app$setInputs(navbar_ID = "Posterior", values_ = FALSE)

# Upload full model -------------------------------------------------------

# app$upload_file(brmsfit_upload = file.path("bacteria_full.rds"))
app$setInputs(posterior_navlist_ID = "MCMC diagnostics",
              show_general_MCMC_tab = TRUE,
              values_ = FALSE)
app$setInputs(posterior_navlist_ID = "Default summary", values_ = FALSE)
app$setInputs(posterior_navlist_ID = "Custom summary",
              par_sel = "b_week",
              par_add = "click",
              values_ = FALSE)
app$setInputs(cust_text = "`b_week` + ",
              par_sel = "b_week:trtdrug",
              par_add = "click",
              values_ = FALSE)
app$setInputs(cust_act = "click", values_ = FALSE)
app$setInputs(cust_text = "`b_week` + ",
              par_sel = "b_week:trtdrugP",
              par_add = "click",
              values_ = FALSE)
app$setInputs(cust_name = "week for drugP",
              cust_act = "click",
              values_ = FALSE)
app$setInputs(posterior_navlist_ID = "Conditional effects",
              term_sel = "week:trt",
              values_ = FALSE)
app$setInputs(posterior_navlist_ID = "Launch <strong>shinystan</strong>", values_ = FALSE)
### Needed because `brmsfit_upload`'s element `"size"` is highly sensitive to
### even small changes (like the date stored in the `brmsfit`, etc.); note that
### app$getValue() does not work here:
bfit_upld <- app$getAllValues(input = "brmsfit_upload",
                              output = FALSE,
                              export = FALSE)$input$brmsfit_upload

stopifnot(identical(
  bfit_upld, data.frame(name = "bacteria_full.rds", type = "", datapath = "0.rds")
))
app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "brmsfit_upload"),###
                          output = setdiff(app$listWidgets()$output, "fit_date"),
                          export = TRUE),
             filename = "post_full.json")

# Upload no-interaction model ---------------------------------------------

app$setInputs(posterior_navlist_ID = "Run Stan", values_ = FALSE)
# app$upload_file(brmsfit_upload = file.path("bacteria_noInt.rds"))
app$setInputs(posterior_navlist_ID = "MCMC diagnostics", values_ = FALSE)
app$setInputs(posterior_navlist_ID = "Default summary", values_ = FALSE)
app$setInputs(posterior_navlist_ID = "Custom summary",
              par_sel = "b_week",
              par_add = "click",
              values_ = FALSE)
app$setInputs(cust_text = "abs(`b_week`, values_ = FALSE)",
              cust_name = "absolute week effect",
              cust_act = "click",
              values_ = FALSE)
app$setInputs(posterior_navlist_ID = "Conditional effects",
              term_sel = "week",
              values_ = FALSE)
app$setInputs(posterior_navlist_ID = "Launch <strong>shinystan</strong>", values_ = FALSE)
bfit_upld <- app$getAllValues(input = "brmsfit_upload",
                              output = FALSE,
                              export = FALSE)$input$brmsfit_upload

stopifnot(identical(
  bfit_upld, data.frame(name = "bacteria_noInt.rds", type = "", datapath = "0.rds")
))
app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "brmsfit_upload"),
                          output = setdiff(app$listWidgets()$output, "fit_date"),
                          export = TRUE),
             filename = "post_noInt.json")

# Click "Run Stan" button -------------------------------------------------

app$setInputs(posterior_navlist_ID = "Run Stan", wait_ = FALSE, values_ = FALSE)
app$setInputs(run_stan = "click", timeout_ = 10000, values_ = FALSE)
bfit_upld <- app$getAllValues(input = "brmsfit_upload",
                              output = FALSE,
                              export = FALSE)$input$brmsfit_upload

stopifnot(identical(
  bfit_upld, data.frame(name = "bacteria_noInt.rds", type = "", datapath = "0.rds")
))

# Upload no-week model ----------------------------------------------------

if (getOption("sbtst.run_upd_extend", TRUE)) {
#   app$upload_file(brmsfit_upload = file.path("bacteria_noWeek.rds"))
  app$setInputs(posterior_navlist_ID = "MCMC diagnostics", values_ = FALSE)
  app$setInputs(posterior_navlist_ID = "Default summary", values_ = FALSE)
  app$setInputs(posterior_navlist_ID = "Custom summary", values_ = FALSE)
  app$setInputs(posterior_navlist_ID = "Conditional effects",
                term_sel = "trt",
                values_ = FALSE)
  app$setInputs(posterior_navlist_ID = "Launch <strong>shinystan</strong>", values_ = FALSE)
  bfit_upld <- app$getAllValues(input = "brmsfit_upload",
                                output = FALSE,
                                export = FALSE)$input$brmsfit_upload
  
  stopifnot(identical(
    bfit_upld, data.frame(name = "bacteria_noWeek.rds", type = "", datapath = "0.rds")
  ))
  app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "brmsfit_upload"),
                            output = setdiff(app$listWidgets()$output, "fit_date"),
                            export = TRUE),
               filename = "post_noWeek.json")
}

# Upload empty model ------------------------------------------------------

# app$upload_file(brmsfit_upload = file.path("bacteria_empty.rds"))
app$setInputs(posterior_navlist_ID = "MCMC diagnostics", values_ = FALSE)
app$setInputs(posterior_navlist_ID = "Default summary", values_ = FALSE)
app$setInputs(posterior_navlist_ID = "Custom summary",
              par_sel = "b_Intercept",
              par_add = "click",
              values_ = FALSE)
app$setInputs(cust_text = "`b_Intercept`^2",
              cust_name = "square intercept",
              cust_act = "click",
              values_ = FALSE)
app$setInputs(posterior_navlist_ID = "Conditional effects", values_ = FALSE)
app$setInputs(posterior_navlist_ID = "Launch <strong>shinystan</strong>", values_ = FALSE)
bfit_upld <- app$getAllValues(input = "brmsfit_upload",
                              output = FALSE,
                              export = FALSE)$input$brmsfit_upload

stopifnot(identical(
  bfit_upld, data.frame(name = "bacteria_empty.rds", type = "", datapath = "0.rds")
))
app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "brmsfit_upload"),
                          output = setdiff(app$listWidgets()$output, "fit_date"),
                          export = TRUE),
             filename = "post_empty.json")
