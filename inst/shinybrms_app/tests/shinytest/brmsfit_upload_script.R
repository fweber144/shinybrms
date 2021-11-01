app$setInputs(advOpts_cores = 2, wait_ = FALSE, values_ = FALSE)

app$setInputs(navbar_ID = "Posterior")

# Upload full model -------------------------------------------------------

app$uploadFile(brmsfit_upload = file.path("bacteria_full.rds"))
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
### Needed because `brmsfit_upload`'s element `"size"` is highly sensitive to
### even small changes (like the date stored in the `brmsfit`, etc.); note that
### app$getValue() does not work here:
bfit_upld <- app$getAllValues(input = "brmsfit_upload",
                              output = FALSE,
                              export = FALSE)$input$brmsfit_upload
bfit_upld <- bfit_upld[, setdiff(names(bfit_upld), "size"), drop = FALSE]
stopifnot(identical(
  bfit_upld, data.frame(name = "bacteria_full.rds", type = "", datapath = "0.rds")
))
app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "brmsfit_upload"),###
                          output = setdiff(app$listWidgets()$output, "fit_date"),
                          export = TRUE),
             filename = "post_full.json")

# Upload no-interaction model ---------------------------------------------

app$setInputs(posterior_navlist_ID = "Run Stan")
app$uploadFile(brmsfit_upload = file.path("bacteria_noInt.rds"))
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
bfit_upld <- app$getAllValues(input = "brmsfit_upload",
                              output = FALSE,
                              export = FALSE)$input$brmsfit_upload
bfit_upld <- bfit_upld[, setdiff(names(bfit_upld), "size"), drop = FALSE]
stopifnot(identical(
  bfit_upld, data.frame(name = "bacteria_noInt.rds", type = "", datapath = "0.rds")
))
app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "brmsfit_upload"),
                          output = setdiff(app$listWidgets()$output, "fit_date"),
                          export = TRUE),
             filename = "post_noInt.json")

# Click "Run Stan" button -------------------------------------------------

app$setInputs(posterior_navlist_ID = "Run Stan", wait_ = FALSE, values_ = FALSE)
app$setInputs(run_stan = "click", timeout_ = 10000)
bfit_upld <- app$getAllValues(input = "brmsfit_upload",
                              output = FALSE,
                              export = FALSE)$input$brmsfit_upload
bfit_upld <- bfit_upld[, setdiff(names(bfit_upld), "size"), drop = FALSE]
stopifnot(identical(
  bfit_upld, data.frame(name = "bacteria_noInt.rds", type = "", datapath = "0.rds")
))

# Upload no-week model ----------------------------------------------------

if (getOption("sbtst.run_upd_extend", TRUE)) {
  app$uploadFile(brmsfit_upload = file.path("bacteria_noWeek.rds"))
  app$setInputs(posterior_navlist_ID = "MCMC diagnostics")
  app$setInputs(posterior_navlist_ID = "Default summary")
  app$setInputs(posterior_navlist_ID = "Custom summary")
  app$setInputs(posterior_navlist_ID = "Conditional effects",
                term_sel = "trt")
  app$setInputs(posterior_navlist_ID = "Launch <strong>shinystan</strong>")
  bfit_upld <- app$getAllValues(input = "brmsfit_upload",
                                output = FALSE,
                                export = FALSE)$input$brmsfit_upload
  bfit_upld <- bfit_upld[, setdiff(names(bfit_upld), "size"), drop = FALSE]
  stopifnot(identical(
    bfit_upld, data.frame(name = "bacteria_noWeek.rds", type = "", datapath = "0.rds")
  ))
  app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "brmsfit_upload"),
                            output = setdiff(app$listWidgets()$output, "fit_date"),
                            export = TRUE),
               filename = "post_noWeek.json")
}

# Upload empty model ------------------------------------------------------

app$uploadFile(brmsfit_upload = file.path("bacteria_empty.rds"))
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
bfit_upld <- app$getAllValues(input = "brmsfit_upload",
                              output = FALSE,
                              export = FALSE)$input$brmsfit_upload
bfit_upld <- bfit_upld[, setdiff(names(bfit_upld), "size"), drop = FALSE]
stopifnot(identical(
  bfit_upld, data.frame(name = "bacteria_empty.rds", type = "", datapath = "0.rds")
))
app$snapshot(items = list(input = setdiff(app$listWidgets()$input, "brmsfit_upload"),
                          output = setdiff(app$listWidgets()$output, "fit_date"),
                          export = TRUE),
             filename = "post_empty.json")
