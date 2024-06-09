library(shinytest2)

test_that("Migrated shinytest test: brmsfit_upload_run_windows.R", {
  app <- AppDriver$new(options = list(shinybrms.allow_upd = getOption("shinybrms.allow_upd",
                                                                      TRUE), brms.backend = getOption("brms.backend", "rstan")))
  
  
  app$set_inputs(advOpts_cores = 2, wait_ = FALSE)
  
  app$set_inputs(navbar_ID = "Posterior")
  
  # Upload full model -------------------------------------------------------
  
  # app$upload_file(brmsfit_upload = file.path("bacteria_full.rds"))
  app$set_inputs(posterior_navlist_ID = "MCMC diagnostics", show_general_MCMC_tab = TRUE)
  app$set_inputs(posterior_navlist_ID = "Default summary")
  app$set_inputs(posterior_navlist_ID = "Custom summary", par_sel = "b_week",
                 par_add = "click")
  app$set_inputs(cust_text = "`b_week` + ", par_sel = "b_week:trtdrug",
                 par_add = "click")
  app$set_inputs(cust_act = "click")
  app$set_inputs(cust_text = "`b_week` + ", par_sel = "b_week:trtdrugP",
                 par_add = "click")
  app$set_inputs(cust_name = "week for drugP", cust_act = "click")
  app$set_inputs(posterior_navlist_ID = "Conditional effects",
                 term_sel = "week:trt")
  app$set_inputs(posterior_navlist_ID = "Launch <strong>shinystan</strong>")
  ### Needed because `brmsfit_upload`'s element `"size"` is highly sensitive to
  ### even small changes (like the date stored in the `brmsfit`, etc.); note that
  ### app$getValue() does not work here:
  bfit_upld <- app$get_values(input = "brmsfit_upload", output = FALSE,
                              export = FALSE)$input$brmsfit_upload
  bfit_upld <- bfit_upld[, setdiff(names(bfit_upld), "size"), drop = FALSE]
  stopifnot(identical(
    bfit_upld, data.frame(name = "bacteria_full.rds", type = "", datapath = "0.rds")
  ))
  app$expect_values(input = setdiff(lapply(app$get_values(), names)$input,
                                    "brmsfit_upload"), output = setdiff(lapply(app$get_values(),
                                                                               names)$output, "fit_date"), export = TRUE)
  
  # Upload no-interaction model ---------------------------------------------
  
  app$set_inputs(posterior_navlist_ID = "Run Stan")
  # app$upload_file(brmsfit_upload = file.path("bacteria_noInt.rds"))
  app$set_inputs(posterior_navlist_ID = "MCMC diagnostics")
  app$set_inputs(posterior_navlist_ID = "Default summary")
  app$set_inputs(posterior_navlist_ID = "Custom summary", par_sel = "b_week",
                 par_add = "click")
  app$set_inputs(cust_text = "abs(`b_week`, values_ = FALSE)",
                 cust_name = "absolute week effect", cust_act = "click")
  app$set_inputs(posterior_navlist_ID = "Conditional effects",
                 term_sel = "week")
  app$set_inputs(posterior_navlist_ID = "Launch <strong>shinystan</strong>")
  bfit_upld <- app$get_values(input = "brmsfit_upload", output = FALSE,
                              export = FALSE)$input$brmsfit_upload
  bfit_upld <- bfit_upld[, setdiff(names(bfit_upld), "size"), drop = FALSE]
  stopifnot(identical(
    bfit_upld, data.frame(name = "bacteria_noInt.rds", type = "", datapath = "0.rds")
  ))
  app$expect_values(input = setdiff(lapply(app$get_values(), names)$input,
                                    "brmsfit_upload"), output = setdiff(lapply(app$get_values(),
                                                                               names)$output, "fit_date"), export = TRUE)
  
  # Click "Run Stan" button -------------------------------------------------
  
  app$set_inputs(posterior_navlist_ID = "Run Stan", wait_ = FALSE)
  app$set_inputs(run_stan = "click", timeout_ = 10000)
  bfit_upld <- app$get_values(input = "brmsfit_upload", output = FALSE,
                              export = FALSE)$input$brmsfit_upload
  bfit_upld <- bfit_upld[, setdiff(names(bfit_upld), "size"), drop = FALSE]
  stopifnot(identical(
    bfit_upld, data.frame(name = "bacteria_noInt.rds", type = "", datapath = "0.rds")
  ))
  
  # Upload no-week model ----------------------------------------------------
  
  if (getOption("sbtst.run_upd_extend", TRUE)) {
    app$set_inputs(posterior_navlist_ID = "MCMC diagnostics")
    app$set_inputs(posterior_navlist_ID = "Default summary")
    app$set_inputs(posterior_navlist_ID = "Custom summary")
    app$set_inputs(posterior_navlist_ID = "Conditional effects",
                   term_sel = "trt")
    app$set_inputs(posterior_navlist_ID = "Launch <strong>shinystan</strong>")
    bfit_upld <- app$get_values(input = "brmsfit_upload", output = FALSE,
                                export = FALSE)$input$brmsfit_upload
    bfit_upld <- bfit_upld[, setdiff(names(bfit_upld), "size"), drop = FALSE]
    stopifnot(identical(bfit_upld, data.frame(name = "bacteria_noWeek.rds",
                                              type = "", datapath = "0.rds")))
    app$expect_values(input = setdiff(lapply(app$get_values(),
                                             names)$input, "brmsfit_upload"), output = setdiff(lapply(app$get_values(),
                                                                                                      names)$output, "fit_date"), export = TRUE)
  }
  
  # Upload empty model ------------------------------------------------------
  
  # app$upload_file(brmsfit_upload = file.path("bacteria_empty.rds"))
  app$set_inputs(posterior_navlist_ID = "MCMC diagnostics")
  app$set_inputs(posterior_navlist_ID = "Default summary")
  app$set_inputs(posterior_navlist_ID = "Custom summary", par_sel = "b_Intercept",
                 par_add = "click")
  app$set_inputs(cust_text = "`b_Intercept`^2", cust_name = "square intercept",
                 cust_act = "click")
  app$set_inputs(posterior_navlist_ID = "Conditional effects")
  app$set_inputs(posterior_navlist_ID = "Launch <strong>shinystan</strong>")
  bfit_upld <- app$get_values(input = "brmsfit_upload", output = FALSE,
                              export = FALSE)$input$brmsfit_upload
  bfit_upld <- bfit_upld[, setdiff(names(bfit_upld), "size"), drop = FALSE]
  stopifnot(identical(
    bfit_upld, data.frame(name = "bacteria_empty.rds", type = "", datapath = "0.rds")
  ))
  app$expect_values(input = setdiff(lapply(app$get_values(), names)$input,
                                    "brmsfit_upload"), output = setdiff(lapply(app$get_values(),
                                                                               names)$output, "fit_date"), export = TRUE)
})
