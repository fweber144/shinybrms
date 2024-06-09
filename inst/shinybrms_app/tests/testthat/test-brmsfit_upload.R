library(shinytest2)

test_that("Upload of an existing `brmsfit` for the \"bacteria\" example", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  skip_if_not_installed("MASS")
  
  app <- AppDriver$new(
    variant = paste0(
      platform_variant(r_version = FALSE), "_check",
      substr(
        # `!identical(Sys.getenv("_R_CHECK_FORCE_SUGGESTS_"), "")` means we are
        # (most probably) in an `R CMD check` run:
        as.character(!identical(Sys.getenv("_R_CHECK_FORCE_SUGGESTS_"), "")),
        1,
        1
      )
    ),
    expect_values_screenshot_args = FALSE,
    options = list(shinybrms.allow_upd = getOption("shinybrms.allow_upd", TRUE),
                   brms.backend = getOption("brms.backend", "rstan"))
  )
  
  app$set_window_size(height = 1024, width = 768)
  
  app$set_inputs(advOpts_cores = 2, wait_ = FALSE)
  
  app$set_inputs(navbar_ID = "Posterior")
  app$wait_for_idle()
  
  # Upload full model -------------------------------------------------------
  
  app$upload_file(brmsfit_upload = file.path("bacteria_full.rds"))
  app$set_inputs(posterior_navlist_ID = "MCMC diagnostics")
  app$set_inputs(show_general_MCMC_tab = TRUE)
  app$set_inputs(posterior_navlist_ID = "Default summary")
  app$set_inputs(posterior_navlist_ID = "Custom summary")
  app$set_inputs(par_sel = "b_week")
  app$set_inputs(par_add = "click")
  app$set_inputs(cust_text = "`b_week` + ")
  app$set_inputs(par_sel = "b_week:trtdrug")
  app$set_inputs(par_add = "click")
  app$set_inputs(cust_act = "click")
  app$set_inputs(cust_text = "`b_week` + ")
  app$set_inputs(par_sel = "b_week:trtdrugP")
  app$set_inputs(par_add = "click")
  app$set_inputs(cust_name = "week for drugP")
  app$set_inputs(cust_act = "click")
  app$set_inputs(posterior_navlist_ID = "Conditional effects")
  app$set_inputs(term_sel = "week:trt")
  app$set_inputs(posterior_navlist_ID = "Launch <strong>shinystan</strong>")
  ### Needed because `brmsfit_upload`'s element `"size"` is highly sensitive to
  ### even small changes (like the date stored in the `brmsfit`, etc.); note that
  ### app$getValue() does not work here:
  bfit_upld <- app$get_values(input = "brmsfit_upload",
                              output = FALSE,
                              export = FALSE)$input$brmsfit_upload
  bfit_upld <- bfit_upld[, setdiff(names(bfit_upld), "size"), drop = FALSE]
  stopifnot(identical(
    bfit_upld,
    data.frame(name = "bacteria_full.rds", type = "", datapath = "0.rds")
  ))
  app$expect_values(input = setdiff(lapply(app$get_values(), names)$input, "brmsfit_upload"),###
                    output = setdiff(lapply(app$get_values(), names)$output, "fit_date"),
                    export = TRUE,
                    name = "post_full.json")
  
  # Upload no-interaction model ---------------------------------------------
  
  app$set_inputs(posterior_navlist_ID = "Run Stan")
  app$upload_file(brmsfit_upload = file.path("bacteria_noInt.rds"))
  app$set_inputs(posterior_navlist_ID = "MCMC diagnostics")
  app$set_inputs(posterior_navlist_ID = "Default summary")
  app$set_inputs(posterior_navlist_ID = "Custom summary")
  app$set_inputs(par_sel = "b_week")
  app$set_inputs(par_add = "click")
  app$set_inputs(cust_text = "abs(`b_week`)")
  app$set_inputs(cust_name = "absolute week effect")
  app$set_inputs(cust_act = "click")
  app$set_inputs(posterior_navlist_ID = "Conditional effects")
  app$set_inputs(term_sel = "week")
  app$set_inputs(posterior_navlist_ID = "Launch <strong>shinystan</strong>")
  bfit_upld <- app$get_values(input = "brmsfit_upload",
                              output = FALSE,
                              export = FALSE)$input$brmsfit_upload
  bfit_upld <- bfit_upld[, setdiff(names(bfit_upld), "size"), drop = FALSE]
  stopifnot(identical(
    bfit_upld,
    data.frame(name = "bacteria_noInt.rds", type = "", datapath = "0.rds")
  ))
  app$expect_values(input = setdiff(lapply(app$get_values(), names)$input, "brmsfit_upload"),
                    output = setdiff(lapply(app$get_values(), names)$output, "fit_date"),
                    export = TRUE,
                    name = "post_noInt.json")
  
  # Click "Run Stan" button -------------------------------------------------
  
  app$set_inputs(posterior_navlist_ID = "Run Stan", wait_ = FALSE)
  app$set_inputs(run_stan = "click", timeout_ = 10000)
  bfit_upld <- app$get_values(input = "brmsfit_upload",
                              output = FALSE,
                              export = FALSE)$input$brmsfit_upload
  bfit_upld <- bfit_upld[, setdiff(names(bfit_upld), "size"), drop = FALSE]
  stopifnot(identical(
    bfit_upld,
    data.frame(name = "bacteria_noInt.rds", type = "", datapath = "0.rds")
  ))
  
  # Upload no-week model ----------------------------------------------------
  
  if (getOption("sbtst.run_upd_extend", TRUE)) {
    app$upload_file(brmsfit_upload = file.path("bacteria_noWeek.rds"))
    app$set_inputs(posterior_navlist_ID = "MCMC diagnostics")
    app$set_inputs(posterior_navlist_ID = "Default summary")
    app$set_inputs(posterior_navlist_ID = "Custom summary")
    app$set_inputs(posterior_navlist_ID = "Conditional effects")
    app$set_inputs(term_sel = "trt")
    app$set_inputs(posterior_navlist_ID = "Launch <strong>shinystan</strong>")
    bfit_upld <- app$get_values(input = "brmsfit_upload",
                                output = FALSE,
                                export = FALSE)$input$brmsfit_upload
    bfit_upld <- bfit_upld[, setdiff(names(bfit_upld), "size"), drop = FALSE]
    stopifnot(identical(
      bfit_upld,
      data.frame(name = "bacteria_noWeek.rds", type = "", datapath = "0.rds")
    ))
    app$expect_values(input = setdiff(lapply(app$get_values(), names)$input, "brmsfit_upload"),
                      output = setdiff(lapply(app$get_values(), names)$output, "fit_date"),
                      export = TRUE,
                      name = "post_noWeek.json")
  }
  
  # Upload empty model ------------------------------------------------------
  
  app$upload_file(brmsfit_upload = file.path("bacteria_empty.rds"))
  app$set_inputs(posterior_navlist_ID = "MCMC diagnostics")
  app$set_inputs(posterior_navlist_ID = "Default summary")
  app$set_inputs(posterior_navlist_ID = "Custom summary")
  app$set_inputs(par_sel = "b_Intercept")
  app$set_inputs(par_add = "click")
  app$set_inputs(cust_text = "`b_Intercept`^2")
  app$set_inputs(cust_name = "square intercept")
  app$set_inputs(cust_act = "click")
  app$set_inputs(posterior_navlist_ID = "Conditional effects")
  app$set_inputs(posterior_navlist_ID = "Launch <strong>shinystan</strong>")
  bfit_upld <- app$get_values(input = "brmsfit_upload",
                              output = FALSE,
                              export = FALSE)$input$brmsfit_upload
  bfit_upld <- bfit_upld[, setdiff(names(bfit_upld), "size"), drop = FALSE]
  stopifnot(identical(
    bfit_upld,
    data.frame(name = "bacteria_empty.rds", type = "", datapath = "0.rds")
  ))
  app$expect_values(input = setdiff(lapply(app$get_values(), names)$input, "brmsfit_upload"),
                    output = setdiff(lapply(app$get_values(), names)$output, "fit_date"),
                    export = TRUE,
                    name = "post_empty.json")
  
  # Stop running the app ----------------------------------------------------
  
  app$stop()
})
