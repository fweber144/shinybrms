library(shinytest2)

test_that("Stan run for the \"bacteria\" example", {
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
  
  # Full model --------------------------------------------------------------
  
  ## Preparation ------------------------------------------------------------
  
  app$set_inputs(navbar_ID = "Data")
  app$set_inputs(ex_da_sel = "bacteria")
  app$set_inputs(navbar_ID = "Likelihood")
  app$set_inputs(outc_sel = "y")
  app$set_inputs(dist_sel = "bernoulli")
  app$set_inputs(likelihood_navlist_ID = "Formula preview")
  app$expect_values(output = c("formula_view"))
  app$set_inputs(likelihood_navlist_ID = "Predictors")
  app$set_inputs(pred_mainPL_sel = c("week", "trt"))
  app$set_inputs(pred_mainGL_sel = "ID")
  app$set_inputs(likelihood_navlist_ID = "Formula preview")
  app$expect_values(output = c("formula_view"))
  app$set_inputs(likelihood_navlist_ID = "Predictors")
  app$set_inputs(pred_int_build = c("week", "trt"))
  app$set_inputs(pred_int_add = "click")
  app$set_inputs(likelihood_navlist_ID = "Formula preview")
  app$set_inputs(navbar_ID = "Prior")
  app$set_inputs(prior_class_sel = "b")
  app$set_inputs(prior_text = "student_t(3, 0, 4)")
  app$set_inputs(prior_add = "click")
  app$set_inputs(navbar_ID = "Posterior")
  app$set_inputs(show_advOpts = TRUE)
  app$set_inputs(advOpts_seed = 6789)
  app$set_inputs(advOpts_cores = 2)
  app$set_inputs(advOpts_chains = 2)
  app$set_inputs(advOpts_iter = 500)
  app$set_inputs(advOpts_adapt_delta = 0.8)
  app$set_inputs(advOpts_max_treedepth = 10)
  app$set_inputs(advOpts_save_warmup = FALSE)
  app$expect_values(name = "prep_full.json")
  
  ## Posterior --------------------------------------------------------------
  
  app$set_inputs(run_stan = "click", timeout_ = 1800000)
  app$get_download("stanout_download",
                   filename = file.path(".", paste0("bacteria", "_full.rds")))
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
  app$expect_values(input = TRUE,
                    output = setdiff(lapply(app$get_values(), names)$output, "fit_date"),
                    export = TRUE,
                    name = "post_full.json")
  
  # No-week model -----------------------------------------------------------
  
  if (getOption("sbtst.run_upd_extend", TRUE)) {
    ## Preparation ------------------------------------------------------------
    app$set_inputs(navbar_ID = "Likelihood")
    app$set_inputs(likelihood_navlist_ID = "Predictors")
    app$set_inputs(pred_mainPL_sel = "trt")
    app$set_inputs(likelihood_navlist_ID = "Formula preview")
    app$set_inputs(navbar_ID = "Prior")
    app$set_inputs(prior_add = "click")
    app$set_inputs(navbar_ID = "Posterior", wait_ = FALSE)
    app$set_inputs(posterior_navlist_ID = "Run Stan")
    app$expect_values(input = TRUE,
                      output = setdiff(lapply(app$get_values(), names)$output, "fit_date"),
                      export = TRUE,
                      name = "prep_noWeek.json")
    
    ## Posterior --------------------------------------------------------------
    
    app$set_inputs(run_stan = "click", timeout_ = 1800000)
    app$get_download("stanout_download",
                     filename = file.path(".", paste0("bacteria", "_noWeek.rds")))
    app$set_inputs(posterior_navlist_ID = "MCMC diagnostics")
    app$set_inputs(posterior_navlist_ID = "Default summary")
    app$set_inputs(posterior_navlist_ID = "Custom summary")
    app$set_inputs(posterior_navlist_ID = "Conditional effects")
    app$set_inputs(term_sel = "trt")
    app$set_inputs(posterior_navlist_ID = "Launch <strong>shinystan</strong>")
    app$expect_values(input = TRUE,
                      output = setdiff(lapply(app$get_values(), names)$output, "fit_date"),
                      export = TRUE,
                      name = "post_noWeek.json")
  }
  
  # No-interaction model ----------------------------------------------------
  
  ## Preparation ------------------------------------------------------------
  
  app$set_inputs(navbar_ID = "Likelihood")
  app$set_inputs(likelihood_navlist_ID = "Predictors")
  app$set_inputs(pred_mainPL_sel = c("week", "trt"))
  app$set_inputs(likelihood_navlist_ID = "Formula preview")
  app$set_inputs(navbar_ID = "Prior")
  app$set_inputs(prior_add = "click")
  app$set_inputs(navbar_ID = "Posterior", wait_ = FALSE)
  app$set_inputs(posterior_navlist_ID = "Run Stan")
  app$expect_values(input = TRUE,
                    output = setdiff(lapply(app$get_values(), names)$output, "fit_date"),
                    export = TRUE,
                    name = "prep_noInt.json")
  
  ## Posterior --------------------------------------------------------------
  
  app$set_inputs(run_stan = "click", timeout_ = 1800000)
  app$get_download("stanout_download",
                   filename = file.path(".", paste0("bacteria", "_noInt.rds")))
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
  app$expect_values(input = TRUE,
                    output = setdiff(lapply(app$get_values(), names)$output, "fit_date"),
                    export = TRUE,
                    name = "post_noInt.json")
  
  # Upload full model -------------------------------------------------------
  
  app$set_inputs(posterior_navlist_ID = "Run Stan")
  app$upload_file(brmsfit_upload = file.path(paste0("bacteria", "_full.rds")))
  app$set_inputs(posterior_navlist_ID = "MCMC diagnostics")
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
    data.frame(name = paste0("bacteria", "_full.rds"), type = "", datapath = "0.rds")
  ))
  app$expect_values(input = setdiff(lapply(app$get_values(), names)$input, "brmsfit_upload"),###
                    output = setdiff(lapply(app$get_values(), names)$output, "fit_date"),
                    export = TRUE,
                    name = "post_full_upload.json")
  
  # Empty model -------------------------------------------------------------
  
  ## Preparation ------------------------------------------------------------
  
  app$set_inputs(navbar_ID = "Likelihood")
  app$set_inputs(likelihood_navlist_ID = "Predictors")
  app$set_inputs(pred_mainPL_sel = character(0))
  app$set_inputs(likelihood_navlist_ID = "Formula preview")
  app$set_inputs(navbar_ID = "Prior")
  app$set_inputs(navbar_ID = "Posterior", wait_ = FALSE)
  app$set_inputs(posterior_navlist_ID = "Run Stan")
  app$expect_values(input = setdiff(lapply(app$get_values(), names)$input, "brmsfit_upload"),
                    output = setdiff(lapply(app$get_values(), names)$output, "fit_date"),
                    export = TRUE,
                    name = "prep_empty.json")
  
  ## Posterior --------------------------------------------------------------
  
  app$set_inputs(run_stan = "click", timeout_ = 1800000)
  app$get_download("stanout_download",
                   filename = file.path(".", paste0("bacteria", "_empty.rds")))
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
  app$expect_values(input = setdiff(lapply(app$get_values(), names)$input, "brmsfit_upload"),
                    output = setdiff(lapply(app$get_values(), names)$output, "fit_date"),
                    export = TRUE,
                    name = "post_empty.json")
  
  # Stop running the app ----------------------------------------------------
  
  app$stop()
})
