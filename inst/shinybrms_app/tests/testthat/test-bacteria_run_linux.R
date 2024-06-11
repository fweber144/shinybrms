library(shinytest2)

test_that("Migrated shinytest test: bacteria_run_linux.R", {
  app <- AppDriver$new(options = list(
    shinybrms.allow_upd = getOption("shinybrms.allow_upd", TRUE),
    brms.backend = getOption("brms.backend", "rstan")
  ))
  
  # Full model --------------------------------------------------------------
  
  ## Preparation ------------------------------------------------------------
  
  app$set_inputs(navbar_ID = "Data",
                 ex_da_sel = "bacteria")
  app$set_inputs(navbar_ID = "Likelihood",
                 outc_sel = "y",
                 dist_sel = "bernoulli")
  app$set_inputs(likelihood_navlist_ID = "Formula preview")
  app$expect_values(output = c("formula_view"))
  app$set_inputs(likelihood_navlist_ID = "Predictors",
                 pred_mainPL_sel = c("week", "trt"),
                 pred_mainGL_sel = "ID")
  app$set_inputs(likelihood_navlist_ID = "Formula preview")
  app$expect_values(output = c("formula_view"))
  app$set_inputs(likelihood_navlist_ID = "Predictors",
                 pred_int_build = c("week", "trt"),
                 pred_int_add = "click")
  app$set_inputs(likelihood_navlist_ID = "Formula preview")
  app$set_inputs(navbar_ID = "Prior",
                 prior_class_sel = "b",
                 prior_text = "student_t(3, 0, 4)",
                 prior_add = "click")
  app$set_inputs(navbar_ID = "Posterior",
                 show_advOpts = TRUE,
                 advOpts_seed = 6789,
                 advOpts_cores = 2,
                 advOpts_chains = 2,
                 advOpts_iter = 500,
                 advOpts_adapt_delta = 0.8,
                 advOpts_max_treedepth = 10,
                 advOpts_save_warmup = FALSE)
  app$expect_values()
  
  ## Posterior --------------------------------------------------------------
  
  app$set_inputs(run_stan = "click", timeout_ = 1800000)
  app$expect_download("stanout_download",
                      name = file.path("..", paste0("bacteria", "_full.rds")))
  app$set_inputs(posterior_navlist_ID = "MCMC diagnostics",
                 show_general_MCMC_tab = TRUE)
  app$set_inputs(posterior_navlist_ID = "Default summary")
  app$set_inputs(posterior_navlist_ID = "Custom summary",
                 par_sel = "b_week",
                 par_add = "click")
  app$set_inputs(cust_text = "`b_week` + ",
                 par_sel = "b_week:trtdrug",
                 par_add = "click")
  app$set_inputs(cust_act = "click")
  app$set_inputs(cust_text = "`b_week` + ",
                 par_sel = "b_week:trtdrugP",
                 par_add = "click")
  app$set_inputs(cust_name = "week for drugP",
                 cust_act = "click")
  app$set_inputs(posterior_navlist_ID = "Conditional effects",
                 term_sel = "week:trt")
  app$set_inputs(posterior_navlist_ID = "Launch <strong>shinystan</strong>")
  app$expect_values(input = TRUE,
                    output = setdiff(lapply(app$get_values(), names)$output, "fit_date"),
                    export = TRUE)
  
  # No-week model -----------------------------------------------------------
  
  if (getOption("sbtst.run_upd_extend", TRUE)) {
    app$set_inputs(navbar_ID = "Likelihood")
    app$set_inputs(likelihood_navlist_ID = "Predictors",
                   pred_mainPL_sel = "trt")
    app$set_inputs(likelihood_navlist_ID = "Formula preview")
    app$set_inputs(navbar_ID = "Prior",
                   prior_add = "click")
    app$set_inputs(navbar_ID = "Posterior", wait_ = FALSE)
    app$set_inputs(posterior_navlist_ID = "Run Stan")
    app$expect_values(input = TRUE,
                      output = setdiff(lapply(app$get_values(), names)$output, "fit_date"),
                      export = TRUE)
    app$set_inputs(run_stan = "click", timeout_ = 1800000)
    app$expect_download("stanout_download",
                        name = file.path("..", paste0("bacteria", "_noWeek.rds")))
    app$set_inputs(posterior_navlist_ID = "MCMC diagnostics")
    app$set_inputs(posterior_navlist_ID = "Default summary")
    app$set_inputs(posterior_navlist_ID = "Custom summary")
    app$set_inputs(posterior_navlist_ID = "Conditional effects",
                   term_sel = "trt")
    app$set_inputs(posterior_navlist_ID = "Launch <strong>shinystan</strong>")
    app$expect_values(input = TRUE,
                      output = setdiff(lapply(app$get_values(), names)$output, "fit_date"),
                      export = TRUE)
  }
  
  # No-interaction model ----------------------------------------------------
  
  ## Preparation ------------------------------------------------------------
  
  app$set_inputs(navbar_ID = "Likelihood")
  app$set_inputs(likelihood_navlist_ID = "Predictors",
                 pred_mainPL_sel = c("week", "trt"))
  app$set_inputs(likelihood_navlist_ID = "Formula preview")
  app$set_inputs(navbar_ID = "Prior",
                 prior_add = "click")
  app$set_inputs(navbar_ID = "Posterior", wait_ = FALSE)
  app$set_inputs(posterior_navlist_ID = "Run Stan")
  app$expect_values(input = TRUE,
                    output = setdiff(lapply(app$get_values(), names)$output, "fit_date"),
                    export = TRUE)
  
  ## Posterior --------------------------------------------------------------
  
  app$set_inputs(run_stan = "click", timeout_ = 1800000)
  app$expect_download("stanout_download",
                      name = file.path("..", paste0("bacteria", "_noInt.rds")))
  app$set_inputs(posterior_navlist_ID = "MCMC diagnostics")
  app$set_inputs(posterior_navlist_ID = "Default summary")
  app$set_inputs(posterior_navlist_ID = "Custom summary",
                 par_sel = "b_week",
                 par_add = "click")
  app$set_inputs(cust_text = "abs(`b_week`, values_ = FALSE)",
                 cust_name = "absolute week effect",
                 cust_act = "click")
  app$set_inputs(posterior_navlist_ID = "Conditional effects",
                 term_sel = "week")
  app$set_inputs(posterior_navlist_ID = "Launch <strong>shinystan</strong>")
  app$expect_values(input = TRUE,
                    output = setdiff(lapply(app$get_values(), names)$output, "fit_date"),
                    export = TRUE)
  
  # Upload full model -------------------------------------------------------
  
  app$set_inputs(posterior_navlist_ID = "Run Stan")
  app$upload_file(brmsfit_upload = file.path(paste0("bacteria", "_full.rds")))
  app$set_inputs(posterior_navlist_ID = "MCMC diagnostics")
  app$set_inputs(posterior_navlist_ID = "Default summary")
  app$set_inputs(posterior_navlist_ID = "Custom summary",
                 par_sel = "b_week",
                 par_add = "click")
  app$set_inputs(cust_text = "`b_week` + ",
                 par_sel = "b_week:trtdrug",
                 par_add = "click")
  app$set_inputs(cust_act = "click")
  app$set_inputs(cust_text = "`b_week` + ",
                 par_sel = "b_week:trtdrugP",
                 par_add = "click")
  app$set_inputs(cust_name = "week for drugP",
                 cust_act = "click")
  app$set_inputs(posterior_navlist_ID = "Conditional effects",
                 term_sel = "week:trt")
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
  app$expect_values(input = setdiff(lapply(app$get_values(), names)$input, "brmsfit_upload"),
                    output = setdiff(lapply(app$get_values(), names)$output, "fit_date"),
                    export = TRUE)
  
  # Empty model -------------------------------------------------------------
  
  ## Preparation ------------------------------------------------------------
  
  app$set_inputs(navbar_ID = "Likelihood")
  app$set_inputs(likelihood_navlist_ID = "Predictors",
                 pred_mainPL_sel = character(0))
  app$set_inputs(likelihood_navlist_ID = "Formula preview")
  app$set_inputs(navbar_ID = "Prior")
  app$set_inputs(navbar_ID = "Posterior", wait_ = FALSE)
  app$set_inputs(posterior_navlist_ID = "Run Stan")
  app$expect_values(input = setdiff(lapply(app$get_values(), names)$input, "brmsfit_upload"),
                    output = setdiff(lapply(app$get_values(), names)$output, "fit_date"),
                    export = TRUE)
  
  ## Posterior --------------------------------------------------------------
  
  app$set_inputs(run_stan = "click", timeout_ = 1800000)
  app$expect_download("stanout_download",
                      name = file.path("..", paste0("bacteria", "_empty.rds")))
  app$set_inputs(posterior_navlist_ID = "MCMC diagnostics")
  app$set_inputs(posterior_navlist_ID = "Default summary")
  app$set_inputs(posterior_navlist_ID = "Custom summary",
                 par_sel = "b_Intercept",
                 par_add = "click")
  app$set_inputs(cust_text = "`b_Intercept`^2",
                 cust_name = "square intercept",
                 cust_act = "click")
  app$set_inputs(posterior_navlist_ID = "Conditional effects")
  app$set_inputs(posterior_navlist_ID = "Launch <strong>shinystan</strong>")
  app$expect_values(input = setdiff(lapply(app$get_values(), names)$input, "brmsfit_upload"),
                    output = setdiff(lapply(app$get_values(), names)$output, "fit_date"),
                    export = TRUE)
})
