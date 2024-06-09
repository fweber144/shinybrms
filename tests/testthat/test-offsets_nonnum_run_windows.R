library(shinytest2)

test_that("Migrated shinytest test: offsets_nonnum_run_windows.R", {
  app <- AppDriver$new(options = list(shinybrms.allow_upd = getOption("shinybrms.allow_upd",
    TRUE), brms.backend = getOption("brms.backend", "rstan")))


  app$set_inputs(navbar_ID = "Data", ex_da_sel = "epilepsy")
  app$set_inputs(navbar_ID = "Likelihood", outc_sel = "count",
    dist_sel = "negbinomial")
  app$set_inputs(likelihood_navlist_ID = "Predictors", offs_sel = "Trt")
  app$set_inputs(navbar_ID = "Prior")
  app$set_inputs(navbar_ID = "Posterior", show_advOpts = TRUE,
    advOpts_seed = 5493, advOpts_cores = 2, advOpts_chains = 2,
    advOpts_iter = 500, advOpts_adapt_delta = 0.8, advOpts_max_treedepth = 10,
    advOpts_save_warmup = FALSE)
  app$expect_values()
  app$set_inputs(run_stan = "click", timeout_ = 10000)
  app$expect_values()
})
