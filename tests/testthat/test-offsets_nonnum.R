library(shinytest2)

test_that("nonnumeric offsets don't allow to run Stan, but also don't crash the app", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  skip_if_not_installed("MASS")
  
  app <- AppDriver$new(
    variant = platform_variant(r_version = FALSE),
    expect_values_screenshot_args = FALSE,
    options = list(shinybrms.allow_upd = getOption("shinybrms.allow_upd", TRUE),
                   brms.backend = getOption("brms.backend", "rstan"))
  )
  
  app$set_inputs(navbar_ID = "Data")
  app$set_inputs(ex_da_sel = "epilepsy")
  app$set_inputs(navbar_ID = "Likelihood")
  app$set_inputs(outc_sel = "count")
  app$set_inputs(dist_sel = "negbinomial")
  app$set_inputs(likelihood_navlist_ID = "Predictors")
  app$set_inputs(offs_sel = "Trt")
  app$set_inputs(navbar_ID = "Prior")
  app$set_inputs(navbar_ID = "Posterior")
  app$set_inputs(show_advOpts = TRUE)
  app$set_inputs(advOpts_seed = 5493)
  app$set_inputs(advOpts_cores = 2)
  app$set_inputs(advOpts_chains = 2)
  app$set_inputs(advOpts_iter = 500)
  app$set_inputs(advOpts_adapt_delta = 0.8)
  app$set_inputs(advOpts_max_treedepth = 10)
  app$set_inputs(advOpts_save_warmup = FALSE)
  app$expect_values(name = "prep.json")
  app$set_inputs(run_stan = "click", timeout_ = 10000)
  app$expect_values(name = "post.json")
  
  app$stop()
})
