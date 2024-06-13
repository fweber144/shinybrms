library(shinytest2)

test_that("nonnumeric offsets don't allow to run Stan, but also don't crash the app on Linux", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  skip_if_not_installed("MASS")
  skip_if_not(identical(.Platform$OS.type, "unix"))
  
  app <- AppDriver$new(
    expect_values_screenshot_args = FALSE,
    options = list(shinybrms.allow_upd = getOption("shinybrms.allow_upd", TRUE),
                   brms.backend = getOption("brms.backend", "rstan"))
  )
  
  app$set_inputs(navbar_ID = "Data",
                 ex_da_sel = "epilepsy")
  app$set_inputs(navbar_ID = "Likelihood",
                 outc_sel = "count",
                 dist_sel = "negbinomial")
  app$set_inputs(likelihood_navlist_ID = "Predictors",
                 offs_sel = "Trt")
  app$set_inputs(navbar_ID = "Prior")
  app$set_inputs(navbar_ID = "Posterior",
                 show_advOpts = TRUE,
                 advOpts_seed = 5493,
                 advOpts_cores = 2,
                 advOpts_chains = 2,
                 advOpts_iter = 500,
                 advOpts_adapt_delta = 0.8,
                 advOpts_max_treedepth = 10,
                 advOpts_save_warmup = FALSE)
  app$expect_values()
  app$set_inputs(run_stan = "click", timeout_ = 10000)
  app$expect_values()
})
