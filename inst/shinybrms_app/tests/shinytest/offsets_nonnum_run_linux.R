app <- ShinyDriver$new("../../", options = list(
  shinybrms.allow_upd = getOption("shinybrms.allow_upd", TRUE),
  brms.backend = getOption("brms.backend", "rstan")
))

app$snapshotInit("offsets_nonnum_run_linux", screenshot = FALSE)

app$setInputs(navbar_ID = "Data",
              ex_da_sel = "epilepsy",
              values_ = FALSE)
app$setInputs(navbar_ID = "Likelihood",
              outc_sel = "count",
              dist_sel = "negbinomial",
              values_ = FALSE)
app$setInputs(likelihood_navlist_ID = "Predictors",
              offs_sel = "Trt",
              values_ = FALSE)
app$setInputs(navbar_ID = "Prior", values_ = FALSE)
app$setInputs(navbar_ID = "Posterior",
              show_advOpts = TRUE,
              advOpts_seed = 5493,
              advOpts_cores = 2,
              advOpts_chains = 2,
              advOpts_iter = 500,
              advOpts_adapt_delta = 0.8,
              advOpts_max_treedepth = 10,
              advOpts_save_warmup = FALSE,
              values_ = FALSE)
app$snapshot(filename = "prep.json")
app$setInputs(run_stan = "click", timeout_ = 10000, values_ = FALSE)
app$snapshot(filename = "post.json")
