app <- ShinyDriver$new("../../", options = list(
  shinybrms.allow_upd = getOption("shinybrms.allow_upd", TRUE),
  brms.backend = getOption("brms.backend", "rstan")
))
tst_prefix <- "bacteria"
source("init_os_dep.R", local = TRUE, echo = FALSE)
