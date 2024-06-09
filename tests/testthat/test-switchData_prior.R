library(shinytest2)

test_that("Migrated shinytest test: switchData_prior.R", {
  app <- AppDriver$new()

  app$set_inputs(advOpts_cores = 2, wait_ = FALSE)

  app$set_inputs(navbar_ID = "Data", ex_da_sel = "Arabidopsis")
  app$set_inputs(navbar_ID = "Likelihood", outc_sel = "total.fruits",
    dist_sel = "negbinomial")
  app$set_inputs(navbar_ID = "Prior")
  app$expect_values()
  app$set_inputs(navbar_ID = "Data", ex_da_sel = "bacteria")
  app$expect_values()
  app$set_inputs(navbar_ID = "Prior")
  app$expect_values()
})
