
<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->

# News for ‘shinybrms’

## shinybrms 1.0.1.9000

### Major changes

  - Added the possibility to include varying effects.

### Minor changes

  - It is now possible to remove interaction terms one by one.
  - Restricted the choices for the outcome and the predictor selections
    (e.g. remove the outcome from the list of possible predictor
    variables).
  - Removed some example datasets and added new ones.
  - Automatically create the R objects needed for the posterior
    predictive checks in **shinystan**.
  - Added the possibility to download the matrix of posterior draws as a
    CSV file.
  - For the “Advanced options” (for **brms** and Stan): Use explicit
    default values and show them to the user. Set `adapt_delta` per
    default to 0.95 as done in package **rstanarm**. Set `max_treedepth`
    per default to 15 as done for most models in **rstanarm**.
  - User interface (UI): Numerous minor improvements, especially
    concerning button labels, input fields, help texts, and
    notifications.
  - Removed argument `launch.browser` from
    `shinybrms::launch_shinybrms()` so that the default from
    `shiny::runApp()` is used.
  - Changed the way how RStudio’s default setting for the global option
    `browser` is avoided. Most importantly, option
    `shinybrms.RStudio_browser` was removed and replaced by the two new
    options `shinybrms.prog_browser` and `shinybrms.shinystan_browser`.
  - Allowed \*.dat files in the file upload.

### Bug fixes

  - Fixed a bug making it possible to add an already existing
    interaction term with a different order of the involved predictor
    variables.
  - Fixed a bug making it possible to add an interaction term and then
    remove the main effect of an involved predictor variable while
    keeping the interaction term involving it.
  - Fixed a bug leading to the automatic removal of selected variables
    (outcome and predictors) under certain circumstances.
  - Fixed a bug making it impossible to select a new dataset after
    having selected the outcome and the distributional family for an
    older dataset.
  - Fixed a bug preventing the progress file from opening up on Linux
    (when started interactively from the terminal).
  - Ensured that R CMD CHECK is passed on R 4.0.0.

## shinybrms 1.0.1

### Bug fixes

  - Example datasets:
      - Added packages required for some example datasets to the
        “Suggests” list.
      - Check for availability of packages required for some example
        datasets.
      - Added links to the online example datasets on page “Links”.

## shinybrms 1.0.0 (only GitHub)

First release. Offers support for Bayesian regression models with a
Gaussian, Bernoulli, or negative binomial distribution for the
(univariate) outcome. For the predictors, only nonvarying (a.k.a.
population-level or “fixed”) effects are supported. Varying (a.k.a.
group-level or “random”) effects are not supported yet. Neither
supported are most of
[**brms**](https://CRAN.R-project.org/package=brms)’s other features,
like monotonic effects for ordinal predictors or non-linear effects.
Interactions are supported, though. For the inspection of the output,
only a short summary (from `brms::summary.brmsfit()`) and the
possibility to launch
[**shinystan**](https://CRAN.R-project.org/package=shinystan) is
offered.
