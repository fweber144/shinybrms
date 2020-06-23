# shinybrms 1.1.0.9000

## Major changes

  - Show Hamiltonian Monte Carlo (HMC) diagnostics in the “Output” panel
    on page “Posterior”.

## Minor changes

  - Added a package website built with `pkgdown`.
  - Added a package logo created with `hexSticker`.
  - Require R \>= 3.6.0.
  - Updated the `README` file.
  - Updated the “Description” field in the `DESCRIPTION` file.

## Bug fixes

  - Try to fix a NOTE occurring in some CRAN checks.

# shinybrms 1.1.0

## Major changes

  - Added the possibility to include varying effects.

## Minor changes

  - All predictor terms (main effects as well as interactions) are now
    shown in a preview table.
  - It is now possible to remove interaction terms one by one.
  - Restricted the choices for the outcome and the predictor selections
    (e.g. remove the outcome from the list of possible predictor
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

## Bug fixes

“Under the hood”, there have been many changes which should make the
**shinybrms** app more robust. In particular, the following bugs have
been fixed:

  - Fixed a bug causing the app to crash if the dataset was changed
    under certain circumstances.
  - Fixed a bug making it possible to keep the outcome and the predictor
    variables selected when clearing the selection of an example
    dataset.
  - Fixed a bug making it possible to clear the selection of an example
    dataset, the outcome, or the distributional family, but to keep
    showing the default priors and the custom priors for the former
    selection.
  - Fixed a bug making it possible to add an already existing
    interaction term with a different order of the involved predictor
    variables.
  - Fixed a bug making it possible to add an interaction term and then
    remove the main effect of an involved predictor variable while
    keeping the interaction term involving it.
  - Fixed a bug causing the app to crash when selecting a new dataset
    after having constructed the model formula and chosen the
    distributional family for an older dataset.
  - Fixed a bug preventing the progress file from opening up on Linux
    (when started interactively from the terminal).

# shinybrms 1.0.1

## Bug fixes

  - Example datasets:
      - Added packages required for some example datasets to the
        “Suggests” list.
      - Check for availability of packages required for some example
        datasets.
      - Added links to the online example datasets on page “Links”.

# shinybrms 1.0.0 (only GitHub)

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
