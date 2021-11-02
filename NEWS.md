# **shinybrms** 1.6.0

## Major changes

* Replace the term "nonpooled" (effects) by "pooled" (effects). This confounding
was introduced by the replacement of "nonvarying" (effects) by "nonpooled"
(effects) in v1.3.0 (which was obviously incorrect since "pooled" is the
opposite of "varying").
* Allow the **cmdstanr** backend for **brms**. The global option `brms.backend`
can be used to control the default backend selected in the "Advanced options" in
the **shinybrms** app (see `?launch_shinybrms` for details).
* If a previous `brmsfit` exists (i.e., it is currently presented on page
"Posterior"), then---if possible---a click on the "Run Stan" button causes this
previous `brmsfit` to get updated using `brms:::update.brmsfit()`, saving the
compilation time. The fact that `brms:::update.brmsfit()` currently does not
recompute the default priors if the dataset has changed, combined with the
flexibility that *any* `brmsfit` may be uploaded in **shinybrms** (even ones
which were not fitted through **shinybrms**), an uploaded `brmsfit` may not be
updated. This updating of a previous `brmsfit` may be turned off using the
global option `shinybrms.allow_upd` (see `?launch_shinybrms` for details).
* Allow the upload of a previously created `brmsfit` object (page "Posterior",
tab "Run Stan", panel "Run Stan"). This required increasing the default size
limit for file uploads (global option `shiny.maxRequestSize`; now at 50 MB if
not set by the user).
* Allow for `offset()` terms without the need to resort to the previous
workaround based on a `constant()` prior (see **brms**'s GitHub issue #923
solved in **brms** v2.16.0). This increases the required **brms** version to at
least 2.16.0.

## Minor changes

* UI: The width of some UI elements in panel "Run Stan" has been adjusted.
* The "Default summary" from page "Posterior" may now be downloaded (as a text
file).
* The download of the MCMC diagnostics was moved from tab "Run Stan" to tab
"MCMC diagnostics" (both on page "Posterior").
* Page "Posterior" (tab "Run Stan", panel "Run Stan") now also shows important
software versions used for the Stan run.
* Improved documentation of global options used by **shinybrms**.
* Theme: Some colors have been slightly changed to match the overall theme
better.
* UI: In case of a failed file upload, a modal dialog is now shown (instead of a
notification in the lower right corner).
* UI: Minor formatting and wording improvements in help texts and other UI
elements.

## Bug fixes

* Fix an error for Stan runs with advanced option "Open progress" set to
`FALSE`.
* Fix a typo in the "Get started" vignette shown on the **pkgdown** website.
(The scale parameter for the Student-*t* prior is 4.)
* Clear the input field for the name of a custom summary expression as soon as
new Stan results are obtained.

# **shinybrms** 1.5.2

## Minor changes

* UI: Minor wording improvements.
* Set a minimum width for the output on tab "Default summary".
* Append trailing zeros in the rounding of the E-BFMI values.

## Bug fixes

* UI: Fixed some typos and incorrect spacing in the text.
* Avoid the deprecation warning for `brms::parnames()`.
* Fixed a NOTE occurring in some CRAN checks.

# **shinybrms** 1.5.1

## Minor changes

* Updated some references.
* Now using `parallel::detectCores(logical = FALSE)` instead of
`parallel::detectCores()` when determining the default number of cores for the
Stan run.
* Improved help texts in the UI.
* Renamed tab "Summary" (page "Posterior") to "Default summary".

# **shinybrms** 1.5.0

## Major changes

* Use the automatic `rstan::rstan_options("javascript" = FALSE)` fix on *all*
platforms (not only Windows) if **rstan** version 2.21.1 or above is used.
* Added support for conditional-effects plots (see
[`brms::conditional_effects()`](https://paul-buerkner.github.io/brms/reference/conditional_effects.brmsfit.html)).
* Added the possibility to perform custom posterior summary analyses, e.g., for
a transformation of parameters or for a sum of parameters.

## Minor changes

* Tab "Summary": Increased the number of printed decimal digits from 2 to 4.
* Tab "Summary": Replaced posterior mean and posterior standard deviation by
posterior median and posterior median absolute deviation, respectively.
* For partially pooled main effects, numeric (including integer) variables are
not allowed anymore.
* Example dataset `kidiq`: Converted column `mom_hs` to factor.
* UI: Minor improvements (e.g., in label and notification texts).
* Minor cosmetic improvement in the CSS theme.
* Included some changes necessary for **brms** version 2.14.0 (and above).
* Changed package description.
* Added CITATION file.
* Minor improvements in the documentation for `launch_shinybrms()`.

## Bug fixes

* Fixed a possible security issue for the text input field "Prior distribution".

# **shinybrms** 1.4.1

## Bug fixes

* Corrected a mistake in the DESCRIPTION file: The contributors and copyright
holders for external components have to be listed among the authors.

# **shinybrms** 1.4.0

## Major changes

* A new theme is used for the **shinybrms** app. This theme is basically the
"United" theme from [Bootswatch](https://github.com/thomaspark/bootswatch)
v3.3.7 with just a few minor modifications. For the font, the "Open Sans" font
from [Google Fonts](https://fonts.google.com/) is used. See the new file
"LICENSE" for details concerning the licenses of these new components.

## Minor changes

* When deselecting a variable from the list for the nonpooled main effects or
deselecting a variable from the list for the partially pooled main effects, the
interactions involving this variable are now reduced to the corresponding
lower-order interactions (instead of being simply removed).
* When updating the selections for the class or the coefficient of a custom
prior, the former value is now selected if it exists among the new choices.
* When specifying a custom prior and switching the class or the coefficient, the
input field for the group is now only cleared (automatically) if necessary.
Before, it was cleared at every change of class or coefficient (provided that
the clearing was allowed, meaning that such a combination was present in the
table of the default priors).
* UI: Minor improvements (e.g., improved button labels and help texts).
* On Windows with **rstan** version \>= 2.21.1: Automatically run
`rstan::rstan_options("javascript" = FALSE)` at the launch of the **shinybrms**
app to prevent occasional crashes of the R session when starting the Stan run.
* UI: Added internal links.

## Bug fixes

* Fixed the bug that when setting the input field "Group" (of a custom prior) to
a nonempty value and later switching back to an empty value, the "Group" was
empty in the UI, but internally, the former value was kept. This bug was solved
by initializing the input field "Group" *with* a choose prompt (which made it
necessary to catch impossible combinations of "Class", "Coefficient", and
"Group" with a user notification).

# **shinybrms** 1.3.0

## Major changes

* Use the term "nonpooled effects" instead of "nonvarying effects" and the term
"partially pooled effects" instead of "varying effects".
* Restructured page "Posterior".
* Up to now, some general MCMC diagnostics (R-hat, bulk-ESS, tail-ESS) were only
included implicitly (as part of the warnings introduced in version 1.2.2),
besides being accessible from within the **shinystan** app. Now, these general
MCMC diagnostics are accessible from within the **shinybrms** app and the
warnings from version 1.2.2 are handled differently.
* Added a page "Home".
* Added a page "References" (listed under "More").
* Removed page "Help" (which was listed under "More"). Its content is now
located at other (more appropriate) places in the **shinybrms** app.
* Added a description how to include an offset. For this, the required version
of **brms** had to be increased to 2.13.3.

## Minor changes

* Minor improvements in the UI (e.g., improved and added help texts and
notifications, but also added hyperlinks to the online documentation of R
functions).
* Added the possibility to download the MCMC diagnostics (including the newly
added general MCMC diagnostics).
* Minor improvements in the README file.
* Minor improvements on pages "About" and "Links" (both listed under "More").
* Added example dataset `roaches` from package **rstanarm**.

## Bug fixes

* Fixed a bug when switching the dataset.

# **shinybrms** 1.2.2 (only GitHub)

## Minor changes

* Minor improvements in the UI.
* Show more warnings from the Stan run. Some of these additional warnings
overlap with the HMC diagnostics added in v1.2.0, but rather show too many
warnings (even if they are partly duplicated) than too few.

## Bug fixes

* Fixed a NOTE occurring in some CRAN checks.

# **shinybrms** 1.2.1

## Minor changes

* Minor improvements in the UI.
* Updated information about the **shinybrms** package and app (e.g., added the
URL of the **shinybrms** [website](https://fweber144.github.io/shinybrms/)).

## Bug fixes

* Fixed a bug causing the sampling progress file not to open up if RStudio's
internal `rs_shinyviewer` was used for option `"browser"`.

# **shinybrms** 1.2.0 (only GitHub)

## Major changes

* Show Hamiltonian Monte Carlo (HMC) diagnostics in the "Output" panel on page
"Posterior".

## Minor changes

* Added a package website built with `pkgdown`.
* Added a package logo created with `hexSticker`.
* Require R \>= 3.6.0.

## Bug fixes

* Fixed a NOTE occurring in some CRAN checks.

# **shinybrms** 1.1.0

## Major changes

* Added the possibility to include varying effects.

## Minor changes

* All predictor terms (main effects as well as interactions) are now shown in a
preview table.
* It is now possible to remove interaction terms one by one.
* Restricted the choices for the outcome and the predictor selections (e.g., the
outcome variable is now automatically removed from the list of possible
predictor variables).
* Removed some example datasets and added new ones.
* Automatically create the R objects needed for the posterior predictive checks
in **shinystan**.
* Added the possibility to download the matrix of posterior draws as a CSV file.
* For the "Advanced options" (for **brms** and Stan): Use explicit default
values and show them to the user. Set `adapt_delta` per default to 0.95 as done
in package **rstanarm**. Set `max_treedepth` per default to 15 as done for most
models in **rstanarm**.
* User interface (UI): Numerous minor improvements, especially concerning button
labels, input fields, help texts, and notifications.
* Removed argument `launch.browser` from `shinybrms::launch_shinybrms()` so that
the default from `shiny::runApp()` is used.
* Changed the way how RStudio's default setting for the global option `browser`
is avoided. Most importantly, option `shinybrms.RStudio_browser` was removed and
replaced by the two new options `shinybrms.prog_browser` and
`shinybrms.shinystan_browser`.
* Allowed \*.dat files in the file upload.

## Bug fixes

"Under the hood", there have been many changes which should make the
**shinybrms** app more stable. In particular, the following bugs have been
fixed:

* Fixed a bug causing the app to crash if the dataset was changed under certain
circumstances.
* Fixed a bug making it possible to keep the outcome and the predictor variables
selected when clearing the selection of an example dataset.
* Fixed a bug making it possible to clear the selection of an example dataset or
the outcome, but to keep showing the default priors and the custom priors for
the former selection.
* Fixed a bug making it possible to add an already existing interaction term
with a different order of the involved predictor variables.
* Fixed a bug making it possible to add an interaction term and then remove the
main effect of an involved predictor variable while keeping the interaction term
involving it.
* Fixed a bug causing the app to crash when selecting a new dataset after having
constructed the model formula and chosen the distributional family for an older
dataset.
* Fixed a bug preventing the progress file from opening up on Linux (when
started interactively from the terminal).

# **shinybrms** 1.0.1

## Bug fixes

* Example datasets:
    + Added packages required for some example datasets to the
    "Suggests" list.
    + Check for availability of packages required for some example
    datasets.
    + Added links to the online example datasets on page "Links".

# **shinybrms** 1.0.0 (only GitHub)

First release. Offers support for Bayesian regression models with a Gaussian,
Bernoulli, or negative binomial distribution for the (univariate) outcome. For
the predictors, only nonvarying (a.k.a. population-level or "fixed") effects are
supported. Varying (a.k.a. group-level or "random") effects are not supported
yet. Neither supported are most of
[**brms**](https://CRAN.R-project.org/package=brms)'s other features, like
monotonic effects for ordinal predictors or non-linear effects. Interactions are
supported, though. For the inspection of the output, only a short summary (from
`brms::summary.brmsfit()`) and the possibility to launch
[**shinystan**](https://CRAN.R-project.org/package=shinystan) is offered.
