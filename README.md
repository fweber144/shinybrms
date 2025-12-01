
# **shinybrms** <img src="man/figures/logo.svg" width="124.44px" height="144px" align="right" />

<br>

## Description

The [R](https://www.R-project.org/) package
[**shinybrms**](https://fweber144.github.io/shinybrms/) provides a
graphical user interface (GUI) for fitting Bayesian regression models
using the R package [**brms**](https://paulbuerkner.com/brms/) which in
turn relies on [Stan](https://mc-stan.org/). The **shinybrms** GUI is a
[**shiny**](https://shiny.rstudio.com/) app.

To get an impression of the **shinybrms** app, see the [Get
Started](https://fweber144.github.io/shinybrms/articles/shinybrms.html)
page.

## Launching the **shinybrms** app

The following two sections describe two ways for launching the
**shinybrms** app, either *with* or *without* the installation of
**shinybrms**. The former is recommended as it offers all advantages
that R packages have (e.g., offline usage). For both ways, the following
steps need to be performed first:

1.  Install R (see the [R homepage](https://www.R-project.org/)).
2.  Install the R package [**rstan**](https://mc-stan.org/rstan/) (see
    the [RStan Getting
    Started](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)
    GitHub page for instructions; make sure to use
    `install.packages("rstan", [...], dependencies = TRUE)` with `[...]`
    as advised there).
3.  If the [**cmdstanr**](https://mc-stan.org/cmdstanr/) backend should
    be used (or if it is needed because the **rstan** backend does not
    work as expected), then **cmdstanr** as well as CmdStan need to be
    installed by following the instructions on the **cmdstanr**
    [homepage](https://mc-stan.org/cmdstanr/). In general, however, the
    **rstan** backend should be sufficient. In the context of
    **shinybrms**, the major advantage of the **cmdstanr** backend
    compared to the **rstan** backend is that in general, it provides a
    speed improvement for the Stan run.

### *With* installation of **shinybrms**

1.  Use one of the following approaches to install the R package
    **shinybrms** either from
    [CRAN](https://CRAN.R-project.org/package=shinybrms) or from
    [GitHub](https://github.com/fweber144/shinybrms). The GitHub version
    might be more recent than the CRAN version, but the CRAN version
    might be more stable. Another decision is whether the example
    datasets from the R packages
    [**lme4**](https://CRAN.R-project.org/package=lme4),
    [**MASS**](https://CRAN.R-project.org/package=MASS), and
    [**rstanarm**](https://mc-stan.org/rstanarm/) should be available.

    - If the example datasets from the R packages **lme4**, **MASS**,
      and **rstanarm** should be available, then the R code for
      installing **shinybrms** from CRAN or GitHub (respectively) is as
      follows:
      - To install **shinybrms** from CRAN:

        ``` r
        install.packages("shinybrms", dependencies = TRUE)
        ```

      - To install **shinybrms** from GitHub:

        ``` r
        if (!requireNamespace("devtools", quietly = TRUE)) {
          install.packages("devtools")
        }
        devtools::install_github("fweber144/shinybrms", dependencies = TRUE)
        ```
    - If the example datasets from the R packages **lme4**, **MASS**,
      and **rstanarm** should *not* be available, then the R code for
      installing **shinybrms** from CRAN or GitHub (respectively) is as
      follows:
      - To install **shinybrms** from CRAN:

        ``` r
        install.packages("shinybrms")
        ```

      - To install **shinybrms** from GitHub:

        ``` r
        if (!requireNamespace("devtools", quietly = TRUE)) {
          install.packages("devtools")
        }
        devtools::install_github("fweber144/shinybrms")
        ```

2.  Launch the **shinybrms** app by either running the following R code:

    ``` r
    library(shinybrms)
    launch_shinybrms()
    ```

    or this R code which ensures that the app opens up in the default
    web browser (helpful, e.g., if [RStudio](https://www.rstudio.com/)
    is used):

    ``` r
    library(shinybrms)
    launch_shinybrms(launch.browser = TRUE)
    ```

### *Without* installation of **shinybrms**

1.  Install the R package **brms**. The following R code may be used for
    this:

    ``` r
    install.packages("brms")
    ```

2.  If the example datasets from the R packages
    [**lme4**](https://CRAN.R-project.org/package=lme4),
    [**MASS**](https://CRAN.R-project.org/package=MASS), and
    [**rstanarm**](https://mc-stan.org/rstanarm/) should be available,
    these packages need to be installed. The following R code may be
    used for this:

    ``` r
    install.packages(c("lme4", "MASS", "rstanarm"))
    ```

3.  Launch the **shinybrms** app directly [from
    GitHub](https://github.com/fweber144/shinybrms/tree/master/inst/shinybrms_app)
    by either running the following R code:

    ``` r
    shiny::runGitHub("fweber144/shinybrms",
                     subdir = "inst/shinybrms_app")
    ```

    or this R code which ensures that the app opens up in the default
    web browser (helpful, e.g., if [RStudio](https://www.rstudio.com/)
    is used):

    ``` r
    shiny::runGitHub("fweber144/shinybrms",
                     subdir = "inst/shinybrms_app",
                     launch.browser = TRUE)
    ```
