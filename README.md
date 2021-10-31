
# **shinybrms** <img src='man/figures/logo.svg' align="right" height="139" />

<br>

## Description

The [R](https://www.R-project.org/) package
[**shinybrms**](https://fweber144.github.io/shinybrms/) provides a
graphical user interface (GUI) for fitting Bayesian regression models
using the R package [**brms**](https://paul-buerkner.github.io/brms/)
which in turn relies on [Stan](https://mc-stan.org/). The **shinybrms**
GUI is a [**shiny**](https://shiny.rstudio.com/) app.

To get an impression of the **shinybrms** app, have a look at [this
page](https://fweber144.github.io/shinybrms/articles/shinybrms.html).
The following text explains how to launch the **shinybrms** app (and
also how to install it, if necessary).

## Launching the **shinybrms** app

The following two sections describe two ways for launching the
**shinybrms** app, either *with* or *without* the installation of
**shinybrms**. The former is recommended as it offers all advantages
that R packages have (e.g., offline usage). For both ways, you need to
perform the following steps first:

1.  Install R (see the [R homepage](https://www.R-project.org/)).
2.  Install the R package [**rstan**](https://mc-stan.org/rstan/) (see
    the [“RStan Getting Started” GitHub
    page](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)
    for instructions; make sure to use
    `install.packages("rstan", [...], dependencies = TRUE)` with `[...]`
    as advised on the “RStan Getting Started” GitHub page).
3.  If you want to be able to use the
    [**cmdstanr**](https://mc-stan.org/cmdstanr/) backend (or if you
    need it because the **rstan** backend doesn’t work as expected),
    then you need to install **cmdstanr** as well as CmdStan by
    following the instructions on the [**cmdstanr**
    homepage](https://mc-stan.org/cmdstanr/). In general, the **rstan**
    backend should be sufficient, though. In the context of
    **shinybrms**, the major advantage of the **cmdstanr** backend is a
    (generally) faster Stan run.

### *With* installation of **shinybrms**

1.  Use one of the following approaches to install the R package
    **shinybrms** either from
    [CRAN](https://CRAN.R-project.org/package=shinybrms) or from
    [GitHub](https://github.com/fweber144/shinybrms). The GitHub version
    might be more recent than the CRAN version, but the CRAN version
    might be more stable. You also need to decide whether you want to
    use the example datasets from the R packages
    [**lme4**](https://CRAN.R-project.org/package=lme4),
    [**MASS**](https://CRAN.R-project.org/package=MASS), and
    [**rstanarm**](https://mc-stan.org/rstanarm/) or not.

    -   If you want to use the example datasets from the R packages
        **lme4**, **MASS**, and **rstanarm**, then the R code for
        installing **shinybrms** from CRAN and GitHub (respectively) is
        as follows:
        -   To install **shinybrms** from CRAN:

            ``` r
            install.packages("shinybrms", dependencies = TRUE)
            ```

        -   To install **shinybrms** from GitHub:

            ``` r
            if (!requireNamespace("devtools", quietly = TRUE)) {
              install.packages("devtools")
            }
            devtools::install_github("fweber144/shinybrms", dependencies = TRUE)
            ```
    -   If you *don’t* want to use the example datasets from the R
        packages **lme4**, **MASS**, and **rstanarm**, then the R code
        for installing **shinybrms** from CRAN and GitHub (respectively)
        is as follows:
        -   To install **shinybrms** from CRAN:

            ``` r
            install.packages("shinybrms")
            ```

        -   To install **shinybrms** from GitHub:

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
    web browser (helpful, e.g., if you are using
    [RStudio](https://www.rstudio.com/)):

    ``` r
    library(shinybrms)
    launch_shinybrms(launch.browser = TRUE)
    ```

### *Without* installation of **shinybrms**

1.  Install the R package **brms**. You may use the following R code for
    this:

    ``` r
    install.packages("brms")
    ```

2.  If you want to use the example datasets from the R packages
    [**lme4**](https://CRAN.R-project.org/package=lme4),
    [**MASS**](https://CRAN.R-project.org/package=MASS), and
    [**rstanarm**](https://mc-stan.org/rstanarm/), you need to install
    these packages. You may use the following R code for this:

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
    web browser (helpful, e.g., if you are using
    [RStudio](https://www.rstudio.com/)):

    ``` r
    shiny::runGitHub("fweber144/shinybrms",
                     subdir = "inst/shinybrms_app",
                     launch.browser = TRUE)
    ```
