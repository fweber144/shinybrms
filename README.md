
<br>

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/fweber144/shinybrms.svg?branch=master)](https://travis-ci.org/fweber144/shinybrms)
<!-- badges: end -->

# shinybrms <img src='man/figures/logo.svg' align="right" height="139" />

<br>

## Description

This [R](https://www.R-project.org/) package **shinybrms** provides a
graphical user interface (GUI) for the R package
[**brms**](https://CRAN.R-project.org/package=brms) which allows to fit
Bayesian regression models using [Stan](https://mc-stan.org/) (more
specifically, using its R interface, the R package
[**rstan**](https://CRAN.R-project.org/package=rstan)). The
**shinybrms** GUI is a [Shiny](https://shiny.rstudio.com/) app, i.e. it
was created using the R package
[**shiny**](https://CRAN.R-project.org/package=shiny).

To get an impression of the **shinybrms** app, have a look at [this
page](https://fweber144.github.io/shinybrms/articles/shinybrms.html).
The following text explains how to launch the **shinybrms** app.

## Launching the shinybrms app

There are two options for launching the **shinybrms** app:

  - Install the R package **shinybrms** (either [from
    CRAN](https://CRAN.R-project.org/package=shinybrms) or [from
    GitHub](https://github.com/fweber144/shinybrms)) and then launch the
    **shinybrms** app from the installed package.
  - Launch the **shinybrms** app directly [from
    GitHub](https://github.com/fweber144/shinybrms/tree/master/inst/shinybrms_app).

The first option is described in the following section “*With*
installation of shinybrms”. The second option is described in the
subsequent section “*Without* installation of shinybrms”. In both cases,
you need to perform the following steps first:

1.  Install R (see the [R homepage](https://www.R-project.org/)).
2.  Install the R package **rstan** (see the [“RStan Getting Started”
    GitHub
    page](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)
    for instructions; make sure to use `install.packages("rstan", [...],
    dependencies = TRUE)` with `[...]` as advised on the “RStan Getting
    Started” GitHub page).

### *With* installation of shinybrms

1.  Install the R package **shinybrms** either from CRAN or from GitHub.
    The GitHub version might be more recent than the CRAN version. You
    also need to decide whether you want to use the example datasets
    from the R packages
    [**lme4**](https://CRAN.R-project.org/package=lme4),
    [**MASS**](https://CRAN.R-project.org/package=MASS), and
    [**rstanarm**](https://CRAN.R-project.org/package=rstanarm) or not.
    
      - If you *don’t want* to use the example datasets from the R
        packages **lme4**, **MASS**, and **rstanarm**, then the R code
        for installing **shinybrms** from CRAN and GitHub (respectively)
        is as follows:
          - To install **shinybrms** from CRAN:
            
            ``` r
            install.packages("shinybrms")
            ```
        
          - To install **shinybrms** from GitHub:
            
            ``` r
            if(!requireNamespace("devtools", quietly = TRUE)){
              install.packages("devtools")
            }
            devtools::install_github("fweber144/shinybrms")
            ```
      - If you *want* to use the example datasets from the R packages
        **lme4**, **MASS**, and **rstanarm**, then the R code for
        installing **shinybrms** from CRAN and GitHub (respectively) is
        as follows:
          - To install **shinybrms** from CRAN:
            
            ``` r
            install.packages("shinybrms", dependencies = TRUE)
            ```
        
          - To install **shinybrms** from GitHub:
            
            ``` r
            if(!requireNamespace("devtools", quietly = TRUE)){
              install.packages("devtools")
            }
            devtools::install_github("fweber144/shinybrms", dependencies = TRUE)
            ```

2.  Launch the **shinybrms** app by either running the following R code:
    
    ``` r
    library(shinybrms)
    launch_shinybrms()
    ```
    
    or this R code which ensures that the app opens up in the default
    web browser (helpful e.g. if you are using RStudio):
    
    ``` r
    library(shinybrms)
    launch_shinybrms(launch.browser = TRUE)
    ```

### *Without* installation of shinybrms

1.  Install the R package **brms**. You may use the following R code for
    this:
    
    ``` r
    install.packages("brms")
    ```

2.  If you want to use the example datasets from the R packages
    [**lme4**](https://CRAN.R-project.org/package=lme4),
    [**MASS**](https://CRAN.R-project.org/package=MASS), and
    [**rstanarm**](https://CRAN.R-project.org/package=rstanarm), you
    need to install these packages. You may use the following R code for
    this:
    
    ``` r
    install.packages(c("lme4", "MASS", "rstanarm"))
    ```

3.  Launch the **shinybrms** app directly from GitHub by either running
    the following R code:
    
    ``` r
    shiny::runGitHub("fweber144/shinybrms",
                     subdir = "inst/shinybrms_app")
    ```
    
    or this R code which ensures that the app opens up in the default
    web browser (helpful e.g. if you are using RStudio):
    
    ``` r
    shiny::runGitHub("fweber144/shinybrms",
                     subdir = "inst/shinybrms_app",
                     launch.browser = TRUE)
    ```
