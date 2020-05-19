
<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/fweber144/shinybrms.svg?branch=master)](https://travis-ci.org/fweber144/shinybrms)
<!-- badges: end -->

# Description

This [R](https://www.R-project.org/) package **shinybrms** provides a
graphical user interface (GUI) for the R package
[**brms**](https://CRAN.R-project.org/package=brms) which allows to fit
Bayesian regression models using [Stan](https://mc-stan.org/) (more
specifically, using its R interface, the R package
[**rstan**](https://CRAN.R-project.org/package=rstan)). The GUI is a
[Shiny](https://shiny.rstudio.com/) app, i.e. it was created using the R
package [**shiny**](https://CRAN.R-project.org/package=shiny).

# Installation / Usage

There are two options for running the **shinybrms** Shiny app:

  - Run the **shinybrms** Shiny app directly [from
    GitHub](https://github.com/fweber144/shinybrms/tree/master/inst/shinybrms_app).
  - Install the R package **shinybrms** (either [from
    GitHub](https://github.com/fweber144/shinybrms) or [from
    CRAN](https://CRAN.R-project.org/package=shinybrms)) and then run
    the **shinybrms** Shiny app from the installed package.

The first option is described in the following section “Without
installation of ‘shinybrms’”. The second option is described in the
subsequent section “With installation of ‘shinybrms’”. In both cases,
you need to perform the following steps first:

1.  Install R (see the [R homepage](https://www.R-project.org/)).
2.  Install the R package **rstan** (see the [“RStan Getting Started”
    GitHub
    page](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)
    for instructions; make sure to use `install.packages("rstan", [...],
    dependencies = TRUE)` with `[...]` as advised on the “RStan Getting
    Started” GitHub page).

## Without installation of ‘shinybrms’

1.  Install the R package **brms**. You may use the following R code for
    this:

<!-- end list -->

``` r
install.packages("brms")
```

2.  If you want to use the example datasets from the R packages
    [**rstanarm**](https://CRAN.R-project.org/package=rstanarm) and
    [**lasso2**](https://CRAN.R-project.org/package=lasso2), you need to
    install these packages. You may use the following R code for this:

<!-- end list -->

``` r
install.packages(c("rstanarm", "lasso2"))
```

3.  Launch the **shinybrms** Shiny app directly from GitHub by running
    the following R code:

<!-- end list -->

``` r
shiny::runGitHub("fweber144/shinybrms",
                 subdir = "inst/shinybrms_app",
                 launch.browser = TRUE)
```

## With installation of ‘shinybrms’

1.  Install the R package **shinybrms** either from GitHub or from CRAN.
    Note that installing from GitHub might give a more recent version of
    **shinybrms** than installing from CRAN. You also need to decide
    whether you want to use the example datasets from the R packages
    [**rstanarm**](https://CRAN.R-project.org/package=rstanarm) and
    [**lasso2**](https://CRAN.R-project.org/package=lasso2) or not.
      - If you *don’t want* to use the example datasets from the R
        packages **rstanarm** and **lasso2**, then the R code for
        installing **shinybrms** from GitHub and CRAN (respectively) is
        as follows:
          - To install **shinybrms** from GitHub:
            
            ``` r
            if(!requireNamespace("devtools", quietly = TRUE)){
              install.packages("devtools")
            }
            devtools::install_github("fweber144/shinybrms")
            ```
        
          - To install **shinybrms** from CRAN:
            
            ``` r
            install.packages("shinybrms")
            ```
      - If you *want* to use the example datasets from the R packages
        **rstanarm** and **lasso2**, then the R code for installing
        **shinybrms** from GitHub and CRAN (respectively) is as follows:
          - To install **shinybrms** from GitHub:
            
            ``` r
            if(!requireNamespace("devtools", quietly = TRUE)){
              install.packages("devtools")
            }
            devtools::install_github("fweber144/shinybrms", dependencies = TRUE)
            ```
        
          - To install **shinybrms** from CRAN:
            
            ``` r
            install.packages("shinybrms", dependencies = TRUE)
            ```
2.  Launch the **shinybrms** Shiny app by running the following R code:

<!-- end list -->

``` r
library(shinybrms)
launch_shinybrms()
```
