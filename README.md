
<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/fweber144/shinybrms.svg?branch=master)](https://travis-ci.org/fweber144/shinybrms)
<!-- badges: end -->

# Description

This [R](https://www.R-project.org/) package
[**shinybrms**](https://CRAN.R-project.org/package=shinybrms) provides a
graphical user interface (GUI) for the R package
[**brms**](https://CRAN.R-project.org/package=brms) which allows to fit
Bayesian regression models using [Stan](https://mc-stan.org/) (more
specifically, using its R interface, the R package
[**rstan**](https://CRAN.R-project.org/package=rstan)). The GUI is a
[Shiny](https://shiny.rstudio.com/) app, i.e. it was created using the R
package [**shiny**](https://CRAN.R-project.org/package=shiny).

# Installation

If not already done, you need to install R first (see the [R
homepage](https://www.R-project.org/)). Then, in R, perform the
following steps:

1.  Install the package **rstan** (see [this GitHub
    page](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)
    for instructions).
2.  Install the package **shinybrms** either from
    [GitHub](https://github.com/) or from
    [CRAN](https://CRAN.R-project.org/). Note that installing from
    GitHub might give a more recent version of **shinybrms** than
    installing from CRAN. In R, the commands are as follows:
      - To install [**shinybrms** from
        GitHub](https://github.com/fweber144/shinybrms):
    <!-- end list -->
    ``` r
    if(!requireNamespace("devtools", quietly = TRUE)){
      install.packages("devtools")
    }
    devtools::install_github("fweber144/shinybrms")
    ```
      - To install [**shinybrms** from
        CRAN](https://CRAN.R-project.org/package=shinybrms):
    <!-- end list -->
    ``` r
    install.packages("shinybrms")
    ```

# Usage

In R, launch the **shinybrms** Shiny app by typing:

``` r
library(shinybrms)
launch_shinybrms()
```
