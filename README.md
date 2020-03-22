
# Description

This [R](https://www.r-project.org/) package **shinybrms** provides a
graphical user interface (GUI) for the package
[**brms**](https://cran.r-project.org/web/packages/brms/index.html)
which allows to fit Bayesian regression models using
[Stan](https://mc-stan.org/) (more specifically, using its R interface,
the package
[**rstan**](https://cran.r-project.org/web/packages/rstan/index.html)).
The GUI is a [Shiny](https://shiny.rstudio.com/) app, i.e. created using
the package
[**shiny**](https://cran.r-project.org/web/packages/shiny/index.html).

# Installation

If not already done, you need to install R first (see the [R
homepage](https://www.r-project.org/)). Then, in R, perform the
following steps:

1.  Install the package
    [**rstan**](https://cran.r-project.org/web/packages/rstan/index.html)
    (see [this GitHub
    page](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)
    for instructions).
2.  Install the package **shinybrms** with the following commands:

<!-- end list -->

``` r
if(!requireNamespace("devtools", quietly = TRUE)){
  install.packages("devtools")
}
devtools::install_github("fweber144/shinybrms")
```

# Usage

In R, launch the **shinybrms** Shiny app by typing:

``` r
library(shinybrms)
launch_shinybrms()
```
