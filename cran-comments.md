## Test environments

* Local:
    + R 4.1.2 on Ubuntu 20.04.3 LTS system (platform:
      x86_64-pc-linux-gnu (64-bit))
    + R 4.1.3 on Windows 10 x64 (build 19044) system (platform:
      x86_64-w64-mingw32/x64 (64-bit))
* win-builder:
    + R-devel (R Under development (unstable) (2022-04-25 r82253 ucrt))
    + R-release (R version 4.2.0 (2022-04-22 ucrt))
    + R-oldrelease (R version 4.1.3 (2022-03-10))

## R CMD check results

There were no ERRORs or WARNINGs. The local checks also gave no NOTEs. However,
the 'win-builder' checks gave 2 NOTEs:

*   checking CRAN incoming feasibility ... NOTE
    Maintainer: 'Frank Weber <fweber144@protonmail.com>'
    
    Suggests or Enhances not in mainstream repositories:
      cmdstanr
    Availability using Additional_repositories specification:
      cmdstanr   yes   https://mc-stan.org/r-packages/

*   checking package dependencies ... NOTE
    Package suggested but not available for checking: 'cmdstanr'

Currently, the 'cmdstanr' package (<https://mc-stan.org/cmdstanr/>) is not
available on CRAN yet. It provides an optional backend for the 'brms' package.
The default backend in 'brms' is the 'rstan' package which is available on CRAN.
The 'cmdstanr' backend is used in local unit tests, but it is not necessary for
'shinybrms' to work. As the first NOTE says, I have added the repository URL for
the 'cmdstanr' package to the 'DESCRIPTION' file (field
'Additional_repositories').

## Downstream dependencies

There are currently no downstream dependencies for this package.
