## Test environments

* Local:
    + R version 4.5.2 (2025-10-31) on Ubuntu 25.10 system (platform:
      x86_64-pc-linux-gnu (64-bit))
* win-builder:
    + R-devel (R Under development (unstable) (2025-11-30 r89082 ucrt))
    + R-release (R version 4.5.2 (2025-10-31 ucrt))
    + Package could no be submitted to win-builder's 'R-oldrelease' flavor
      because package 'curl' threw an error:
      ```
      Error in curl::curl_fetch_memory(url, handle = h) : 
        Upload failed (at start/before it took off) [win-builder.r-project.org]:
      Failed FTP upload: 550
      ```
* macOS builder:
    + R version 4.5.1 Patched (2025-07-10 r88405) on macOS Ventura 13.3.1 system
      (platform: aarch64-apple-darwin20 (64-bit))

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. The local check also gave no INFOs.
However, the 'win-builder' gave 2 INFOs and the 'macOS builder' check gave
1 INFO. For 'win-builder', the INFOs were:

* checking CRAN incoming feasibility ... INFO
    Maintainer: 'Frank Weber <fweber144 [ at ] protonmail.com>'
    
    Suggests or Enhances not in mainstream repositories:
      cmdstanr
    Availability using Additional_repositories specification:
      cmdstanr   yes   https://stan-dev.r-universe.dev/

* checking package dependencies ... INFO
    Package suggested but not available for checking: 'cmdstanr'

For 'macOS builder', the INFO was:

* checking package dependencies ... INFO
    Package suggested but not available for checking: 'cmdstanr'

Currently, the 'cmdstanr' package (<https://mc-stan.org/cmdstanr/>) is not
available on CRAN. It provides an optional backend for the 'brms' package. The
default backend in 'brms' is the 'rstan' package which is available on CRAN. The
'cmdstanr' backend is used in local unit tests, but it is not necessary for
'shinybrms' to work. As the first NOTE says, the repository URL for the
'cmdstanr' package was added to the 'DESCRIPTION' file (field
'Additional_repositories').

## Downstream dependencies

There are currently no downstream dependencies for this package.
