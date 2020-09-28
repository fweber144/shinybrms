## Test environments

* Local:
  - R 4.0.2 on Ubuntu 20.04.1 LTS system (platform: x86_64-pc-linux-gnu (64-bit))
  - R 4.0.2 on Windows 10 x64 (build 19041) system (platform: x86_64-w64-mingw32/x64 (64-bit))
* Travis CI:
  - R 4.0.2 on Ubuntu 16.04.6 LTS system (platform: x86_64-pc-linux-gnu (64-bit))
  - R 4.0.2 on macOS High Sierra 10.13.6 system (platform: x86_64-apple-darwin17.0 (64-bit))
* win-builder:
  - R-devel (2020-09-25 r79262)
  - R-release (R 4.0.2)
  - R-oldrelease (R 3.6.3)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE for both local systems and both Travis CI systems systems:

* checking for future file timestamps ... NOTE
  unable to verify current time

This NOTE seems to be related to the external resource at <http://worldclockapi.com/> currently not being available. On the local systems, I could get rid of this NOTE by setting the environment variable `_R_CHECK_SYSTEM_CLOCK_` to "false". On the remote systems where this NOTE occurred (both Travis CI systems), I did not try this approach (since I don't consider the NOTE as being harmful anyway).

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Other comments

According to CRAN's policies, "Any change to a package's license must be
highlighted when an update is submitted [...]". The previous version of
**shinybrms** (1.4.0) *did* imply a change of license (not of
**shinybrms** itself, but version 1.4.0 introduced external components
and the licenses for these external components were added). I am sorry
that I forgot to mention this in the CRAN comments for version 1.4.0.
