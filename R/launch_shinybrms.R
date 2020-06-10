#' @title Launch the \pkg{shinybrms} Shiny app
#'
#' @description This function launches the \pkg{shinybrms} Shiny app.
#'
#' @param \dots Arguments passed to [shiny::runApp()]. For example, in order to force launching the
#'   Shiny app in the system's default web browser (even if using RStudio), set argument
#'   `launch.browser` to `TRUE`.
#'
#' @note The following global options are introduced by \pkg{shinybrms}:
#'
#' * `shinybrms.prog_browser`: The value for the global option `browser` to be used for opening the
#' [Stan](https://mc-stan.org/) progress file (an HTML file). Defaults to option `browser` (see
#' [base::options()]). Note that RStudio's default for option `browser` cannot be used for the Stan
#' progress file. Therefore, \pkg{shinybrms} automatically redefines RStudio's default for option
#' `browser` to fix this issue.
#' * `shinybrms.shinystan_browser`: The value for the global option
#' `browser` to be used for opening the \pkg{shinystan} app. Defaults to option `browser` (see
#' [base::options()]). Note that RStudio's default for option `browser` cannot be used for opening
#' the \pkg{shinystan} app from within \pkg{shinybrms}. Therefore, \pkg{shinybrms} automatically
#' redefines RStudio's default for option `browser` to fix this issue.
#'
#' The only other global option used by \pkg{shinybrms} is `mc.cores` (see [base::options()]): If
#' option `mc.cores` is set, then the advanced option "Cores" (in the \pkg{shinybrms} app) defaults
#' to the value of this option. If option `mc.cores` is not set, then the advanced option "Cores"
#' defaults to the value returned by [parallel::detectCores()].
#'
#' Note that \pkg{shinystan}'s global option `shinystan.rstudio` (see
#' [shinystan::launch_shinystan()]) may not be used here as \pkg{shinystan} is called from an
#' external \R process where RStudio's browsers are not available.
#'
#' @return `TRUE` (invisible).
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'   launch_shinybrms(launch.browser = TRUE)
#' }

launch_shinybrms <- function(...){
  app_path <- system.file("shinybrms_app", package = "shinybrms")
  if(identical(app_path, "")){
    stop("Path to package \"shinybrms\" could not be found. Try re-installing package \"shinybrms\".")
  }
  shiny::runApp(
    appDir = app_path,
    ...
  )
  if(FALSE){
    # Just a dummy call of a "brms" function for avoiding a NOTE in CRAN checks on some platforms:
    brms::expp1(0)
  }
  return(invisible(TRUE))
}
