#' @title Launch the \pkg{shinybrms} Shiny app
#'
#' @description This function launches the \pkg{shinybrms} Shiny app.
#'
#' @param launch.browser Argument `launch.browser` from [shiny::runApp()]. Defaults here to
#'   `interactive()`.
#' @param \dots Further arguments passed to [shiny::runApp()]. For example, in order to force
#'   launching the Shiny app in the system's default web browser, set argument `launch.browser` to
#'   `TRUE`.
#'
#' @note If option `mc.cores` is set, then the advanced option "Cores" defaults to the value of this
#'   option. If option `mc.cores` is not set, then the advanced option "Cores" defaults to the value
#'   returned by [parallel::detectCores()].
#'
#'   By default, the [Stan](https://mc-stan.org/) progress automatically opens up as an HTML file.
#'   This behavior may be disabled in the "Advanced options" of the \pkg{shinybrms} Shiny app
#'   (option "Open progress"), but for the remainder of this paragraph, it is assumed that the
#'   automatic opening of the Stan progress file is desired. In RStudio, the default for option
#'   `browser` cannot be used for the Stan progress file. Therefore, \pkg{shinybrms} has an option
#'   `shinybrms.browser_RStudio` specifying the value for option `browser` to be used when running
#'   in RStudio. If not specified, option `shinybrms.browser_RStudio` defaults to `NULL`. On
#'   Windows, `NULL` means that the default browser (as determined by the file association
#'   mechanism) is used. On non-Windows systems, `NULL` has no meaning and automatically disables
#'   the advanced option "Open progress" from the \pkg{shinybrms} app internally (with a
#'   notification popping up) if not done by the user. On any system, option
#'   `shinybrms.browser_RStudio` may directly specify the HTML browser to use (see argument
#'   `browser` in [utils::browseURL()] and option `browser` in [base::options()] for details).
#'
#'   The option `shinybrms.browser_RStudio` (see above) is also used when launching the
#'   \pkg{shinystan} Shiny app and running in RStudio. Here, the default (`NULL`) makes it
#'   impossible to launch the \pkg{shinystan} app if running on a non-Windows system (and in
#'   RStudio).
#'
#' @return `TRUE` (invisible).
#'
#' @export
#'
#' @examples
#' if(interactive){
#'   launch_shinybrms()
#' }

launch_shinybrms <- function(launch.browser = interactive(), ...){
  app_path <- system.file("shinybrms_app", package = "shinybrms")
  if(identical(app_path, "")){
    stop("Path to package \"shinybrms\" could not be found. Try re-installing package \"shinybrms\".")
  }
  shiny::runApp(
    appDir = app_path,
    launch.browser = launch.browser,
    ...
  )
  return(invisible(TRUE))
}
