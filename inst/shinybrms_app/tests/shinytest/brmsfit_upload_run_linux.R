app <- ShinyDriver$new("../../")
app$snapshotInit("brmsfit_upload_run_linux", screenshot = FALSE)

source("brmsfit_upload_script.R", local = TRUE, echo = FALSE)
