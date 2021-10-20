app <- ShinyDriver$new("../../")
app$snapshotInit("bacteria_run_windows", screenshot = FALSE)

source("bacteria_script.R", local = TRUE, echo = FALSE)
