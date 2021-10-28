app <- ShinyDriver$new("../../")
app$snapshotInit("offsets_nonnum_run_windows", screenshot = FALSE)

source("offsets_nonnum_script.R", local = TRUE, echo = FALSE)
