app <- ShinyDriver$new("../../")
app$snapshotInit("offsets_nonnum_run_linux", screenshot = FALSE)

source("offsets_nonnum_script.R", local = TRUE, echo = FALSE)
