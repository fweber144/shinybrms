# Initialize an OS-dependent test -----------------------------------------

# Abbreviation (character) for the OS (one could simply use shinytest::osName()
# instead, but that gives "win" instead of "windows", so would require changing
# the names of the folders for the expected test results):
if (identical(Sys.info()["sysname"], setNames("Linux", "sysname"))) {
  os_chr <- "linux"
} else {
  os_chr <- .Platform$OS.type
}
tst_folder <- paste0(tst_prefix, "_run_", os_chr)

app$snapshotInit(tst_folder, screenshot = FALSE)

source(paste0(tst_prefix, "_script.R"), local = TRUE, echo = FALSE)
