# CamphoraToolkit_Launcher.R
# Colleagues double-click a desktop shortcut that runs this file.
# It installs any missing packages, then fetches the latest app from GitHub
# and runs it locally in the browser.

required <- c(
  "shiny", "shinyFiles", "fs", "bslib", "bsicons",
  "tidyverse", "openxlsx", "tools",
  "exifr", "zip", "batch", "vegan", "RSQLite", "parallel", "camtrapR"
)
missing  <- required[!required %in% rownames(installed.packages())]
if (length(missing)) install.packages(missing)

shiny::runGitHub(
  repo     = "CamphoraToolkit",
  username = "camphora-ecology",   # update to the actual GitHub org/username
  ref      = "HEAD"                # always runs the latest on the default branch
)
