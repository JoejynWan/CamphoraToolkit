# CamphoraToolkit_Launcher.R
# Colleagues double-click a desktop shortcut that runs this file.
# It installs any missing packages, then fetches the latest app from GitHub
# and runs it locally in the browser.

required <- c(
  "shiny", "shinyFiles", "fs", "bslib", "bsicons",
  "tidyverse", "openxlsx", "tools",
  "exifr", "zip", "batch", "vegan", "RSQLite", "parallel", "camtrapR",
  "rlang", "knitr", "rmarkdown", "magick", "pbapply"
)
missing  <- required[!required %in% rownames(installed.packages())]
if (length(missing)) install.packages(missing)

# NOTE: rmarkdown::render() to Word (Arbo Report) needs Pandoc, and exifr needs
# ExifTool, and magick needs ImageMagick. These are system dependencies, not R
# packages, so install.packages() above does NOT cover them. RStudio bundles
# its own Pandoc; running this launcher outside RStudio may need Pandoc
# installed separately (https://pandoc.org/installing.html).

shiny::runGitHub(
  repo     = "CamphoraToolkit",
  username = "JoejynWan",
  ref      = "HEAD"                # always runs the latest on the default branch
)
