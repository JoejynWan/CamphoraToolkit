# ── CamphoraToolkit Launcher ──────────────────────────────────────────────────────────────────────
# Run this file by pressing "source". 
# You may also create a desktop shortcut and double click that to run this file.
# It installs any missing packages, then fetches the latest app from GitHub and runs it locally on 
# the browser.

required <- c(
  "shiny", "shinyFiles", "fs", "bslib", "bsicons",
  "tidyverse", "openxlsx", "tools",
  "exifr", "zip", "batch", "vegan", "RSQLite", "parallel", "camtrapR",
  "rlang", "knitr", "rmarkdown", "magick", "pbapply"
)
missing  <- required[!required %in% rownames(installed.packages())]
if (length(missing)) install.packages(missing)

# NOTE: 
#  - To run Arbo Report, you need Pandoc for rmarkdown::render() to Word and ImageMagick for magick
#  - To run CT processing, you need ExifTool for exifr
#  - These are system dependencies, not R packages, so install.packages() above does NOT cover them. 
#    RStudio bundles its own Pandoc; running this launcher outside RStudio may need Pandoc 
#    installed separately (https://pandoc.org/installing.html).

shiny::runGitHub(
  repo     = "CamphoraToolkit",
  username = "JoejynWan",
  ref      = "HEAD" # always runs the latest on the default branch
)
