# ── CamphoraToolkit Launcher ──────────────────────────────────────────────────────────────────────
# Run this file by pressing "source". 
# You may also create a desktop shortcut and double click that to run this file.
# It installs any missing packages, then fetches the latest app from GitHub and runs it locally on 
# the browser.

required <- c(
  "shiny", "shinyFiles", "fs", "bslib", "bsicons", "curl",
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

# ── Fetch and run the latest app ──────────────────────────────────────────────────────────────────
# This does by hand what shiny::runGitHub() does. runGitHub() downloads via download.file(), which
# RStudio replaces with its own downloader; on Windows that combination fails to reach GitHub.
# curl::curl_download() is not intercepted, so it works on every platform, in and out of RStudio.
url     <- "https://github.com/JoejynWan/CamphoraToolkit/archive/HEAD.tar.gz" # HEAD = latest commit
tarball <- tempfile("CamphoraToolkit", fileext = ".tar.gz")
unpacked <- tempfile("CamphoraToolkit")
dir.create(unpacked, showWarnings = FALSE)

message("Downloading ", url)
curl::curl_download(url, tarball, mode = "wb")

top <- utils::untar(tarball, list = TRUE)[1] # e.g. "CamphoraToolkit-main/"
utils::untar(tarball, exdir = unpacked)

appdir <- file.path(unpacked, top)
if (!utils::file_test("-d", appdir)) appdir <- dirname(appdir)

shiny::runApp(appdir)

unlink(c(tarball, unpacked), recursive = TRUE)
