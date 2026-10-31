# Analysis: Export Shiny App to Shinylive WebAssembly (webR) for GitHub Pages
# Data: app/02_Working_Data/Intervention_Benchmark_Data.csv
# Packages: shinylive, httpuv

# Working Directory
getwd()

# Packages
library(shinylive)

# Export to static WebAssembly docs
shinylive::export(
  appdir = "app",
  destdir = "docs"
)

# Test local static server (interactive)
if (interactive()) {
  httpuv::runStaticServer("docs", port = 8080)
}
