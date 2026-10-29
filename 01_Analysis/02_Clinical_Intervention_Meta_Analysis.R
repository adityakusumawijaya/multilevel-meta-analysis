# Analysis: Clinical Intervention Meta-Analysis (REML)
# Data: Intervention_Benchmark_Data.csv (k = 14)
# Packages: metafor, esc

# Working Directory
getwd()

# Packages
library(metafor)
library(esc)

# Load Data
dat <- read.csv("02_Working_Data/Intervention_Benchmark_Data.csv", header = TRUE)

# Estimate Random-Effects Model (REML)
mod.meta <- rma(
  yi = yi,
  vi = vi,
  data = dat,
  method = "REML"
)
summary(mod.meta)

# Test Heterogeneity (Cochran Q, Tau2, I2)
mod.meta$QE
mod.meta$QEp
mod.meta$tau2
mod.meta$I2

# 95% Prediction Interval
pred.meta <- predict(mod.meta)
pred.meta

# Meta-Regression / Subgroup Analysis (Intervention Format)
mod.reg <- rma(
  yi = yi,
  vi = vi,
  mods = ~ Format,
  data = dat,
  method = "REML"
)
summary(mod.reg)

# Publication Bias Diagnostics (Egger and Begg tests)
test.egger <- regtest(
  mod.meta,
  model = "rma",
  predictor = "sei"
)
test.egger

test.begg <- ranktest(mod.meta)
test.begg

# Duval and Tweedie Trim and Fill Analysis
mod.tf <- trimfill(mod.meta)
summary(mod.tf)

# Diagnostic plots
png(
  "03_Visuals/Intervention-Forest_Plot.png",
  width = 2800,
  height = 2000,
  res = 300
)
forest(
  mod.meta,
  slab = paste0(dat$Study, " (", dat$Year, ")"),
  xlab = "Besaran Efek Intervensi (Hedges' g)",
  mlab = "Model Efek Acak (REML)",
  header = c("Studi Primer & Tahun", "Hedges' g [95% CI]"),
  addpred = TRUE,
  showweights = TRUE,
  col = "#1e3a8a",
  border = "#1e3a8a"
)
dev.off()

png(
  "03_Visuals/Intervention-Funnel_Plot.png",
  width = 2400,
  height = 1800,
  res = 300
)
funnel(
  mod.meta,
  level = c(90, 95, 99),
  shade = c("white", "gray75", "gray90"),
  refline = 0,
  legend = TRUE,
  xlab = "Besaran Efek Intervensi (Hedges' g)",
  ylab = "Standar Error (SE)"
)
dev.off()

png(
  "03_Visuals/Intervention-TrimFill.png",
  width = 2400,
  height = 1800,
  res = 300
)
funnel(
  mod.tf,
  legend = TRUE,
  xlab = "Besaran Efek Intervensi (Hedges' g)",
  ylab = "Standar Error (SE)"
)
dev.off()

# Interactive plotting (VS Code / httpgd)
if (interactive()) {
  forest(mod.meta, slab = paste0(dat$Study, " (", dat$Year, ")"), addpred = TRUE, showweights = TRUE)
  funnel(mod.meta, level = c(90, 95, 99), shade = c("white", "gray75", "gray90"), refline = 0)
  funnel(mod.tf, legend = TRUE)
}
