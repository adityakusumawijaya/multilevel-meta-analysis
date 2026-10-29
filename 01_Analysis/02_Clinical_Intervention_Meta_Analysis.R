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

# Meta-Regression: Delivery Format (Individual vs Group vs Guided Digital)
mod.reg.format <- rma(
  yi = yi,
  vi = vi,
  mods = ~ Format,
  data = dat,
  method = "REML"
)
summary(mod.reg.format)

# Meta-Regression: Control Condition Type (Waitlist vs TAU vs Placebo)
mod.reg.ctrl <- rma(
  yi = yi,
  vi = vi,
  mods = ~ Control_Type,
  data = dat,
  method = "REML"
)
summary(mod.reg.ctrl)

# Meta-Regression: Intervention Approach (CBT vs ACT vs BA vs iCBT)
mod.reg.interv <- rma(
  yi = yi,
  vi = vi,
  mods = ~ Intervention,
  data = dat,
  method = "REML"
)
summary(mod.reg.interv)

# Meta-Regression: Treatment Duration / Dosage (Weeks)
mod.reg.dur <- rma(
  yi = yi,
  vi = vi,
  mods = ~ Duration_Weeks,
  data = dat,
  method = "REML"
)
summary(mod.reg.dur)

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

png(
  "03_Visuals/Intervention-Bubble_Plot.png",
  width = 2400,
  height = 1800,
  res = 300
)
weights <- 1 / mod.reg.dur$vi
cex_scaled <- 1.0 + 3.0 * (weights - min(weights)) / (max(weights) - min(weights) + 1e-5)
x_seq <- seq(min(dat$Duration_Weeks), max(dat$Duration_Weeks), length.out = 100)
preds <- predict(mod.reg.dur, newmods = x_seq)

plot(
  dat$Duration_Weeks,
  mod.reg.dur$yi,
  pch = 21,
  bg = rgb(30 / 255, 58 / 255, 138 / 255, 0.65),
  col = "#1e3a8a",
  cex = cex_scaled,
  xlab = "Durasi Intervensi (Minggu)",
  ylab = "Besaran Efek Intervensi (Hedges' g)",
  main = "Meta-Regression Bubble Plot (Ukuran Titik = Bobot Presisi)",
  las = 1,
  bty = "l"
)
polygon(
  c(x_seq, rev(x_seq)),
  c(preds$ci.lb, rev(preds$ci.ub)),
  col = rgb(219 / 255, 234 / 255, 254 / 255, 0.5),
  border = NA
)
lines(
  x_seq,
  preds$pred,
  col = "#dc2626",
  lwd = 2.5
)
grid(
  nx = NULL,
  ny = NULL,
  lty = 2,
  col = "gray85"
)
points(
  dat$Duration_Weeks,
  mod.reg.dur$yi,
  pch = 21,
  bg = rgb(30 / 255, 58 / 255, 138 / 255, 0.65),
  col = "#1e3a8a",
  cex = cex_scaled
)
abline(h = 0, lty = 3, col = "gray40")
dev.off()

# Interactive plotting (VS Code / httpgd)
if (interactive()) {
  forest(mod.meta, slab = paste0(dat$Study, " (", dat$Year, ")"), addpred = TRUE, showweights = TRUE)
  funnel(mod.meta, level = c(90, 95, 99), shade = c("white", "gray75", "gray90"), refline = 0)
  funnel(mod.tf, legend = TRUE)
}
