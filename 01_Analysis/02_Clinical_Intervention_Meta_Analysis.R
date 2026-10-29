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
  col = "#650915"
)
dev.off()

# Subgroup Forest Plot (Control_Type)
dat_ord <- dat[order(dat$Control_Type, dat$yi, decreasing = FALSE), ]
cats <- unique(dat_ord$Control_Type)
row_cursor <- 3
cat_poly_rows <- list()
cat_header_rows <- list()
study_rows <- numeric(nrow(dat_ord))

for (cat in rev(cats)) {
  idx <- which(dat_ord$Control_Type == cat)
  cat_poly_rows[[cat]] <- row_cursor
  row_cursor <- row_cursor + 1
  for (i in seq_along(idx)) {
    study_rows[idx[i]] <- row_cursor
    row_cursor <- row_cursor + 1
  }
  cat_header_rows[[cat]] <- row_cursor
  row_cursor <- row_cursor + 2
}
top_row <- row_cursor

slabs_sub <- paste0(
  dat_ord$Study, " (", dat_ord$Year, ") [",
  ifelse(dat_ord$Control_Type == "Active Control" & !is.na(dat_ord$Active_Comparator),
         dat_ord$Active_Comparator, dat_ord$Control_Type), "]"
)

png(
  "03_Visuals/Intervention-Subgroup_Forest.png",
  width = 3000,
  height = 2400,
  res = 300
)
forest(
  dat_ord$yi,
  dat_ord$vi,
  slab = slabs_sub,
  rows = study_rows,
  ylim = c(-1.5, top_row + 1),
  xlab = "Besaran Efek Intervensi (Hedges' g)",
  header = c("Studi Primer & Subkelompok", "Hedges' g [95% CI]"),
  cex = 0.85,
  col = "#650915",
  refline = 0,
  alim = c(min(-0.5, min(dat_ord$yi - 1.96 * sqrt(dat_ord$vi)) - 0.2), max(1.5, max(dat_ord$yi + 1.96 * sqrt(dat_ord$vi)) + 0.2))
)
addpoly(
  mod.meta,
  row = 1,
  mlab = "Total Pooled (REML)",
  cex = 0.85,
  col = "#650915",
  border = "#650915"
)
for (cat in cats) {
  idx <- which(dat_ord$Control_Type == cat)
  k_cat <- length(idx)
  sub_res <- rma(dat_ord$yi[idx], dat_ord$vi[idx], method = "REML")
  addpoly(
    sub_res,
    row = cat_poly_rows[[cat]],
    mlab = paste0("Subtotal: ", cat, " (k = ", k_cat, ")"),
    cex = 0.85,
    col = "#C5A059",
    border = "#650915"
  )
  text(
    par("usr")[1],
    cat_header_rows[[cat]],
    paste0("Subkelompok: ", cat, " (k = ", k_cat, ")"),
    font = 2,
    pos = 4,
    cex = 0.85
  )
}
usr <- par("usr")
text(
  usr[1],
  -0.8,
  sprintf("Uji Beda Subkelompok (QB): QM = %.2f, df = %d, p = %.4f  |  Heterogenitas Residual (QE): QE = %.2f, p = %.4f",
          mod.reg.ctrl$QM, mod.reg.ctrl$QMdf[1], mod.reg.ctrl$QMp, mod.reg.ctrl$QE, mod.reg.ctrl$QEp),
  pos = 4,
  cex = 0.75,
  font = 3
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
  shade = c("#FFFFFF", "#FAF8F5", "#F2EBE1"),
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
  bg = rgb(101 / 255, 9 / 255, 21 / 255, 0.70),
  col = "#650915",
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
  col = rgb(197 / 255, 160 / 255, 89 / 255, 0.25),
  border = "#C5A059"
)
lines(
  x_seq,
  preds$pred,
  col = "#650915",
  lwd = 2.5
)
grid(
  nx = NULL,
  ny = NULL,
  lty = 2,
  col = "#E5D3B3"
)
points(
  dat$Duration_Weeks,
  mod.reg.dur$yi,
  pch = 21,
  bg = rgb(101 / 255, 9 / 255, 21 / 255, 0.70),
  col = "#650915",
  cex = cex_scaled
)
abline(h = 0, lty = 3, col = "#C5A059")
dev.off()

# Interactive plotting (VS Code / httpgd)
if (interactive()) {
  forest(mod.meta, slab = paste0(dat$Study, " (", dat$Year, ")"), addpred = TRUE, showweights = TRUE)
  funnel(mod.meta, level = c(90, 95, 99), shade = c("white", "gray75", "gray90"), refline = 0)
  funnel(mod.tf, legend = TRUE)
}
