# Multilevel Clinical Meta-Analysis 

Aplikasi interaktif berbasis **R Shiny** yang digunakan untuk melakukan sintesis studo-studi primer dalam kerangka meta analisis. Fitur yang dapat digunakan adalah untuk mengestimasi besaran efek (*effect size*) studi primer, rekayasa psikometrika, dan sintesis bukti klinis (*meta-analysis*) pada uji coba intervensi psikologis. 

## Packages R

Pastikan paket-paket berikut telah terpasang pada lingkungan R:

```r
install.packages(c(
  "shiny",
  "bslib",
  "metafor",
  "esc",
  "DT",
  "ggplot2",
  "readxl"
))
```

---

## Referensi Ilmiah
Borenstein, M., Higgins, J. P., Hedges, L. V., & Rothstein, H. R. (2017). Basics of meta-analysis: $I^2$ is not an absolute measure of heterogeneity. *Research Synthesis Methods*, 8(1), 5–18.
Cuijpers, P., et al. (2020). The effects of psychotherapies for depression on response, remission, reliable change, and deterioration: A meta-analysis. *Acta Psychiatrica Scandinavica*, 141(4), 312–322.
Duval, S., & Tweedie, R. (2000). Trim and fill: A simple funnel-plot-based method of testing and adjusting for publication bias in meta-analysis. *Biometrics*, 56(2), 455–463.
Egger, M., Davey Smith, G., Schneider, M., & Minder, C. (1997). Bias in meta-analysis detected by a simple, graphical test. *BMJ*, 315(7109), 629–634.
Hedges, L. V. (1981). Distribution theory for Glass's estimator of effect size and related estimators. *Journal of Educational Statistics*, 6(2), 107–128.
Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. *Journal of Statistical Software*, 36(3), 1–48.
