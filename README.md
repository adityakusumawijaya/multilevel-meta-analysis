# Multilevel & Intervention Meta-Analysis Platform

Repositori ini menyediakan kerangka kerja analitis dan aplikasi interaktif berbasis **R Shiny** untuk estimasi besaran efek (*effect size*) studi primer dan sintesis bukti klinis (*meta-analysis*) pada uji coba intervensi psikologis/kesehatan mental. Platform ini mengintegrasikan paket `esc` untuk konversi metrik mikro, `metafor` untuk estimasi model meta-analisis efek acak (*random-effects*), serta antarmuka modern berbasis `bslib` dan visualisasi publikasi resolusi tinggi.

---

## 1. Fitur Utama Aplikasi

### A. Kalkulator Parameter Besaran Efek Intervensi (`esc`)
Mendukung konversi langsung berbagai format pelaporan studi primer ke dalam metrik *standardized mean difference* (**Hedges' g** dengan koreksi bias sampel kecil, atau **Cohen's d**):
- **Means & SDs**: Rata-rata dan standar deviasi kelompok intervensi vs kontrol.
- **Means & SE**: Rata-rata dan standar error kelompok intervensi vs kontrol.
- **Independent t-test**: Nilai statistik $t$ beserta ukuran sampel ($n_1, n_2$).
- **Nilai p & Sampel**: Nilai signifikansi $p$ dua arah beserta arah efektivitas.
- **One-way F-test**: Nilai $F$ antar dua kelompok ($df_1 = 1$).
- **Data Biner (2x2 Table)**: Jumlah kejadian respon positif/remisi per kelompok perlakuan.
- **Korelasi**: Koefisien korelasi ($r$) dan total ukuran sampel ($N$).
- **Injeksi Langsung**: Hasil estimasi parameter ($g$, $v_i$, $SE_i$) dapat langsung disuntikkan ke dalam dataset kerja meta-analisis aktif.

### B. Manajemen Dataset Sintesis Bukti
- **Data Benchmark Bawaan**: Memuat 14 uji klinis acak (*RCT*) psikoterapi (CBT, ACT, BA, iCBT) dengan variasi format perlakuan (*Individual*, *Group*, *Online*).
- **Unggah Berkas Mandiri**: Mendukung format `.csv` dan `.xlsx` dengan pemetaan otomatis kolom efek ($y_i$), varians ($v_i$), atau standar error ($SE$).
- **Tabel Interaktif**: Penyaringan data, evaluasi bobot studi (*inverse-variance weight %*), dan ekspor dataset ke `.csv`.

### C. Pemodelan Meta-Analisis (`metafor`)
- **Pilihan Estimator Varians Antarstudi ($\tau^2$)**:
  - *Restricted Maximum Likelihood* (REML) — baku emas untuk model efek acak.
  - *DerSimonian-Laird* (DL), *Maximum Likelihood* (ML), *Paule-Mandel* (PM), *Empirical Bayes* (EB), dan *Fixed-Effect* (FE).
- **Evaluasi Heterogenitas**:
  - Cochran's $Q$-test ($Q$, $df$, $p$-value).
  - Varians antarstudi ($\tau^2$ dan $\tau$).
  - Rasio variabilitas sejati ($I^2$) dan rasio variabilitas total ($H^2$).
  - **95% Prediction Interval (PI)**: Mengestimasi rentang dispersi efek klinis sejati pada studi masa mendatang (Borenstein et al., 2017).
- **Analisis Moderator & Meta-Regresi**: Pengujian kovariat kategorikal (format intervensi) dan kontinu (durasi intervensi) melalui uji omnibus $Q_M$ dan heterogenitas residual $Q_E$.

### D. Visualisasi Publikasi
- **Forest Plot**:
  - Menampilkan nama studi, bobot studi (%), titik estimasi $g$ individual beserta 95% CI, ringkasan efek acak (*summary diamond*), dan batas interval prediksi (*prediction interval*).
  - Pengurutan studi interaktif (berdasarkan besaran efek, tahun, atau data mentah).
  - Ekspor resolusi tinggi: PNG (300 DPI) dan PDF vektor.
- **Funnel Plot & Bias Publikasi**:
  - *Contour-Enhanced Funnel Plot* dengan zona signifikansi statistik ($p < .10, .05, .01$).
  - Uji regresi linier Egger (*Egger's test*) untuk mendeteksi asimetri corong.
  - Uji korelasi peringkat Begg & Mazumdar (*rank correlation test*).
  - Analisis imputasi *Trim and Fill* (Duval & Tweedie) untuk menghitung estimasi studi yang hilang ($k_0$) dan besaran efek yang telah disesuaikan (*adjusted pooled effect*).

---

## 2. Struktur Repositori

```text
multilevel-meta-analysis/
├── .gitignore
├── README.md
├── multilevel-meta-analysis.Rproj
├── app.R                                     # Berkas utama aplikasi R Shiny (1-click Run App)
├── 01_Analysis/
│   ├── 01_Run_Shiny_App.R                    # Skrip peluncur Shiny App (Radian / RStudio)
│   └── 02_Clinical_Intervention_Meta_Analysis.R # Skrip eksekusi CLI / sintesis mandiri baku
├── 02_Working_Data/
│   └── Intervention_Benchmark_Data.csv       # Dataset benchmark 14 uji intervensi klinis
├── 03_Visuals/
│   ├── Intervention-Forest_Plot.png          # Forest plot ekspor 300 DPI
│   ├── Intervention-Funnel_Plot.png          # Contour-enhanced funnel plot ekspor 300 DPI
│   └── Intervention-TrimFill.png             # Trim & Fill funnel plot ekspor 300 DPI
└── 04_Drafts&Outputs/
```

---

## 3. Petunjuk Penggunaan di RStudio

### Menjalankan Melalui RStudio IDE:
1. Buka berkas proyek `multilevel-meta-analysis.Rproj` di RStudio.
2. Buka berkas `app.R`.
3. Klik tombol hijau **Run App** di bilah kanan atas editor skrip RStudio (atau tekan `Ctrl + Shift + Enter`).
4. Aplikasi Shiny akan langsung terbuka di jendela *RStudio Viewer* atau peramban web peramban baku (*default browser*).

### Menjalankan Melalui Konsol R / Radian:
```r
# Jalankan langsung dari root repositori:
shiny::runApp(".", launch.browser = TRUE)

# Atau melalui skrip launcher:
source("01_Analysis/01_Run_Shiny_App.R")
```

### Menjalankan Skrip Analisis Mandiri (Non-Interaktif / CLI):
```bash
Rscript 01_Analysis/02_Clinical_Intervention_Meta_Analysis.R
```

---

## 4. Dependensi Paket R

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

## 5. Referensi Ilmiah

- Borenstein, M., Higgins, J. P., Hedges, L. V., & Rothstein, H. R. (2017). Basics of meta-analysis: $I^2$ is not an absolute measure of heterogeneity. *Research Synthesis Methods*, 8(1), 5–18.
- Cuijpers, P., et al. (2020). The effects of psychotherapies for depression on response, remission, reliable change, and deterioration: A meta-analysis. *Acta Psychiatrica Scandinavica*, 141(4), 312–322.
- Duval, S., & Tweedie, R. (2000). Trim and fill: A simple funnel-plot-based method of testing and adjusting for publication bias in meta-analysis. *Biometrics*, 56(2), 455–463.
- Egger, M., Davey Smith, G., Schneider, M., & Minder, C. (1997). Bias in meta-analysis detected by a simple, graphical test. *BMJ*, 315(7109), 629–634.
- Hedges, L. V. (1981). Distribution theory for Glass's estimator of effect size and related estimators. *Journal of Educational Statistics*, 6(2), 107–128.
- Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. *Journal of Statistical Software*, 36(3), 1–48.
