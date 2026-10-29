# KAWE | Quantitative & Psychometric Research
### Multilevel Clinical Meta-Analysis & Evidence Synthesis Studio

Platform riset dan aplikasi interaktif berbasis **R Shiny** berstandar *academic research press* mewah (*Flat Maroon*, *Antique Gold*, dan *Crisp White*) untuk estimasi besaran efek (*effect size*) studi primer, rekayasa psikometrika, dan sintesis bukti klinis (*meta-analysis*) pada uji coba intervensi psikologis. Diarahkan oleh **Aditya Kusumawijaya** (Universitas Sanata Dharma · Universitas Gadjah Mada).

---

## 1. Identitas Visual & Estetika Akademis (KAWE Press Identity)
Platform ini dibangun dengan mengadopsi estetika penerbitan akademik klasik (*luxury academic press*):
- **Palet Warna Inti**:
  - *Primary Brand (Flat Maroon)*: `#650915` (digunakan pada tajuk utama, tombol aksi primer, dan status aktif).
  - *Secondary Accent (Antique Gold)*: `#C5A059` (aksen garis batas 1px, pembatas kurva psikometrika, lencana sekunder, dan efek hover).
  - *Background Primary*: `#FFFFFF` (putih bersih) dan *Background Secondary*: `#FAF8F5` (nuansa kertas buku *warm ivory*).
  - *Text Primary*: `#231618` (dark espresso/charcoal marun) dan *Text Muted*: `#5A4B4C`.
- **Tipografi Klasik**:
  - *Tajuk Utama & Logo Brand*: `Cinzel` & `Cormorant Garamond` (kapital penuh dengan spasi renggang *tracking-widest* `letter-spacing: 0.22em`).
  - *Sub-tajuk & Tagline*: `Cormorant Garamond` & `EB Garamond` (`letter-spacing: 0.16em`).
  - *Teks Tubuh*: `Inter` (sans-serif netral untuk keterbacaan tinggi).
  - *Persamaan Matematika & Kode*: MathJax ($\LaTeX$) dan `JetBrains Mono`.

---

## 2. Fitur Utama Studio

### A. Portal Riset & Eksplorasi Psikometrika (Research Hub)
- **Manifesto & Profil Laboratorium**: Memperkenalkan visi riset KAWE dalam rekayasa psikometrika terapan dan sintesis bukti berpresisi tinggi.
- **Engine Distribusi Normal Interaktif**: Eksplorasi fungsi kerapatan probabilitas normal ($f(x)$ PDF) interaktif dengan visualisasi Flat Maroon & Antique Gold serta penyetaraan skala ($z$-score, $T$-score, Wechsler IQ).
- **Empat Pilar Keunggulan Riset**: (1) Rekayasa Psikometrika & IRT; (2) Sintesis Bukti Klinis & IPD; (3) Invariansi Pengukuran & Deteksi Bias; (4) Komputasi Reproduksibel & Sains Terbuka.

### B. Kalkulator Parameter Besaran Efek Intervensi (`esc`)
Mendukung konversi langsung berbagai format pelaporan studi primer ke dalam metrik *standardized mean difference* (**Hedges' g** dengan koreksi bias sampel kecil, atau **Cohen's d**):
- **Pre-Post Eksperimen vs Kontrol (`esc_mean_gain`)**: Mendukung desain pengukuran berulang ganda (*mixed within-between design*): *pretest* dan *posttest* (mean & SD) untuk kelompok eksperimen dan kontrol, atau skor perolehan (*gain score*), disesuaikan dengan korelasi pre-post $r$.
- **Means & SDs**: Rata-rata dan standar deviasi pascates kelompok intervensi vs kontrol.
- **Means & SE**: Rata-rata dan standar error kelompok intervensi vs kontrol.
- **Independent t-test**: Nilai statistik $t$ beserta ukuran sampel ($n_1, n_2$).
- **Nilai p & Sampel**: Nilai signifikansi $p$ dua arah beserta arah efektivitas.
- **One-way F-test**: Nilai $F$ antar dua kelompok ($df_1 = 1$).
- **Data Biner (2x2 Table)**: Jumlah kejadian respon positif/remisi per kelompok perlakuan.
- **Korelasi**: Koefisien korelasi ($r$) dan total ukuran sampel ($N$).
- **Injeksi Langsung**: Hasil estimasi parameter ($g$, $v_i$, $SE_i$) dapat langsung disuntikkan ke dalam dataset kerja meta-analisis aktif.

### B. Gabungan Hasil Kalkulator (Integrasi End-to-End Pipeline)
- **Kompilasi Otomatis**: Setiap studi primer yang diestimasi pada modul kalkulator secara otomatis terakumulasi dalam matriks kompilasi kalkulator.
- **Integrasi Modular ke Meta-Analisis**: Dilengkapi tombol aksi satu klik:
  - *Kirim & Timpa ke Meta-Analisis*: Mengalirkan seluruh studi kalkulator menjadi dataset kerja utama guna menjalankan pemodelan efek acak, forest plot, dan analisis bias publikasi secara instan.
  - *Gabungkan ke Dataset Aktif*: Menggabungkan studi-studi baru hasil kalkulator dengan studi yang telah ada.
  - *Muat Contoh Batch Kalkulator*: Memuat 5 studi contoh uji klinis terkompilasi.
  - *Unduh CSV Kompilasi*: Mengekspor seluruh parameter hasil kalkulator ke berkas CSV.

### C. Manajemen Dataset Sintesis Bukti
- **Data Benchmark Bawaan**: Memuat 14 uji klinis acak (*RCT*) psikoterapi (CBT, ACT, BA, iCBT) lengkap dengan variabel moderator substantif psikologi klinis (Tipe Kontrol, Format, Populasi, Kualifikasi Terapis, Risiko Bias RoB 2.0, Durasi, Total Sesi).
- **Unggah Berkas Mandiri**: Mendukung format `.csv` dan `.xlsx` dengan pemetaan otomatis kolom efek ($y_i$), varians ($v_i$), atau standar error ($SE$).
- **Tabel Interaktif**: Penyaringan data, evaluasi bobot studi (*inverse-variance weight %*), dan ekspor dataset ke `.csv`.

### D. Pemodelan Meta-Analisis & Analisis Moderator Substantif (`metafor`)
- **Pilihan Estimator Varians Antarstudi ($\tau^2$)**:
  - *Restricted Maximum Likelihood* (REML) — baku emas untuk model efek acak.
  - *DerSimonian-Laird* (DL), *Maximum Likelihood* (ML), *Paule-Mandel* (PM), *Empirical Bayes* (EB), dan *Fixed-Effect* (FE).
- **Evaluasi Heterogenitas**:
  - Cochran's $Q$-test ($Q$, $df$, $p$-value).
  - Varians antarstudi ($\tau^2$ dan $\tau$).
  - Rasio variabilitas sejati ($I^2$) dan rasio variabilitas total ($H^2$).
  - **95% Prediction Interval (PI)**: Mengestimasi rentang dispersi efek klinis sejati pada studi masa mendatang (Borenstein et al., 2017).
- **Analisis Moderator Substantif Psikologi**:
  - **Tipe Komparator / Kelompok Kontrol** (`Control_Type`): *Waitlist Control* (WLC), *Treatment as Usual* (TAU), *Psychological Placebo*, *Active Control*.
  - **Format Penghantaran Terapi** (`Format`): *Individual*, *Group*, *Guided Digital*, *Unguided Self-Help*.
  - **Pendekatan Intervensi** (`Intervention`): CBT, ACT, BA, iCBT, Mindfulness, IPT.
  - **Populasi Sasaran Klinis** (`Population`): *Clinical (MDD)*, *Clinical (Anxiety)*, *Clinical (PTSD)*, *Subclinical*, *Community*.
  - **Kualifikasi Terapis** (`Therapist_Level`): *Licensed Clinician*, *Supervised Trainee*, *Automated Digital*.
  - **Risiko Bias Metodologis** (`Risk_of_Bias`): RoB 2.0 (*Low Risk*, *Some Concerns*, *High Risk*).
  - **Kelompok Usia Partisipan** (`Age_Group`): *Adults*, *Adolescents*, *Older Adults*.
  - **Dosis & Durasi Intervensi Kontinu** (`Duration_Weeks`, `Total_Sessions`): Analisis meta-regresi linier lengkap dengan *Bubble Plot* (ukuran titik merepresentasikan bobot presisi $1/v_i$).

### E. Modul Analisis Subkelompok Klinis Dedikatif (Subgroup Analysis)
- **Komparasi Kategori Klinis**: Membandingkan efektivitas intervensi antar-strata (Tipe Kontrol, Format, Pendekatan Terapi, Risiko Bias, Populasi, Kualifikasi Terapis, dan Kelompok Usia).
- **Uji Beda Antar-Subkelompok ($Q_B$) & Heterogenitas Residual ($Q_E$)**:
  - Uji omnibus moderator ($Q_B$, $df$, $p$-value).
  - Evaluasi variabilitas residual yang belum terjelaskan ($Q_E$, $df$, $p$-value).
  - Proporsi varians sejati antarstudi yang dijelaskan ($R^2$ %).
- **Subgroup Forest Plot Multi-Intan**:
  - Penataan bertingkat (*hierarchical layout*) dengan intan poligon terpisah untuk tiap subkelompok (`addpoly`) dan intan gabungan total (*overall pooled*).
  - Ekspor gambar resolusi tinggi PNG (300 DPI) dan PDF vektor.
- **Tabel Komparasi Subkelompok Komprehensif**: Menampilkan $k$, Total $N$, Pooled $g$, SE, 95% CI, 95% PI, $z$, $p$, $I^2$, $\tau^2$, dan $Q_w$.
- **Naratif & Interpretasi Substantif Otomatis**: Analisis implikasi klinis berdasarkan kaidah pelaporan akademik baku (tanpa diksi kausal terlarang).

### F. Visualisasi Publikasi
- **Forest Plot Standar & Berstrata**:
  - Menampilkan nama studi, bobot studi (%), titik estimasi $g$ individual beserta 95% CI, ringkasan efek acak (*summary diamond*), dan batas interval prediksi (*prediction interval*).
  - Pengurutan studi interaktif berdasarkan moderator klinis (`Control_Type`, `Format`, `Intervention`, `Risk_of_Bias`).
  - Ekspor resolusi tinggi: PNG (300 DPI) dan PDF vektor.
- **Funnel Plot & Bias Publikasi**:
  - *Contour-Enhanced Funnel Plot* dengan zona signifikansi statistik ($p < .10, .05, .01$).
  - Uji regresi linier Egger (*Egger's test*) untuk mendeteksi asimetri corong.
  - Uji korelasi peringkat Begg & Mazumdar (*rank correlation test*).
  - Analisis imputasi *Trim and Fill* (Duval & Tweedie) untuk menghitung estimasi studi yang hilang ($k_0$) dan besaran efek yang telah disesuaikan (*adjusted pooled effect*).
- **Meta-Regression Bubble Plot**: Memvisualisasikan relasi antara variabel kontinu (durasi/dosis) dan besaran efek intervensi.

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
│   ├── Intervention-Subgroup_Forest.png      # Subgroup forest plot ekspor 300 DPI
│   ├── Intervention-Funnel_Plot.png          # Contour-enhanced funnel plot ekspor 300 DPI
│   ├── Intervention-TrimFill.png             # Trim & Fill funnel plot ekspor 300 DPI
│   └── Intervention-Bubble_Plot.png          # Meta-regression bubble plot ekspor 300 DPI
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
