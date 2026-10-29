# Analysis: Effect Size Estimation and Clinical Intervention Meta-Analysis (Shiny Application)
# Data: Interactive Entry / CSV Upload / Intervention Benchmark Data (k = 14)
# Packages: shiny, bslib, metafor, esc, DT, ggplot2

library(shiny)
library(bslib)
library(metafor)
library(esc)
library(DT)
library(ggplot2)

# Benchmark Dataset Generator
create_benchmark_data <- function() {
  data.frame(
    Study = c("Beck et al.", "Clark & Wells", "Cuijpers et al.", "Hofmann et al.", 
              "Andersson et al.", "Linehan et al.", "Ehlers et al.", "Barlow et al.", 
              "Butler et al.", "Kessler et al.", "Richards et al.", "Cuijpers & Reynolds",
              "Dimidjian et al.", "Andrews et al."),
    Year = c(2012, 2014, 2015, 2017, 2018, 2019, 2020, 2021, 2022, 2022, 2023, 2023, 2024, 2024),
    Intervention = c("CBT", "CBT", "iCBT", "CBT", "iCBT", "ACT", "CBT", "ACT", "CBT", "iCBT", "BA", "CBT", "BA", "iCBT"),
    Format = c("Individual", "Individual", "Guided Digital", "Group", "Guided Digital", "Individual", "Individual", "Group", "Individual", "Guided Digital", "Individual", "Group", "Individual", "Guided Digital"),
    Control_Type = c("Waitlist", "TAU", "Waitlist", "TAU", "Placebo", "TAU", "Waitlist", "Active Control", "TAU", "Waitlist", "TAU", "Waitlist", "Active Control", "Placebo"),
    Active_Comparator = c("None (Waitlist)", "Treatment as Usual", "None (Waitlist)", "Treatment as Usual", "Attention Placebo", "Treatment as Usual", "None (Waitlist)", "Relaxation Training", "Treatment as Usual", "None (Waitlist)", "Treatment as Usual", "None (Waitlist)", "Supportive Counseling", "Attention Placebo"),
    Population = c("Clinical (MDD)", "Clinical (MDD)", "Subclinical", "Clinical (MDD)", "Subclinical", "Clinical (MDD)", "Clinical (MDD)", "Clinical (Anxiety)", "Clinical (MDD)", "Subclinical", "Clinical (MDD)", "Clinical (MDD)", "Clinical (MDD)", "Subclinical"),
    Age_Group = c("Adults", "Adults", "Adults", "Adults", "Adults", "Adults", "Adults", "Adults", "Adults", "Adolescents", "Adults", "Older Adults", "Adults", "Adults"),
    Therapist_Level = c("Licensed Clinician", "Licensed Clinician", "Automated Digital", "Licensed Clinician", "Automated Digital", "Licensed Clinician", "Licensed Clinician", "Supervised Trainee", "Licensed Clinician", "Automated Digital", "Supervised Trainee", "Licensed Clinician", "Licensed Clinician", "Automated Digital"),
    Risk_of_Bias = c("Low Risk", "Low Risk", "Some Concerns", "Low Risk", "Some Concerns", "Low Risk", "Low Risk", "Some Concerns", "Low Risk", "Some Concerns", "Low Risk", "Some Concerns", "Low Risk", "Low Risk"),
    Duration_Weeks = c(12, 16, 8, 12, 10, 24, 12, 16, 12, 8, 10, 14, 16, 8),
    Total_Sessions = c(12, 16, 8, 12, 8, 24, 12, 14, 12, 8, 10, 14, 16, 8),
    N_Intervention = c(45, 52, 110, 60, 95, 40, 48, 55, 62, 120, 50, 70, 55, 115),
    N_Control = c(42, 50, 105, 58, 90, 38, 46, 52, 60, 115, 48, 68, 52, 110),
    yi = c(0.820, 0.640, 0.280, 0.540, 0.320, 0.880, 0.700, 0.460, 0.650, 0.240, 0.610, 0.480, 0.660, 0.290),
    vi = c(0.0499, 0.0412, 0.0188, 0.0351, 0.0219, 0.0563, 0.0452, 0.0384, 0.0345, 0.0172, 0.0427, 0.0298, 0.0394, 0.0180),
    sei = c(0.2234, 0.2030, 0.1371, 0.1873, 0.1480, 0.2373, 0.2126, 0.1960, 0.1857, 0.1311, 0.2066, 0.1726, 0.1985, 0.1342),
    stringsAsFactors = FALSE
  )
}

# Compiled Calculator Batch Generator
create_calc_sample_batch <- function() {
  data.frame(
    Study = c("Cuijpers et al. (Trial A)", "Hofmann & Smits", "Linehan et al. (Pre-Post)", "Beck & Freeman", "Andersson et al. (iCBT)"),
    Year = c(2021, 2022, 2023, 2023, 2024),
    Intervention = c("CBT", "CBT", "ACT", "CBT", "iCBT"),
    Format = c("Individual", "Group", "Individual", "Individual", "Guided Digital"),
    Control_Type = c("Waitlist", "TAU", "Active Control", "TAU", "Placebo"),
    Active_Comparator = c("None (Waitlist)", "Treatment as Usual", "Supportive Counseling", "Treatment as Usual", "Attention Placebo"),
    Population = c("Clinical (MDD)", "Clinical (Anxiety)", "Clinical (MDD)", "Clinical (MDD)", "Subclinical"),
    Age_Group = c("Adults", "Adults", "Adults", "Older Adults", "Adults"),
    Therapist_Level = c("Licensed Clinician", "Licensed Clinician", "Licensed Clinician", "Licensed Clinician", "Automated Digital"),
    Risk_of_Bias = c("Low Risk", "Low Risk", "Low Risk", "Some Concerns", "Low Risk"),
    Duration_Weeks = c(12, 14, 20, 16, 8),
    Total_Sessions = c(12, 14, 20, 16, 8),
    N_Intervention = c(40, 55, 38, 50, 100),
    N_Control = c(40, 52, 36, 48, 95),
    yi = c(0.720, 0.510, 0.840, 0.610, 0.310),
    vi = c(0.0532, 0.0385, 0.0571, 0.0425, 0.0210),
    sei = c(0.2307, 0.1962, 0.2390, 0.2062, 0.1449),
    stringsAsFactors = FALSE
  )
}

# UI Definition
ui <- page_navbar(
  title = tags$div(
    class = "d-flex align-items-center",
    tags$img(src = "kawe-logo-transparent.png", height = "38px", class = "me-2", style = "filter: drop-shadow(0 1px 3px rgba(101,9,21,0.15));"),
    tags$div(
      tags$span(class = "brand-title-nav", "KAWE"),
      tags$span(class = "brand-subtitle-nav", "QUANTITATIVE & PSYCHOMETRIC RESEARCH")
    )
  ),
  theme = bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = "#231618",
    primary = "#650915",
    secondary = "#C5A059",
    success = "#650915",
    info = "#C5A059",
    warning = "#800020",
    danger = "#4A050D"
  ),
  header = tagList(
    tags$head(
      tags$link(rel = "shortcut icon", href = "favicon.svg", type = "image/svg+xml"),
      tags$style(HTML("
        /* Google Fonts */
        @import url('https://fonts.googleapis.com/css2?family=Cinzel:wght@500;600;700&family=Cormorant+Garamond:ital,wght@0,400;0,500;0,600;0,700;1,400;1,600&family=EB+Garamond:ital,wght@0,400;0,500;0,600;1,400&family=Inter:wght@300;400;500;600&family=JetBrains+Mono:wght@400;500&display=swap');

        :root {
          /* Palet Warna Exact dari Logo KAWE */
          --kawe-maroon: #650915;       /* Warna merah marun gelap pada huruf KAWE & kurva */
          --kawe-maroon-deep: #4A050D;  /* Untuk efek hover tombol */
          --kawe-maroon-light: #7E0D1B; /* Aksen marun terang */
          --kawe-gold: #C5A059;         /* Warna Antique Gold pada bayangan Psi & kurva */
          --kawe-gold-light: #E5D3B3;   /* Untuk garis batas (border) tipis */
          --kawe-gold-trans: rgba(197, 160, 89, 0.40); /* Transparan 40% */
          --kawe-bg-white: #FFFFFF;     /* Latar belakang utama */
          --kawe-bg-paper: #FAF8F5;     /* Latar sekunder (nuansa kertas buku / ivory) */
          --kawe-text-main: #231618;    /* Warna teks paragraf (dark espresso) */
          --kawe-text-muted: #5A4B4C;   /* Warna sub-teks */

          /* Tipografi */
          --font-logo: 'Cinzel', 'Cormorant Garamond', Georgia, serif;
          --font-serif: 'Cormorant Garamond', 'EB Garamond', Georgia, serif;
          --font-sans: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
          --font-mono: 'JetBrains Mono', 'Fira Code', monospace;
        }

        body {
          background-color: var(--kawe-bg-white);
          color: var(--kawe-text-main);
          font-family: var(--font-sans);
          line-height: 1.7;
        }

        /* Headings */
        h1, h2, h3, .brand-title {
          font-family: var(--font-logo) !important;
          color: var(--kawe-maroon) !important;
          text-transform: uppercase;
          letter-spacing: 0.20em;
          font-weight: 600;
        }

        h4, h5, h6 {
          font-family: var(--font-serif) !important;
          color: var(--kawe-maroon) !important;
          letter-spacing: 0.08em;
          font-weight: 600;
        }

        /* Subtitles & Taglines */
        .brand-subtitle {
          font-family: var(--font-serif) !important;
          color: var(--kawe-gold) !important;
          text-transform: uppercase;
          letter-spacing: 0.16em;
          font-size: 0.90rem;
          font-weight: 600;
        }

        /* Dividers */
        hr, .gold-divider {
          border: 0 !important;
          border-top: 1px solid var(--kawe-gold) !important;
          opacity: 0.55;
          margin: 1.5rem 0;
        }

        /* Top Academic Press Banner */
        .kawe-top-banner {
          background-color: var(--kawe-maroon);
          color: #FAF8F5;
          border-bottom: 1px solid var(--kawe-gold);
          font-size: 0.74rem;
        }

        /* Navbar Custom Styling */
        .navbar {
          background-color: #FFFFFF !important;
          border-bottom: 2px solid var(--kawe-maroon) !important;
          box-shadow: 0 2px 10px rgba(101, 9, 21, 0.05);
          padding: 0.5rem 1.25rem !important;
        }

        .brand-title-nav {
          font-family: var(--font-logo);
          color: var(--kawe-maroon);
          font-size: 1.22rem;
          font-weight: 700;
          letter-spacing: 0.22em;
          line-height: 1.1;
          display: block;
        }

        .brand-subtitle-nav {
          font-family: var(--font-serif);
          color: var(--kawe-gold);
          font-size: 0.65rem;
          letter-spacing: 0.16em;
          font-weight: 600;
          text-transform: uppercase;
          display: block;
          margin-top: 1px;
        }

        .navbar .nav-link {
          font-family: var(--font-sans);
          font-size: 0.84rem;
          font-weight: 500;
          color: var(--kawe-text-main) !important;
          padding: 0.55rem 0.80rem !important;
          border-bottom: 2px solid transparent;
          transition: all 0.2s ease;
        }

        .navbar .nav-link:hover {
          color: var(--kawe-maroon) !important;
          border-bottom: 2px solid var(--kawe-gold);
        }

        .navbar .nav-link.active {
          color: var(--kawe-maroon) !important;
          font-weight: 600;
          border-bottom: 2px solid var(--kawe-maroon) !important;
          background-color: rgba(101, 9, 21, 0.03) !important;
        }

        /* Cards & Containers */
        .card {
          background-color: var(--kawe-bg-paper) !important;
          border: 1px solid var(--kawe-gold-trans) !important;
          border-radius: 4px !important;
          box-shadow: none !important;
        }

        .card-header {
          background-color: #FAF8F5 !important;
          border-bottom: 1px solid var(--kawe-gold-trans) !important;
          color: var(--kawe-maroon) !important;
          font-family: var(--font-serif);
          font-weight: 600;
          letter-spacing: 0.05em;
        }

        /* Buttons */
        .btn-primary {
          background-color: var(--kawe-maroon) !important;
          border: 1px solid var(--kawe-gold) !important;
          color: #FFFFFF !important;
          font-weight: 500;
          letter-spacing: 0.04em;
          border-radius: 4px;
          transition: all 0.2s ease;
        }

        .btn-primary:hover, .btn-primary:focus {
          background-color: var(--kawe-maroon-deep) !important;
          border-color: #FFFFFF !important;
          box-shadow: 0 0 0 0.2rem rgba(197, 160, 89, 0.25);
        }

        .btn-outline-primary {
          color: var(--kawe-maroon) !important;
          border-color: var(--kawe-maroon) !important;
          background-color: transparent !important;
          border-radius: 4px;
        }

        .btn-outline-primary:hover {
          background-color: var(--kawe-maroon) !important;
          color: #FFFFFF !important;
        }

        .btn-success {
          background-color: var(--kawe-maroon) !important;
          border: 1px solid var(--kawe-gold) !important;
          color: #FFFFFF !important;
          border-radius: 4px;
        }

        .btn-success:hover {
          background-color: var(--kawe-maroon-deep) !important;
        }

        /* Value Boxes */
        .value-box {
          border: 1px solid var(--kawe-gold-trans) !important;
          border-radius: 4px !important;
        }

        .value-box.bg-primary {
          background-color: var(--kawe-maroon) !important;
          color: #FAF8F5 !important;
        }

        .value-box.bg-secondary, .value-box.bg-info, .value-box.bg-warning {
          background-color: var(--kawe-bg-paper) !important;
          color: var(--kawe-maroon) !important;
          border: 1px solid var(--kawe-gold) !important;
        }

        .value-box-title {
          font-family: var(--font-serif) !important;
          text-transform: uppercase;
          letter-spacing: 0.1em;
          font-size: 0.74rem;
          color: var(--kawe-gold) !important;
        }

        .value-box-value {
          font-family: var(--font-logo) !important;
          font-weight: 600;
          color: #FAF8F5 !important;
        }

        .value-box.bg-secondary .value-box-value, 
        .value-box.bg-info .value-box-value,
        .value-box.bg-warning .value-box-value {
          color: var(--kawe-maroon) !important;
        }

        /* Badges */
        .badge.bg-primary {
          background-color: var(--kawe-maroon) !important;
          color: #FFFFFF !important;
          border: 1px solid var(--kawe-gold);
        }

        .badge.bg-secondary {
          background-color: #FAF8F5 !important;
          color: var(--kawe-maroon) !important;
          border: 1px solid var(--kawe-gold);
        }

        /* Code & Pre */
        code, pre {
          font-family: var(--font-mono);
          background-color: #F4EFEB;
          color: var(--kawe-maroon);
          border-radius: 3px;
        }

        /* Tables */
        table.dataTable thead th {
          background-color: var(--kawe-maroon) !important;
          color: #FFFFFF !important;
          font-family: var(--font-serif) !important;
          letter-spacing: 0.05em;
          border-bottom: 2px solid var(--kawe-gold) !important;
        }

        table.dataTable tbody tr:hover {
          background-color: #F7F3EE !important;
        }

        /* Research Portal Specific Styling */
        .kawe-hero-card {
          background: linear-gradient(180deg, #FFFFFF 0%, #FAF8F5 100%);
          border: 1px solid var(--kawe-gold-trans);
          border-radius: 4px;
          padding: 2.5rem 2rem;
          text-align: center;
        }

        .kawe-pillar-card {
          background-color: #FFFFFF;
          border: 1px solid var(--kawe-gold-trans);
          border-top: 3px solid var(--kawe-maroon);
          border-radius: 4px;
          padding: 1.5rem;
          height: 100%;
          transition: all 0.25s ease;
        }

        .kawe-pillar-card:hover {
          border-top-color: var(--kawe-gold);
          box-shadow: 0 4px 14px rgba(101, 9, 21, 0.08);
        }
      "))
    ),
    tags$div(
      class = "kawe-top-banner py-1 px-4 d-flex justify-content-between align-items-center",
      tags$div(
        tags$span(class = "text-uppercase fw-semibold me-2", style = "letter-spacing: 0.15em; font-size: 0.72rem; color: #C5A059;", "Evidence Synthesis & Psychometrics Laboratory"),
        tags$span(style = "font-size: 0.72rem; color: #E5D3B3;", "| Universitas Sanata Dharma · Universitas Gadjah Mada")
      ),
      tags$div(
        tags$span(style = "font-size: 0.72rem; letter-spacing: 0.1em; color: #FAF8F5;", "Aditya Kusumawijaya, S.Psi — Principal Investigator")
      )
    )
  ),
  
  # Tab 1: Research Hub & Academic Portal
  nav_panel(
    icon = icon("university"),
    title = "Research Hub & Portal",
    withMathJax(),
    
    # Hero Section
    div(
      class = "kawe-hero-card my-3",
      img(src = "kawe-logo-transparent.png", style = "max-height: 150px; margin-bottom: 1.5rem; filter: drop-shadow(0 2px 8px rgba(101, 9, 21, 0.08));"),
      h1(class = "brand-title display-5 mb-2", "KAWE"),
      div(class = "brand-subtitle mb-3", "QUANTITATIVE & PSYCHOMETRIC RESEARCH"),
      div(
        class = "text-uppercase fw-semibold",
        style = "letter-spacing: 0.18em; font-size: 0.85rem; color: #231618;",
        "Aditya Kusumawijaya, S.Psi — Principal Investigator"
      ),
      div(
        style = "font-family: var(--font-serif); font-size: 1rem; color: #5A4B4C; font-style: italic; margin-top: 0.25rem;",
        "Universitas Sanata Dharma · Universitas Gadjah Mada"
      ),
      div(class = "gold-divider w-50 mx-auto my-4"),
      p(
        class = "lead mx-auto text-dark",
        style = "max-width: 860px; font-size: 1.05rem; line-height: 1.8;",
        "Laboratorium riset independen yang mendedikasikan diri pada pengembangan metodologi psikometrika terapan, pemodelan variabel laten tingkat lanjut, evaluasi invariansi pengukuran, serta sintesis bukti klinis berpresisi tinggi (",
        tags$em("Individual Participant Data & Multilevel Meta-Analysis"),
        ") guna memajukan praktik berbasis bukti (", tags$em("Evidence-Based Practice"), ") yang terbebas dari distorsi pengukuran."
      )
    ),
    
    # Interactive Psychometric Normal Engine Section
    card(
      class = "mb-4",
      card_header(
        class = "d-flex justify-content-between align-items-center",
        span(icon("chart-area"), " Eksplorasi Teori Distribusi & Metrik Psikometrika"),
        span(class = "badge bg-primary", "Interaktif · KAWE Psychometric Engine")
      ),
      card_body(
        layout_columns(
          col_widths = c(4, 8),
          div(
            h5("Parameter Kurva Normal:", class = "fw-bold text-primary mb-3"),
            sliderInput("demo_mu", "Rata-rata Populasi (μ):", min = -2.0, max = 2.0, value = 0.0, step = 0.25),
            sliderInput("demo_sigma", "Standar Deviasi (σ):", min = 0.5, max = 2.0, value = 1.0, step = 0.1),
            checkboxInput("demo_show_zones", "Tampilkan Zona Standar Deviasi (±1σ & ±2σ)", value = TRUE),
            checkboxInput("demo_show_scales", "Tampilkan Skala Penyetaraan (z, T-score, IQ)", value = TRUE),
            hr(class = "my-3"),
            h6("Fungsi Kerapatan Probabilitas Normal (PDF):", class = "fw-bold text-dark"),
            div(
              class = "p-3 bg-white rounded border",
              style = "border-color: #E5D3B3 !important; font-size: 0.95rem;",
              "$$f(x) = \\frac{1}{\\sigma \\sqrt{2\\pi}} \\exp\\left( -\\frac{1}{2}\\left( \\frac{x - \\mu}{\\sigma} \\right)^2 \\right)$$"
            ),
            p(
              class = "text-muted small mt-2",
              "Kurva normal bell-curve di atas menjadi fondasi teoritis penyetaraan skor laten (", tags$em("latent trait theta"), ") pada pemodelan IRT dan metrik besaran efek terstandarisasi (", tags$em("Hedges' g"), ")."
            )
          ),
          div(
            plotOutput("kawe_normal_demo_plot", height = "390px")
          )
        )
      )
    ),
    
    # Four Pillars of Research Section
    div(
      class = "mb-4",
      div(
        class = "text-center mb-4",
        h3("Pilar Keunggulan Riset & Paradigma Keilmuan", class = "brand-title"),
        div(class = "brand-subtitle", "RESEARCH PILLARS & METHODOLOGICAL FOUNDATIONS")
      ),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        div(
          class = "kawe-pillar-card",
          h5("I. Rekayasa Psikometrika & IRT", class = "fw-bold text-primary mb-2"),
          p(
            class = "small text-muted mb-0",
            "Pemodelan Rasch dikotomi dan politomi (RSM/PCM), pengujian asumsi unidimensionalitas (Martin-Löf Test), evaluasi kesesuaian responden (person-fit), dan kalibrasi parameter butir berbasis Conditional Maximum Likelihood (CML)."
          )
        ),
        div(
          class = "kawe-pillar-card",
          h5("II. Sintesis Bukti Klinis & IPD", class = "fw-bold text-primary mb-2"),
          p(
            class = "small text-muted mb-0",
            "Sintesis bukti intervensi psikologis berpresisi tinggi berbasis Individual Participant Data (IPD), pemodelan efek acak multilevel (metafor), dan pelaporan Prediction Interval (PI) guna memproyeksikan rentang efektivitas riil."
          )
        ),
        div(
          class = "kawe-pillar-card",
          h5("III. Invariansi & Deteksi Bias", class = "fw-bold text-primary mb-2"),
          p(
            class = "small text-muted mb-0",
            "Identifikasi keberfungsian butir diferensial (DIF), evaluasi asimetri corong melalui Contour-Enhanced Funnel Plot, uji formal Egger/Begg, serta koreksi pemotongan studi melalui algoritma Trim and Fill."
          )
        ),
        div(
          class = "kawe-pillar-card",
          h5("IV. Komputasi Reproduksibel", class = "fw-bold text-primary mb-2"),
          p(
            class = "small text-muted mb-0",
            "Penerapan standar open science dengan kode R modular, lingkungan analisis interaktif (radian/httpgd), struktur repositori terstandarisasi SSH, serta ekspor visualisasi resolusi tinggi (300 DPI)."
          )
        )
      )
    ),
    
    # Studio Workflow & Gateway Table
    card(
      class = "mb-4",
      card_header(
        span(icon("compass"), " Panduan Alur Kerja Meta-Analysis Studio")
      ),
      card_body(
        p("Gunakan bilah navigasi di atas untuk mengakses seluruh modul analitis terintegrasi:"),
        tags$div(
          class = "table-responsive",
          tags$table(
            class = "table table-hover align-middle",
            style = "font-size: 0.9rem;",
            tags$thead(
              tags$tr(
                tags$th(style = "width: 25%;", "Modul Studio"),
                tags$th(style = "width: 45%;", "Fungsi Metodologis & Landasan Teori"),
                tags$th(style = "width: 30%;", "Luaran Analitis Utama")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td(strong(icon("calculator"), " Kalkulator Effect Size")),
                tags$td("Mengkonversi berbagai format laporan data primer (pre-post ganda esc_mean_gain, means/SD, uji-t, data biner) ke Hedges' g tidak bias."),
                tags$td(span(class = "badge bg-primary", "Estimasi Hedges' g & vi"))
              ),
              tags$tr(
                tags$td(strong(icon("layer-group"), " Gabungan Hasil Kalkulator")),
                tags$td("Mengkompilasi hasil perhitungan studi primer dari kalkulator ke dalam satu matriks untuk langsung dikirim dan dianalisis."),
                tags$td(span(class = "badge bg-secondary", "Matriks Kompilasi & Ekspor CSV"))
              ),
              tags$tr(
                tags$td(strong(icon("table"), " Dataset Sintesis Bukti")),
                tags$td("Memuat benchmark 14 uji klinis acak (RCT) atau mengunggah data mandiri (.csv/.xlsx) lengkap dengan variabel moderator psikologi."),
                tags$td(span(class = "badge bg-secondary", "Matriks Data Primer Aktif"))
              ),
              tags$tr(
                tags$td(strong(icon("chart-pie"), " Pemodelan Meta-Analisis")),
                tags$td("Estimasi model efek acak (REML, DL, ML, PM), uji heterogenitas Cochran Q, tau^2, I^2, dan 95% Prediction Interval."),
                tags$td(span(class = "badge bg-primary", "Pooled Effect & Prediksi Klinis"))
              ),
              tags$tr(
                tags$td(strong(icon("object-group"), " Analisis Subkelompok")),
                tags$td("Komparasi efektivitas antar-kategori (Tipe Kontrol, Format, Intervensi, RoB) lengkap dengan uji QB, heterogenitas QE, dan R^2."),
                tags$td(span(class = "badge bg-primary", "Subgroup Forest & Tabel Komparasi"))
              ),
              tags$tr(
                tags$td(strong(icon("tree"), " Forest Plot Publikasi")),
                tags$td("Visualisasi forest plot berstandar publikasi dengan intan efek gabungan, interval prediksi, dan pengurutan studi interaktif."),
                tags$td(span(class = "badge bg-secondary", "Gambar Ekspor PNG 300 DPI & PDF"))
              ),
              tags$tr(
                tags$td(strong(icon("shield-alt"), " Diagnostik Bias Corong")),
                tags$td("Contour-enhanced funnel plot, uji regresi linier Egger, uji Begg, dan analisis imputasi Trim and Fill (Duval & Tweedie)."),
                tags$td(span(class = "badge bg-primary", "Evaluasi Bias & Efek Disesuaikan"))
              ),
              tags$tr(
                tags$td(strong(icon("book"), " Pedoman Metodologis")),
                tags$td("Kaidah penulisan ilmiah dan sintesis bukti berstandar Cochrane Handbook, PRISMA 2020, dan APA 7th Edition."),
                tags$td(span(class = "badge bg-secondary", "Dokumentasi & Referensi Baku"))
              )
            )
          )
        )
      )
    ),
    
    # Academic Press Footer
    div(
      class = "text-center py-4 my-2 border-top",
      style = "border-color: #E5D3B3 !important;",
      div(
        class = "brand-title text-muted mb-1",
        style = "font-size: 0.95rem; letter-spacing: 0.2em;",
        "KAWE | QUANTITATIVE & PSYCHOMETRIC RESEARCH"
      ),
      div(
        class = "small text-muted",
        "Directed by Aditya Kusumawijaya, S.Psi · Evidence Synthesis & Psychometric Laboratory",
        br(),
        "Universitas Sanata Dharma · Universitas Gadjah Mada · Yogyakarta, Indonesia"
      )
    )
  ),
  
  # Tab 2: Kalkulator Effect Size
  nav_panel(
    icon = icon("calculator"),
    title = "Kalkulator Effect Size",
    layout_sidebar(
      sidebar = sidebar(
        width = 380,
        title = "Parameter Konversi Data Primer",
        
        selectInput(
          "input_type",
          "Bentuk Laporan Data Studi Primer:",
          choices = c(
            "Pre-Post Eksperimen vs Kontrol (esc_mean_gain)" = "mean_gain",
            "Rata-rata & Standar Deviasi Pascates (Means & SDs)" = "mean_sd",
            "Rata-rata & Standar Error (Means & SE)" = "mean_se",
            "Uji-t Dua Kelompok Independen" = "t_test",
            "Nilai p Dua Kelompok & Sampel" = "p_val",
            "Uji-F Satu Arah (2 Kelompok)" = "f_test",
            "Tabel Kontingensi 2x2 (Biner/Proporsi)" = "table_2x2",
            "Koefisien Korelasi (r)" = "correlation"
          ),
          selected = "mean_gain"
        ),
        
        hr(),
        
        # Panel Input: Pre-Post Control-Experiment (esc_mean_gain)
        conditionalPanel(
          condition = "input.input_type == 'mean_gain'",
          radioButtons(
            "gain_sub_type",
            "Pola Pelaporan Data Studi:",
            choices = c(
              "Nilai Pre & Post Lengkap (Mean & SD per Fase)" = "pre_post_full",
              "Skor Perubahan / Gain Score Langsung (Mean & SD Gain)" = "gain_direct"
            ),
            selected = "pre_post_full"
          ),
          selectInput(
            "gain_outcome_dir",
            "Orientasi Pengukuran Efektivitas Klinis:",
            choices = c(
              "Skor Menurun = Perbaikan (Depresi, Kecemasan, Stres)" = "decrease_better",
              "Skor Meningkat = Perbaikan (Kesejahteraan, Efikasi, Resiliensi)" = "increase_better"
            )
          ),
          
          # Sub-panel 1: Full Pre & Post
          conditionalPanel(
            condition = "input.gain_sub_type == 'pre_post_full'",
            h6("Kelompok Eksperimen / Intervensi (Treatment):", class = "text-primary fw-bold"),
            layout_columns(
              col_widths = c(4, 4, 4),
              numericInput("pre1_m", "Pre Mean", value = 28.4, step = 0.1),
              numericInput("pre1_sd", "Pre SD", value = 5.2, min = 0.01, step = 0.1),
              numericInput("pre1_n", "Ukuran n1", value = 40, min = 2, step = 1)
            ),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("post1_m", "Post Mean", value = 16.2, step = 0.1),
              numericInput("post1_sd", "Post SD", value = 4.8, min = 0.01, step = 0.1)
            ),
            h6("Kelompok Kontrol / Komparator:", class = "text-secondary fw-bold mt-2"),
            layout_columns(
              col_widths = c(4, 4, 4),
              numericInput("pre2_m", "Pre Mean", value = 27.9, step = 0.1),
              numericInput("pre2_sd", "Pre SD", value = 5.0, min = 0.01, step = 0.1),
              numericInput("pre2_n", "Ukuran n2", value = 40, min = 2, step = 1)
            ),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("post2_m", "Post Mean", value = 24.1, step = 0.1),
              numericInput("post2_sd", "Post SD", value = 5.1, min = 0.01, step = 0.1)
            )
          ),
          
          # Sub-panel 2: Direct Gain Score
          conditionalPanel(
            condition = "input.gain_sub_type == 'gain_direct'",
            h6("Kelompok Eksperimen / Intervensi (Treatment):", class = "text-primary fw-bold"),
            layout_columns(
              col_widths = c(4, 4, 4),
              numericInput("gain1_m", "Gain Mean", value = 12.2, step = 0.1),
              numericInput("gain1_sd", "Gain SD", value = 4.5, min = 0.01, step = 0.1),
              numericInput("gain1_n", "Ukuran n1", value = 40, min = 2, step = 1)
            ),
            h6("Kelompok Kontrol / Komparator:", class = "text-secondary fw-bold mt-2"),
            layout_columns(
              col_widths = c(4, 4, 4),
              numericInput("gain2_m", "Gain Mean", value = 3.8, step = 0.1),
              numericInput("gain2_sd", "Gain SD", value = 4.2, min = 0.01, step = 0.1),
              numericInput("gain2_n", "Ukuran n2", value = 40, min = 2, step = 1)
            )
          ),
          
          numericInput(
            "gain_r",
            "Estimasi Korelasi Pre-Post (r):",
            value = 0.50,
            min = -0.99,
            max = 0.99,
            step = 0.05
          ),
          div(
            class = "text-muted small mb-2",
            "Korelasi pre-post diasumsikan r = 0.50 secara default (standar Rosenthal / Cochrane Handbook jika tidak dilaporkan pada artikel primer)."
          )
        ),
        
        # Panel Input: Means & SDs
        conditionalPanel(
          condition = "input.input_type == 'mean_sd'",
          h6("Kelompok Intervensi (Treatment):", class = "text-primary fw-bold"),
          layout_columns(
            col_widths = c(4, 4, 4),
            numericInput("grp1m_sd", "Mean", value = 14.5, step = 0.1),
            numericInput("grp1sd_sd", "SD", value = 3.8, min = 0.01, step = 0.1),
            numericInput("grp1n_sd", "n", value = 50, min = 2, step = 1)
          ),
          h6("Kelompok Kontrol / Komparator:", class = "text-secondary fw-bold mt-2"),
          layout_columns(
            col_widths = c(4, 4, 4),
            numericInput("grp2m_sd", "Mean", value = 18.2, step = 0.1),
            numericInput("grp2sd_sd", "SD", value = 4.1, min = 0.01, step = 0.1),
            numericInput("grp2n_sd", "n", value = 48, min = 2, step = 1)
          )
        ),
        
        # Panel Input: Means & SE
        conditionalPanel(
          condition = "input.input_type == 'mean_se'",
          h6("Kelompok Intervensi (Treatment):", class = "text-primary fw-bold"),
          layout_columns(
            col_widths = c(4, 4, 4),
            numericInput("grp1m_se", "Mean", value = 14.5, step = 0.1),
            numericInput("grp1se_se", "SE", value = 0.54, min = 0.001, step = 0.01),
            numericInput("grp1n_se", "n", value = 50, min = 2, step = 1)
          ),
          h6("Kelompok Kontrol / Komparator:", class = "text-secondary fw-bold mt-2"),
          layout_columns(
            col_widths = c(4, 4, 4),
            numericInput("grp2m_se", "Mean", value = 18.2, step = 0.1),
            numericInput("grp2se_se", "SE", value = 0.59, min = 0.001, step = 0.01),
            numericInput("grp2n_se", "n", value = 48, min = 2, step = 1)
          )
        ),
        
        # Panel Input: Independent t-test
        conditionalPanel(
          condition = "input.input_type == 't_test'",
          numericInput("t_val", "Nilai Statistik Uji-t:", value = 2.85, step = 0.01),
          layout_columns(
            col_widths = c(6, 6),
            numericInput("t_n1", "n Intervensi", value = 45, min = 2, step = 1),
            numericInput("t_n2", "n Kontrol", value = 45, min = 2, step = 1)
          )
        ),
        
        # Panel Input: p-value
        conditionalPanel(
          condition = "input.input_type == 'p_val'",
          numericInput("p_value", "Nilai p Dua Arah (Two-Tailed):", value = 0.015, min = 0.00001, max = 0.9999, step = 0.005),
          layout_columns(
            col_widths = c(6, 6),
            numericInput("p_n1", "n Intervensi", value = 50, min = 2, step = 1),
            numericInput("p_n2", "n Kontrol", value = 50, min = 2, step = 1)
          ),
          selectInput(
            "p_direction",
            "Arah Besaran Efek:",
            choices = c(
              "Memihak Intervensi (Intervensi Lebih Baik)" = "pos",
              "Memihak Kontrol (Kontrol Lebih Baik)" = "neg"
            )
          )
        ),
        
        # Panel Input: F-test
        conditionalPanel(
          condition = "input.input_type == 'f_test'",
          numericInput("f_val", "Nilai Uji-F (df1 = 1):", value = 7.42, min = 0.001, step = 0.1),
          layout_columns(
            col_widths = c(6, 6),
            numericInput("f_n1", "n Intervensi", value = 50, min = 2, step = 1),
            numericInput("f_n2", "n Kontrol", value = 50, min = 2, step = 1)
          ),
          selectInput(
            "f_direction",
            "Arah Besaran Efek:",
            choices = c(
              "Memihak Intervensi (Intervensi Lebih Baik)" = "pos",
              "Memihak Kontrol (Kontrol Lebih Baik)" = "neg"
            )
          )
        ),
        
        # Panel Input: 2x2 Contingency Table
        conditionalPanel(
          condition = "input.input_type == 'table_2x2'",
          h6("Kelompok Intervensi:", class = "text-primary fw-bold"),
          layout_columns(
            col_widths = c(6, 6),
            numericInput("tab_e1", "Jumlah Respons Positif", value = 35, min = 0, step = 1),
            numericInput("tab_n1", "Total Sampel n1", value = 50, min = 1, step = 1)
          ),
          h6("Kelompok Kontrol:", class = "text-secondary fw-bold mt-2"),
          layout_columns(
            col_widths = c(6, 6),
            numericInput("tab_e2", "Jumlah Respons Positif", value = 18, min = 0, step = 1),
            numericInput("tab_n2", "Total Sampel n2", value = 50, min = 1, step = 1)
          )
        ),
        
        # Panel Input: Correlation
        conditionalPanel(
          condition = "input.input_type == 'correlation'",
          numericInput("corr_r", "Koefisien Korelasi (r):", value = 0.35, min = -0.999, max = 0.999, step = 0.01),
          numericInput("corr_n", "Total Ukuran Sampel (N):", value = 90, min = 4, step = 1)
        ),
        
        hr(),
        
        h6("Identitas & Moderator Studi Psikologi:", class = "fw-bold text-dark"),
        textInput("study_name", "Nama Peneliti / Studi:", value = "Studi Baru et al."),
        layout_columns(
          col_widths = c(6, 6),
          numericInput("study_year", "Tahun Publikasi:", value = 2024, min = 1950, max = 2035, step = 1),
          selectInput("study_interv", "Pendekatan Terapi:", choices = c("CBT", "ACT", "BA", "iCBT", "Mindfulness", "IPT", "Psychodynamic", "Lainnya"))
        ),
        layout_columns(
          col_widths = c(6, 6),
          selectInput("study_format", "Format Penghantaran:", choices = c("Individual", "Group", "Guided Digital", "Unguided Self-Help")),
          selectInput("study_ctrl", "Tipe Kelompok Kontrol:", choices = c("Waitlist", "TAU", "Placebo", "Active Control"))
        ),
        conditionalPanel(
          condition = "input.study_ctrl == 'Active Control'",
          textInput(
            "study_active_comparator",
            "Nama Terapi Pembanding (Active Comparator):",
            value = "Supportive Counseling",
            placeholder = "Misal: Supportive Counseling, Psikoedukasi, Terapi Relaksasi"
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          selectInput("study_pop", "Populasi Sasaran:", choices = c("Clinical (MDD)", "Clinical (Anxiety)", "Clinical (PTSD / Trauma)", "Subclinical", "Community")),
          selectInput("study_therapist", "Kualifikasi Terapis:", choices = c("Licensed Clinician", "Supervised Trainee", "Automated Digital"))
        ),
        layout_columns(
          col_widths = c(4, 4, 4),
          selectInput("study_rob", "Risiko Bias:", choices = c("Low Risk", "Some Concerns", "High Risk")),
          numericInput("study_dur", "Durasi (Minggu):", value = 12, min = 1, step = 1),
          numericInput("study_sessions", "Total Sesi:", value = 12, min = 1, step = 1)
        ),
        selectInput("study_age", "Kelompok Usia:", choices = c("Adults", "Adolescents", "Older Adults")),
        
        radioButtons(
          "es_metric",
          "Metrik Estimasi Besaran Efek:",
          choices = c(
            "Hedges' g (Koreksi Bias Sampel Kecil)" = "g",
            "Cohen's d (Estimasi Baku)" = "d"
          ),
          selected = "g"
        ),
        
        actionButton(
          "btn_calculate",
          "Hitung Effect Size",
          class = "btn-primary w-100 mb-2",
          icon = icon("calculator")
        ),
        actionButton(
          "btn_append_study",
          "Tambahkan ke Dataset Aktif",
          class = "btn-success w-100",
          icon = icon("plus-circle")
        )
      ),
      
      card(
        card_header(
          class = "bg-light d-flex justify-content-between align-items-center",
          span(class = "fw-bold text-primary", icon("chart-line"), " Hasil Estimasi Parameter Besaran Efek Intervensi"),
          span(class = "badge bg-primary", "Paket esc & Formula Hedges")
        ),
        card_body(
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(
              title = "Besaran Efek (Hedges' g / d)",
              value = textOutput("vb_calc_es"),
              showcase = icon("balance-scale"),
              theme = "primary"
            ),
            value_box(
              title = "Varians Sampling (vi)",
              value = textOutput("vb_calc_var"),
              showcase = icon("wave-square"),
              theme = "secondary"
            ),
            value_box(
              title = "Standar Error (SE)",
              value = textOutput("vb_calc_se"),
              showcase = icon("arrows-alt-h"),
              theme = "info"
            )
          ),
          
          hr(),
          
          h6("Rincian Interval Kepercayaan & Pembobotan:", class = "fw-bold text-dark"),
          tableOutput("calc_summary_table"),
          
          div(
            class = "alert alert-light border mt-3",
            h6(class = "fw-bold text-primary", icon("info-circle"), " Landasan Psikometrika & Metodologi:"),
            p(
              "Pada studi intervensi dengan ukuran sampel kecil hingga sedang, penyekoran ",
              tags$em("Cohen's d"), " memiliki tendensi bias ke atas (", tags$em("upward bias"), "). ",
              "Paket ", tags$code("esc"), " mengaplikasikan faktor koreksi eksak ", tags$em("J"), " (Hedges, 1981): ",
              tags$code("J(df) = 1 - (3 / (4*df - 1))"), " dengan derajat kebebasan ",
              tags$code("df = n1 + n2 - 2"), " guna menghasilkan ", tags$em("Hedges' g"),
              " sebagai estimator besaran efek yang tidak bias (", tags$em("unbiased estimator"), ")."
            ),
            p(
              strong("Desain Eksperimen-Kontrol Pretest-Posttest (esc_mean_gain): "),
              "Pada desain pengukuran berulang (", tags$em("mixed within-between design"), "), varians perubahan skor sangat bergantung pada korelasi antar-waktu (", tags$em("pre-post correlation r"), "). ",
              "Fungsi ", tags$code("esc_mean_gain()"), " menghitung selisih perolehan rata-rata terstandarisasi (", tags$em("standardized mean gain difference"), ") ",
              "yang disesuaikan dengan korelasi pre-post ", tags$code("r"), " (standar Morris, 2008 / Becker, 1988), ",
              "sehingga varians sampling ", tags$code("vi"), " terestimasi secara presisi tanpa distorsi kovarians residual."
            )
          )
        )
      )
    )
  ),
  
  # Tab 2: Gabungan Hasil Kalkulator
  nav_panel(
    icon = icon("layer-group"),
    title = "Gabungan Hasil Kalkulator",
    card(
      card_header(
        class = "bg-light d-flex justify-content-between align-items-center",
        span(class = "fw-bold text-primary", icon("layer-group"), " Matriks Studi Hasil Kompilasi Kalkulator Effect Size"),
        div(
          actionButton("btn_push_calc_to_active", "Kirim & Timpa ke Meta-Analisis", class = "btn-sm btn-primary me-2", icon = icon("share-square")),
          actionButton("btn_append_calc_to_active", "Gabungkan ke Dataset Aktif", class = "btn-sm btn-outline-primary me-2", icon = icon("plus")),
          actionButton("btn_load_calc_sample", "Muat Contoh Batch Kalkulator", class = "btn-sm btn-outline-secondary me-2", icon = icon("magic")),
          actionButton("btn_clear_calc_compiled", "Kosongkan Gabungan", class = "btn-sm btn-outline-danger me-2", icon = icon("trash")),
          downloadButton("btn_download_calc_csv", "Unduh CSV Kompilasi", class = "btn-sm btn-outline-success")
        )
      ),
      card_body(
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          value_box(
            title = "Studi Terkompilasi (k)",
            value = textOutput("vb_calc_comp_k"),
            showcase = icon("database"),
            theme = "primary"
          ),
          value_box(
            title = "Rata-rata Effect Size (g)",
            value = textOutput("vb_calc_comp_mean_g"),
            showcase = icon("chart-bar"),
            theme = "info"
          ),
          value_box(
            title = "Rentang Besaran Efek",
            value = textOutput("vb_calc_comp_range_g"),
            showcase = icon("arrows-alt-h"),
            theme = "secondary"
          ),
          value_box(
            title = "Total Sampel Terkumpul",
            value = textOutput("vb_calc_comp_n"),
            showcase = icon("users"),
            theme = "success"
          )
        ),
        
        div(
          class = "alert alert-light border my-3 d-flex align-items-center justify-content-between",
          div(
            icon("check-circle", class = "text-success me-2 fs-5"),
            span("Seluruh studi yang Anda hitung pada formulir kalkulator (termasuk konversi ", tags$em("esc_mean_gain"), ") secara otomatis terkumpul di tabel ini. Klik ", tags$strong("Kirim & Timpa ke Meta-Analisis"), " untuk langsung melakukan pemodelan penuh, analisis subkelompok, forest plot, dan uji bias publikasi.")
          )
        ),
        
        DTOutput("table_calc_compiled")
      )
    )
  ),
  
  # Tab 3: Dataset Meta-Analisis
  nav_panel(
    icon = icon("table"),
    title = "Dataset Sintesis Bukti",
    card(
      card_header(
        class = "bg-light d-flex justify-content-between align-items-center",
        span(class = "fw-bold text-primary", icon("table"), " Matriks Data Studi Primer & Parameter Efek"),
        div(
          actionButton("btn_load_benchmark", "Muat Data Benchmark Intervensi (k=14)", class = "btn-sm btn-outline-primary me-2", icon = icon("database")),
          actionButton("btn_clear_data", "Reset Dataset", class = "btn-sm btn-outline-danger me-2", icon = icon("trash")),
          downloadButton("btn_download_csv", "Unduh CSV", class = "btn-sm btn-outline-success")
        )
      ),
      card_body(
        layout_columns(
          col_widths = c(6, 6),
          fileInput(
            "upload_file",
            "Unggah Berkas Data Kustom (.csv / .xlsx):",
            accept = c(".csv", ".xlsx", ".xls"),
            placeholder = "Pilih berkas CSV atau Excel..."
          ),
          div(
            class = "p-3 bg-light rounded border",
            strong("Ketentuan Kolom Unggah Mandiri:"),
            tags$ul(
              class = "mb-0 ps-3 small text-muted",
              tags$li("Wajib memiliki kolom besaran efek: ", tags$code("yi"), " atau ", tags$code("es"), " atau ", tags$code("g")),
              tags$li("Wajib memiliki kolom varians: ", tags$code("vi"), " atau standar error: ", tags$code("sei")),
              tags$li("Kolom opsional: ", tags$code("Study"), ", ", tags$code("Year"), ", ", tags$code("Format"), ", ", tags$code("Duration_Weeks"))
            )
          )
        ),
        
        hr(),
        
        DTOutput("table_active_dataset")
      )
    )
  ),
  
  # Tab 3: Pemodelan Meta-Analisis
  nav_panel(
    icon = icon("chart-pie"),
    title = "Pemodelan Meta-Analisis",
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        title = "Spesifikasi Model Sintesis",
        
        selectInput(
          "meta_estimator",
          "Estimator Varians Antarstudi (tau^2):",
          choices = c(
            "Restricted Maximum Likelihood (REML) [Baku Emas]" = "REML",
            "DerSimonian-Laird (DL)" = "DL",
            "Maximum Likelihood (ML)" = "ML",
            "Paule-Mandel (PM)" = "PM",
            "Empirical Bayes (EB)" = "EB",
            "Sidik-Jonkman (SJ)" = "SJ",
            "Fixed-Effect (FE / Equal-Effects)" = "FE"
          ),
          selected = "REML"
        ),
        
        selectInput(
          "conf_level",
          "Tingkat Kepercayaan (CI):",
          choices = c("95%" = 0.95, "99%" = 0.99),
          selected = 0.95
        ),
        
        hr(),
        
        h6("Analisis Moderator / Meta-Regresi:", class = "fw-bold"),
        selectInput(
          "mod_variable",
          "Variabel Moderator (Kovariat):",
          choices = c("Tanpa Moderator (Overall Pooled)" = "none")
        ),
        
        div(
          class = "small text-muted",
          "Pemodelan efek acak (", tags$em("random-effects model"), ") mengasumsikan variasi besaran efek antarstudi merefleksikan variabilitas populasi sejati dan kesalahan sampling."
        )
      ),
      
      card(
        card_header(
          class = "bg-light fw-bold text-primary",
          icon("chart-pie"), " Rangkuman Metrik Sintesis & Heterogenitas"
        ),
        card_body(
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            value_box(
              title = "Pooled Effect (Hedges' g)",
              value = textOutput("vb_pooled_es"),
              showcase = icon("bullseye"),
              theme = "primary"
            ),
            value_box(
              title = "Heterogenitas (I^2)",
              value = textOutput("vb_hetero_i2"),
              showcase = icon("percent"),
              theme = "warning"
            ),
            value_box(
              title = "Varians Antarstudi (tau^2)",
              value = textOutput("vb_hetero_tau2"),
              showcase = icon("project-diagram"),
              theme = "info"
            ),
            value_box(
              title = "95% Prediction Interval",
              value = textOutput("vb_pred_interval"),
              showcase = icon("arrows-alt-h"),
              theme = "secondary"
            )
          ),
          
          hr(),
          
          navset_card_tab(
            nav_panel(
              title = "Hasil Estimasi Model Formal",
              verbatimTextOutput("meta_model_summary")
            ),
            nav_panel(
              title = "Interpretasi Ilmiah Akademis",
              uiOutput("meta_interpretation_text")
            ),
            nav_panel(
              title = "Analisis Moderator & Subkelompok (Meta-Regresi)",
              uiOutput("meta_moderator_ui")
            )
          )
        )
      )
    )
  ),
  
  # Tab 5: Analisis Subkelompok (Subgroup Analysis)
  nav_panel(
    icon = icon("object-group"),
    title = "Analisis Subkelompok",
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        title = "Konfigurasi Subkelompok",
        
        selectInput(
          "subgroup_var_select",
          "Pilih Variabel Subkelompok Klinis:",
          choices = c(
            "Tipe Kelompok Kontrol (Control_Type)" = "Control_Type",
            "Format Penghantaran (Format)" = "Format",
            "Pendekatan Terapi (Intervention)" = "Intervention",
            "Risiko Bias (Risk_of_Bias)" = "Risk_of_Bias",
            "Populasi Sasaran (Population)" = "Population",
            "Kualifikasi Terapis (Therapist_Level)" = "Therapist_Level",
            "Kelompok Usia (Age_Group)" = "Age_Group"
          ),
          selected = "Control_Type"
        ),
        
        sliderInput("subgroup_forest_cex", "Skala Ukuran Teks / Titik:", min = 0.7, max = 1.3, value = 0.92, step = 0.05),
        
        hr(),
        
        downloadButton("btn_dl_subgroup_png", "Unduh Subgroup Forest Plot (PNG)", class = "btn-primary w-100 mb-2"),
        downloadButton("btn_dl_subgroup_pdf", "Unduh Subgroup Forest Plot (PDF)", class = "btn-secondary w-100")
      ),
      
      card(
        card_header(
          class = "bg-light fw-bold text-primary",
          icon("object-group"), " Sintesis Bukti Komparatif Antar-Subkelompok"
        ),
        card_body(
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            value_box(
              title = "Uji Beda Subkelompok (QB)",
              value = textOutput("vb_sub_qb"),
              showcase = icon("balance-scale"),
              theme = "primary"
            ),
            value_box(
              title = "Heterogenitas Residual (QE)",
              value = textOutput("vb_sub_qe"),
              showcase = icon("wave-square"),
              theme = "info"
            ),
            value_box(
              title = "Varians Dijelaskan (R^2)",
              value = textOutput("vb_sub_r2"),
              showcase = icon("chart-pie"),
              theme = "warning"
            ),
            value_box(
              title = "Jumlah Subkelompok",
              value = textOutput("vb_sub_k_cats"),
              showcase = icon("layer-group"),
              theme = "secondary"
            )
          ),
          
          hr(),
          
          navset_card_tab(
            nav_panel(
              title = "Subgroup Forest Plot",
              plotOutput("subgroup_forest_plot_display", height = "720px")
            ),
            nav_panel(
              title = "Tabel Komparasi Subkelompok",
              tableOutput("table_subgroup_comprehensive")
            ),
            nav_panel(
              title = "Naratif & Interpretasi Substantif",
              uiOutput("subgroup_interpretation_narrative")
            )
          )
        )
      )
    )
  ),
  
  # Tab 6: Forest Plot
  nav_panel(
    icon = icon("tree"),
    title = "Forest Plot",
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        title = "Konfigurasi Visual Forest Plot",
        
        selectInput(
          "forest_sort",
          "Pengurutan Studi:",
          choices = c(
            "Sesuai Urutan Berkas Data" = "none",
            "Besaran Efek (Menaik / Ascending)" = "yi_asc",
            "Besaran Efek (Menurun / Descending)" = "yi_desc",
            "Tahun Publikasi" = "year",
            "Tipe Kelompok Kontrol (Control_Type)" = "control",
            "Format Penghantaran (Format)" = "format",
            "Pendekatan Terapi (Intervention)" = "interv",
            "Risiko Bias (Risk_of_Bias)" = "rob"
          ),
          selected = "none"
        ),
        
        checkboxInput("forest_show_pi", "Tampilkan Prediction Interval (Intan Biru)", value = TRUE),
        checkboxInput("forest_show_weights", "Tampilkan Kolom Bobot Studi (%)", value = TRUE),
        
        textInput("forest_label_left", "Label Sumbu Kiri (Kontrol Lebih Baik):", value = "Memihak Kontrol"),
        textInput("forest_label_right", "Label Sumbu Kanan (Intervensi Lebih Baik):", value = "Memihak Intervensi"),
        
        sliderInput("forest_cex", "Skala Ukuran Teks / Titik:", min = 0.7, max = 1.3, value = 0.95, step = 0.05),
        
        hr(),
        
        downloadButton("btn_dl_forest_png", "Unduh Gambar PNG (300 DPI)", class = "btn-primary w-100 mb-2"),
        downloadButton("btn_dl_forest_pdf", "Unduh Format PDF Vektor", class = "btn-secondary w-100")
      ),
      
      card(
        card_header(
          class = "bg-light fw-bold text-primary",
          icon("tree"), " Forest Plot Besaran Efek Intervensi Klinis"
        ),
        card_body(
          plotOutput("forest_plot_display", height = "680px")
        )
      )
    )
  ),
  
  # Tab 5: Bias Publikasi
  nav_panel(
    icon = icon("shield-alt"),
    title = "Diagnostik Bias & Funnel Plot",
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        title = "Pengaturan Plot Corong (Funnel Plot)",
        
        radioButtons(
          "funnel_type",
          "Jenis Funnel Plot:",
          choices = c(
            "Standard Funnel Plot (Pseudo 95% CI)" = "standard",
            "Contour-Enhanced Funnel Plot (Area Signifikansi)" = "contour"
          ),
          selected = "contour"
        ),
        
        div(
          class = "small text-muted mb-3",
          tags$em("Contour-enhanced funnel plot"), " menampilkan zona signifikansi statistik (p < 0.10, p < 0.05, p < 0.01) untuk mengidentifikasi apakah asimetri didorong oleh preferensi publikasi atau heterogenitas sejati."
        ),
        
        hr(),
        
        downloadButton("btn_dl_funnel_png", "Unduh Funnel Plot PNG", class = "btn-primary w-100 mb-2"),
        downloadButton("btn_dl_funnel_pdf", "Unduh Funnel Plot PDF", class = "btn-secondary w-100")
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(class = "bg-light fw-bold text-primary", icon("filter"), " Visualisasi Funnel Plot"),
          card_body(
            plotOutput("funnel_plot_display", height = "520px")
          )
        ),
        
        card(
          card_header(class = "bg-light fw-bold text-primary", icon("shield-alt"), " Uji Asimetri & Analisis Trim-and-Fill"),
          card_body(
            h6("1. Uji Regresi Linier Egger (Egger's Test):", class = "fw-bold text-dark"),
            tableOutput("table_egger_test"),
            
            hr(),
            
            h6("2. Uji Korelasi Peringkat Begg & Mazumdar:", class = "fw-bold text-dark"),
            tableOutput("table_begg_test"),
            
            hr(),
            
            h6("3. Analisis Imputasi Trim and Fill (Duval & Tweedie):", class = "fw-bold text-dark"),
            tableOutput("table_trimfill_summary"),
            
            div(
              class = "alert alert-light border mt-3 small",
              "Metode ", tags$em("Trim and Fill"), " mengestimasi jumlah studi yang hilang akibat bias publikasi dan mere-estimasi besaran efek gabungan yang telah disesuaikan."
            )
          )
        )
      )
    )
  ),
  
  # Tab 6: Pedoman Konseptual
  nav_panel(
    icon = icon("book"),
    title = "Pedoman Metodologis",
    card(
      card_header(class = "bg-light fw-bold text-primary", icon("book"), " Konvensi Metodologis Sintesis Bukti Intervensi"),
      card_body(
        layout_columns(
          col_widths = c(6, 6),
          div(
            h5("1. Estimasi Efek: Cohen's d vs Hedges' g", class = "text-primary fw-bold"),
            p(
              "Pada uji klinis psikologi dengan ukuran sampel per kelompok kecil (n < 50), ",
              tags$em("Cohen's d"), " mengalami inflasi parameter (", tags$em("positive bias"), "). ",
              "Hedges (1981) memperkenalkan faktor pembobot ", tags$em("J"), " yang menghasilkan ",
              tags$em("unbiased standardized mean difference"), " (", tags$em("Hedges' g"), "). ",
              "Dalam sintesis bukti klinis berbasis pedoman Cochrane dan PRISMA 2020, penggunaan ",
              tags$em("Hedges' g"), " merupakan konvensi baku emas yang wajib diutamakan."
            ),
            h5("2. Model Efek Tetap (FE) vs Efek Acak (RE)", class = "text-primary fw-bold mt-4"),
            p(
              "Model Efek Tetap (", tags$em("Fixed-Effect"), ") mengasumsikan bahwa semua studi membagikan satu besaran efek sejati tunggal (", tags$code("theta"), "). ",
              "Asumsi ini hampir selalu terlanggar pada uji coba intervensi psikologis di mana karakteristik partisipan, kualifikasi terapis, format sesi, dan durasi terapi bervariasi. ",
              "Model Efek Acak (", tags$em("Random-Effects"), ") mengasumsikan bahwa besaran efek sejati berdistribusi normal di sekitar rata-rata populasi dengan varians antarstudi ",
              tags$code("tau^2"), ", sehingga inferensi dapat digeneralisasikan secara eksternal."
            )
          ),
          div(
            h5("3. Demistifikasi Heterogenitas: I^2 vs tau^2 & Prediction Interval", class = "text-primary fw-bold"),
            p(
              "Statistik ", tags$em("I^2"), " bukan merupakan ukuran absolut dari besaran varians antarstudi, melainkan rasio variabilitas sejati terhadap variabilitas total. ",
              "Nilai ", tags$em("I^2"), " sangat dipengaruhi oleh ukuran sampel studi primer (presisi sampling). ",
              "Oleh karena itu, ", tags$code("tau^2"), " dan ", tags$em("Prediction Interval"), " (Borenstein et al., 2017) wajib dilaporkan untuk menggambarkan rentang efektivitas intervensi yang sesungguhnya pada populasi klinis masa depan."
            ),
            h5("4. Evaluasi Bias Publikasi & Efek Sampel Kecil", class = "text-primary fw-bold mt-4"),
            p(
              "Asimetri pada ", tags$em("funnel plot"), " tidak selalu identik dengan bias publikasi; asimetri dapat mencerminkan heterogenitas metodologis atau efek sampel kecil (", tags$em("small-study effects"), "). ",
              tags$em("Contour-enhanced funnel plot"), " dan uji formal Egger memfasilitasi pembuktian apakah studi-studi kecil yang hilang terkonsentrasi secara selektif pada area ketidaksignifikanan statistik (", tags$code("p > 0.05"), ")."
            )
          )
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Tab 1: Research Hub Normal Distribution Engine
  output$kawe_normal_demo_plot <- renderPlot({
    mu_val <- input$demo_mu
    sigma_val <- input$demo_sigma
    show_zones <- input$demo_show_zones
    show_scales <- input$demo_show_scales
    
    x <- seq(mu_val - 4 * sigma_val, mu_val + 4 * sigma_val, length.out = 500)
    y <- dnorm(x, mean = mu_val, sd = sigma_val)
    
    par(mar = c(4.8, 4.5, 2.8, 1.5), bg = "#FFFFFF")
    plot(
      x, y, type = "n",
      xlab = "Psychometric Theta Scale / Latent Metric (x)",
      ylab = "Probability Density f(x)",
      main = "Normal Distribution Metric — KAWE Psychometric Engine",
      bty = "l",
      las = 1,
      col.axis = "#231618",
      col.lab = "#650915",
      col.main = "#650915",
      font.main = 2
    )
    
    grid(col = "#E5D3B3", lty = 3)
    
    if (isTRUE(show_zones)) {
      # +/- 2 sigma
      idx2 <- x >= (mu_val - 2 * sigma_val) & x <= (mu_val + 2 * sigma_val)
      polygon(
        c(x[idx2], rev(x[idx2])),
        c(y[idx2], rep(0, sum(idx2))),
        col = rgb(197/255, 160/255, 89/255, 0.20),
        border = NA
      )
      # +/- 1 sigma
      idx1 <- x >= (mu_val - 1 * sigma_val) & x <= (mu_val + 1 * sigma_val)
      polygon(
        c(x[idx1], rev(x[idx1])),
        c(y[idx1], rep(0, sum(idx1))),
        col = rgb(197/255, 160/255, 89/255, 0.45),
        border = NA
      )
    }
    
    # Main curve in Flat Maroon
    lines(x, y, col = "#650915", lwd = 3.2)
    # Center line in Antique Gold
    abline(v = mu_val, col = "#C5A059", lty = 2, lwd = 1.8)
    
    # Annotations
    usr <- par("usr")
    text(
      mu_val, usr[4] * 0.92,
      sprintf("mu = %.2f, sigma = %.2f", mu_val, sigma_val),
      col = "#650915",
      font = 2,
      pos = 4
    )
    
    if (isTRUE(show_scales)) {
      text(
        usr[1] + 0.2, usr[4] * 0.85,
        "Standard Psychometric Equivalences:\n• z-score = (x - mu) / sigma\n• T-score = 50 + 10z\n• Wechsler IQ = 100 + 15z",
        adj = c(0, 1),
        cex = 0.85,
        col = "#5A4B4C",
        font = 3
      )
    }
  })

  
  # Reactive Data Storage
  active_data <- reactiveVal(create_benchmark_data())
  calc_result <- reactiveVal(NULL)
  calc_compiled_data <- reactiveVal(create_calc_sample_batch())
  
  # Observe Load Benchmark Button
  observeEvent(input$btn_load_benchmark, {
    active_data(create_benchmark_data())
    showNotification("Data benchmark intervensi klinis (k = 14) berhasil dimuat.", type = "message")
  })
  
  # Observe Clear Data Button
  observeEvent(input$btn_clear_data, {
    empty_df <- data.frame(
      Study = character(0),
      Year = numeric(0),
      Intervention = character(0),
      Format = character(0),
      Duration_Weeks = numeric(0),
      N_Intervention = numeric(0),
      N_Control = numeric(0),
      yi = numeric(0),
      vi = numeric(0),
      sei = numeric(0),
      stringsAsFactors = FALSE
    )
    active_data(empty_df)
    showNotification("Dataset telah direset.", type = "warning")
  })
  
  # Observe File Upload
  observeEvent(input$upload_file, {
    req(input$upload_file)
    file_path <- input$upload_file$datapath
    ext <- tools::file_ext(file_path)
    
    tryCatch({
      if (ext == "csv") {
        df_uploaded <- read.csv(file_path, stringsAsFactors = FALSE)
      } else if (ext %in% c("xlsx", "xls")) {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          stop("Paket 'readxl' diperlukan untuk membaca berkas Excel.")
        }
        df_uploaded <- as.data.frame(readxl::read_excel(file_path))
      } else {
        stop("Format berkas tidak didukung. Harap unggah format .csv atau .xlsx.")
      }
      
      # Standardize Column Names
      colnames_lower <- tolower(colnames(df_uploaded))
      
      # Detect yi
      yi_col <- which(colnames_lower %in% c("yi", "es", "hedges_g", "cohen_d", "effectsize", "g", "d"))[1]
      if (is.na(yi_col)) {
        stop("Kolom estimasi besaran efek (yi / es / g) tidak ditemukan.")
      }
      df_uploaded$yi <- as.numeric(df_uploaded[[yi_col]])
      
      # Detect vi or sei
      vi_col <- which(colnames_lower %in% c("vi", "var", "variance", "samp_var"))[1]
      sei_col <- which(colnames_lower %in% c("sei", "se", "standarderror", "std_err"))[1]
      
      if (!is.na(vi_col)) {
        df_uploaded$vi <- as.numeric(df_uploaded[[vi_col]])
        df_uploaded$sei <- sqrt(df_uploaded$vi)
      } else if (!is.na(sei_col)) {
        df_uploaded$sei <- as.numeric(df_uploaded[[sei_col]])
        df_uploaded$vi <- (df_uploaded$sei)^2
      } else {
        stop("Kolom varians (vi) atau standard error (sei) wajib tersedia.")
      }
      
      # Detect Study Label
      study_col <- which(colnames_lower %in% c("study", "author", "author_year", "peneliti", "label"))[1]
      if (!is.na(study_col)) {
        df_uploaded$Study <- as.character(df_uploaded[[study_col]])
      } else {
        df_uploaded$Study <- paste("Study", seq_len(nrow(df_uploaded)))
      }
      
      # Filter complete cases for analysis
      df_valid <- df_uploaded[!is.na(df_uploaded$yi) & !is.na(df_uploaded$vi) & df_uploaded$vi > 0, ]
      
      if (nrow(df_valid) == 0) {
        stop("Tidak ditemukan baris data yang valid (yi numerik dan vi > 0).")
      }
      
      active_data(df_valid)
      showNotification(paste("Berhasil memuat", nrow(df_valid), "studi dari berkas unggahan."), type = "message")
    }, error = function(e) {
      showNotification(paste("Gagal memuat berkas:", e$message), type = "error")
    })
  })
  
  # Dynamic Moderator Choices
  observe({
    df <- active_data()
    if (nrow(df) > 0) {
      potential_mods <- setdiff(colnames(df), c("Study", "yi", "vi", "sei"))
      valid_mods <- potential_mods[sapply(df[potential_mods], function(x) length(unique(x)) > 1 && length(unique(x)) < nrow(df))]
      if (length(valid_mods) > 0) {
        mod_labels <- sapply(valid_mods, function(v) {
          switch(
            v,
            "Control_Type" = "Tipe Kelompok Kontrol (Waitlist vs TAU vs Placebo)",
            "Format" = "Format Penghantaran (Individual vs Group vs Digital)",
            "Intervention" = "Pendekatan Terapi (CBT vs ACT vs BA vs iCBT)",
            "Population" = "Populasi Sasaran (Klinis vs Subklinis)",
            "Therapist_Level" = "Kualifikasi Terapis (Klinisi vs Trainee vs Digital)",
            "Risk_of_Bias" = "Risiko Bias Metodologis (RoB 2.0)",
            "Age_Group" = "Kelompok Usia (Dewasa vs Remaja vs Lansia)",
            "Duration_Weeks" = "Durasi Terapi (Minggu - Kontinu)",
            "Total_Sessions" = "Jumlah Total Sesi Terapi (Kontinu)",
            v
          )
        })
        choices <- c("Tanpa Moderator (Overall Pooled)" = "none", setNames(valid_mods, mod_labels))
      } else {
        choices <- c("Tanpa Moderator (Overall Pooled)" = "none")
      }
      updateSelectInput(session, "mod_variable", choices = choices)
    }
  })
  
  # Calculate Effect Size using esc
  observeEvent(input$btn_calculate, {
    tryCatch({
      itype <- input$input_type
      metric <- input$es_metric
      
      if (itype == "mean_gain") {
        gsub <- input$gain_sub_type
        gdir <- input$gain_outcome_dir
        r_val <- input$gain_r
        
        if (gsub == "pre_post_full") {
          p1_m <- input$pre1_m
          p1_sd <- input$pre1_sd
          po1_m <- input$post1_m
          po1_sd <- input$post1_sd
          n1 <- input$pre1_n
          
          p2_m <- input$pre2_m
          p2_sd <- input$pre2_sd
          po2_m <- input$post2_m
          po2_sd <- input$post2_sd
          n2 <- input$pre2_n
          
          if (gdir == "decrease_better") {
            # Reduksi simtom: pre - post adalah arah kemajuan positif
            g1_m <- p1_m - po1_m
            g2_m <- p2_m - po2_m
          } else {
            # Peningkatan atribut: post - pre adalah arah kemajuan positif
            g1_m <- po1_m - p1_m
            g2_m <- po2_m - p2_m
          }
          
          res <- esc_mean_gain(
            pre1mean = p1_m,
            pre1sd = p1_sd,
            post1mean = po1_m,
            post1sd = po1_sd,
            gain1mean = g1_m,
            grp1n = n1,
            pre2mean = p2_m,
            pre2sd = p2_sd,
            post2mean = po2_m,
            post2sd = po2_sd,
            gain2mean = g2_m,
            grp2n = n2,
            r = r_val,
            es.type = metric
          )
        } else {
          g1_m <- input$gain1_m
          g1_sd <- input$gain1_sd
          n1 <- input$gain1_n
          
          g2_m <- input$gain2_m
          g2_sd <- input$gain2_sd
          n2 <- input$gain2_n
          
          res <- esc_mean_gain(
            gain1mean = g1_m,
            gain1sd = g1_sd,
            grp1n = n1,
            gain2mean = g2_m,
            gain2sd = g2_sd,
            grp2n = n2,
            grp1r = r_val,
            grp2r = r_val,
            r = r_val,
            es.type = metric
          )
        }
      } else if (itype == "mean_sd") {
        res <- esc_mean_sd(
          grp1m = input$grp1m_sd,
          grp1sd = input$grp1sd_sd,
          grp1n = input$grp1n_sd,
          grp2m = input$grp2m_sd,
          grp2sd = input$grp2sd_sd,
          grp2n = input$grp2n_sd,
          es.type = metric
        )
      } else if (itype == "mean_se") {
        res <- esc_mean_se(
          grp1m = input$grp1m_se,
          grp1se = input$grp1se_se,
          grp1n = input$grp1n_se,
          grp2m = input$grp2m_se,
          grp2se = input$grp2se_se,
          grp2n = input$grp2n_se,
          es.type = metric
        )
      } else if (itype == "t_test") {
        res <- esc_t(
          t = input$t_val,
          grp1n = input$t_n1,
          grp2n = input$t_n2,
          es.type = metric
        )
      } else if (itype == "p_val") {
        df_deg <- input$p_n1 + input$p_n2 - 2
        t_equiv <- qt(1 - (input$p_value / 2), df_deg)
        if (input$p_direction == "neg") t_equiv <- -t_equiv
        res <- esc_t(
          t = t_equiv,
          grp1n = input$p_n1,
          grp2n = input$p_n2,
          es.type = metric
        )
      } else if (itype == "f_test") {
        res <- esc_f(
          f = input$f_val,
          grp1n = input$f_n1,
          grp2n = input$f_n2,
          es.type = metric
        )
        if (input$f_direction == "neg") {
          res$es <- -abs(res$es)
          temp_ci_lo <- -res$ci.hi
          res$ci.hi <- -res$ci.lo
          res$ci.lo <- temp_ci_lo
        }
      } else if (itype == "table_2x2") {
        res <- esc_2x2(
          grp1yes = input$tab_e1,
          grp1no = input$tab_n1 - input$tab_e1,
          grp2yes = input$tab_e2,
          grp2no = input$tab_n2 - input$tab_e2,
          es.type = metric
        )
      } else if (itype == "correlation") {
        res <- esc_rpb(
          r = input$corr_r,
          totaln = input$corr_n,
          es.type = metric
        )
      }
      
      calc_result(res)
      showNotification("Estimasi besaran efek berhasil dihitung.", type = "message")
    }, error = function(e) {
      showNotification(paste("Kesalahan perhitungan:", e$message), type = "error")
    })
  })
  
  # Append Calculated Study to Active Dataset
  observeEvent(input$btn_append_study, {
    res <- calc_result()
    if (is.null(res)) {
      showNotification("Silakan klik 'Hitung Effect Size' terlebih dahulu.", type = "warning")
      return()
    }
    
    current_df <- active_data()
    itype <- input$input_type
    n1_val <- 50
    n2_val <- 50
    if (itype == "mean_gain") {
      if (input$gain_sub_type == "pre_post_full") {
        n1_val <- input$pre1_n
        n2_val <- input$pre2_n
      } else {
        n1_val <- input$gain1_n
        n2_val <- input$gain2_n
      }
    } else if (itype == "mean_sd") {
      n1_val <- input$grp1n_sd
      n2_val <- input$grp2n_sd
    } else if (itype == "mean_se") {
      n1_val <- input$grp1n_se
      n2_val <- input$grp2n_se
    } else if (itype == "t_test") {
      n1_val <- input$t_n1
      n2_val <- input$t_n2
    } else if (itype == "p_val") {
      n1_val <- input$p_n1
      n2_val <- input$p_n2
    } else if (itype == "f_test") {
      n1_val <- input$f_n1
      n2_val <- input$f_n2
    } else if (itype == "table_2x2") {
      n1_val <- input$tab_n1
      n2_val <- input$tab_n2
    } else if (itype == "correlation") {
      n1_val <- round(input$corr_n / 2)
      n2_val <- round(input$corr_n / 2)
    }

    active_comp_val <- if (input$study_ctrl == "Active Control") {
      input$study_active_comparator
    } else if (input$study_ctrl == "Waitlist") {
      "None (Waitlist)"
    } else if (input$study_ctrl == "TAU") {
      "Treatment as Usual"
    } else if (input$study_ctrl == "Placebo") {
      "Attention Placebo"
    } else {
      "-"
    }

    new_row <- data.frame(
      Study = input$study_name,
      Year = input$study_year,
      Intervention = input$study_interv,
      Format = input$study_format,
      Control_Type = input$study_ctrl,
      Active_Comparator = active_comp_val,
      Population = input$study_pop,
      Age_Group = input$study_age,
      Therapist_Level = input$study_therapist,
      Risk_of_Bias = input$study_rob,
      Duration_Weeks = input$study_dur,
      Total_Sessions = input$study_sessions,
      N_Intervention = n1_val,
      N_Control = n2_val,
      yi = round(res$es, 4),
      vi = round(res$var, 5),
      sei = round(res$se, 4),
      stringsAsFactors = FALSE
    )
    
    active_data(rbind(current_df, new_row))
    comp_df <- calc_compiled_data()
    calc_compiled_data(rbind(comp_df, new_row))
    showNotification(paste("Studi '", input$study_name, "' berhasil ditambahkan ke dataset aktif dan gabungan hasil kalkulator."), type = "message")
  })
  
  # Tab 2: Gabungan Hasil Kalkulator Server Handlers
  
  # Push and Overwrite to Active Dataset
  observeEvent(input$btn_push_calc_to_active, {
    df_calc <- calc_compiled_data()
    if (nrow(df_calc) == 0) {
      showNotification("Matriks kompilasi kalkulator masih kosong.", type = "warning")
      return()
    }
    active_data(df_calc)
    showNotification(paste("Berhasil mengirim", nrow(df_calc), "studi hasil kalkulator ke dataset aktif meta-analisis."), type = "message")
  })
  
  # Append to Active Dataset
  observeEvent(input$btn_append_calc_to_active, {
    df_calc <- calc_compiled_data()
    if (nrow(df_calc) == 0) {
      showNotification("Matriks kompilasi kalkulator masih kosong.", type = "warning")
      return()
    }
    current_df <- active_data()
    active_data(rbind(current_df, df_calc))
    showNotification(paste("Berhasil menggabungkan", nrow(df_calc), "studi kompilasi ke dataset aktif."), type = "message")
  })
  
  # Load Sample Batch
  observeEvent(input$btn_load_calc_sample, {
    calc_compiled_data(create_calc_sample_batch())
    showNotification("Contoh batch studi kalkulator (k = 5) berhasil dimuat.", type = "message")
  })
  
  # Clear Compiled Data
  observeEvent(input$btn_clear_calc_compiled, {
    empty_df <- data.frame(
      Study = character(0),
      Year = numeric(0),
      Intervention = character(0),
      Format = character(0),
      Control_Type = character(0),
      Active_Comparator = character(0),
      Population = character(0),
      Age_Group = character(0),
      Therapist_Level = character(0),
      Risk_of_Bias = character(0),
      Duration_Weeks = numeric(0),
      Total_Sessions = numeric(0),
      N_Intervention = numeric(0),
      N_Control = numeric(0),
      yi = numeric(0),
      vi = numeric(0),
      sei = numeric(0),
      stringsAsFactors = FALSE
    )
    calc_compiled_data(empty_df)
    showNotification("Matriks gabungan hasil kalkulator telah dikosongkan.", type = "warning")
  })
  
  # Download Compiled Calculator CSV
  output$btn_download_calc_csv <- downloadHandler(
    filename = function() {
      paste0("Compiled_Calculator_Effects_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(calc_compiled_data(), file, row.names = FALSE)
    }
  )
  
  # Value Boxes in Compiled Calculator Tab
  output$vb_calc_comp_k <- renderText({
    df <- calc_compiled_data()
    if (nrow(df) == 0) return("0")
    paste(nrow(df), "Studi")
  })
  
  output$vb_calc_comp_mean_g <- renderText({
    df <- calc_compiled_data()
    if (nrow(df) == 0) return("-")
    sprintf("%.3f", mean(df$yi, na.rm = TRUE))
  })
  
  output$vb_calc_comp_range_g <- renderText({
    df <- calc_compiled_data()
    if (nrow(df) == 0) return("-")
    sprintf("%.3f s/d %.3f", min(df$yi, na.rm = TRUE), max(df$yi, na.rm = TRUE))
  })
  
  output$vb_calc_comp_n <- renderText({
    df <- calc_compiled_data()
    if (nrow(df) == 0) return("0")
    total_n <- sum(df$N_Intervention + df$N_Control, na.rm = TRUE)
    sprintf("%s Partisipan", format(total_n, big.mark = "."))
  })
  
  # Compiled Calculator DT Table
  output$table_calc_compiled <- renderDT({
    df <- calc_compiled_data()
    if (nrow(df) == 0) return(NULL)
    
    inv_var <- 1 / df$vi
    df$Bobot_Persen <- round((inv_var / sum(inv_var)) * 100, 2)
    
    datatable(
      df,
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE),
      rownames = FALSE,
      class = "display compact cell-border"
    ) %>%
      formatRound(columns = c("yi", "vi", "sei"), digits = 4) %>%
      formatRound(columns = "Bobot_Persen", digits = 2)
  })
  
  # Value Boxes in Calculator Tab
  output$vb_calc_es <- renderText({
    res <- calc_result()
    if (is.null(res)) return("-")
    sprintf("%.4f", res$es)
  })
  
  output$vb_calc_var <- renderText({
    res <- calc_result()
    if (is.null(res)) return("-")
    sprintf("%.5f", res$var)
  })
  
  output$vb_calc_se <- renderText({
    res <- calc_result()
    if (is.null(res)) return("-")
    sprintf("%.4f", res$se)
  })
  
  # Calculator Summary Table
  output$calc_summary_table <- renderTable({
    res <- calc_result()
    if (is.null(res)) {
      return(data.frame(Status = "Belum ada hasil perhitungan. Masukkan parameter dan klik 'Hitung Effect Size'."))
    }
    
    data.frame(
      Parameter = c("Metrik Besaran Efek", "Batas Bawah 95% CI", "Batas Atas 95% CI", "Bobot Varians Terbalik (wi = 1/vi)", "Kategori Besaran Efek (Cohen)"),
      Nilai = c(
        ifelse(input$es_metric == "g", "Hedges' g (Unbiased)", "Cohen's d"),
        sprintf("%.4f", res$ci.lo),
        sprintf("%.4f", res$ci.hi),
        sprintf("%.2f", res$w),
        ifelse(abs(res$es) < 0.2, "Dapat Diabaikan (< 0.20)",
               ifelse(abs(res$es) < 0.5, "Efek Kecil (0.20 - 0.49)",
                      ifelse(abs(res$es) < 0.8, "Efek Sedang (0.50 - 0.79)", "Efek Besar (>= 0.80)")))
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Active Dataset DT Table
  output$table_active_dataset <- renderDT({
    df <- active_data()
    if (nrow(df) == 0) return(NULL)
    
    # Calculate inverse-variance study weight
    inv_var <- 1 / df$vi
    df$Bobot_Persen <- round((inv_var / sum(inv_var)) * 100, 2)
    
    datatable(
      df,
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE),
      rownames = FALSE,
      class = "display compact cell-border"
    ) %>%
      formatRound(columns = c("yi", "vi", "sei"), digits = 4) %>%
      formatRound(columns = "Bobot_Persen", digits = 2)
  })
  
  # Download Dataset
  output$btn_download_csv <- downloadHandler(
    filename = function() {
      paste0("Intervention_Meta_Dataset_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(active_data(), file, row.names = FALSE)
    }
  )
  
  # Reactive Meta-Analysis Model (metafor)
  meta_model <- reactive({
    df <- active_data()
    req(nrow(df) >= 2)
    
    method <- input$meta_estimator
    conf_level <- as.numeric(input$conf_level)
    mod_var <- input$mod_variable
    
    if (mod_var != "none" && mod_var %in% colnames(df)) {
      form <- as.formula(paste("~", mod_var))
      res <- rma(
        yi = yi,
        vi = vi,
        mods = form,
        data = df,
        method = method,
        level = conf_level * 100
      )
    } else {
      res <- rma(
        yi = yi,
        vi = vi,
        data = df,
        method = method,
        level = conf_level * 100
      )
    }
    res
  })
  
  # Value Boxes in Meta-Analysis Tab
  output$vb_pooled_es <- renderText({
    res <- tryCatch(meta_model(), error = function(e) NULL)
    if (is.null(res)) return("-")
    sprintf("%.3f [%.3f, %.3f]", res$b[1], res$ci.lb[1], res$ci.ub[1])
  })
  
  output$vb_hetero_i2 <- renderText({
    res <- tryCatch(meta_model(), error = function(e) NULL)
    if (is.null(res)) return("-")
    if (input$meta_estimator == "FE") return("N/A (Fixed-Effect)")
    sprintf("%.1f%% (Q = %.2f, p = %.4f)", res$I2, res$QE, res$QEp)
  })
  
  output$vb_hetero_tau2 <- renderText({
    res <- tryCatch(meta_model(), error = function(e) NULL)
    if (is.null(res)) return("-")
    if (input$meta_estimator == "FE") return("0 (Fixed-Effect)")
    sprintf("%.4f (tau = %.3f)", res$tau2, sqrt(res$tau2))
  })
  
  output$vb_pred_interval <- renderText({
    res <- tryCatch(meta_model(), error = function(e) NULL)
    if (is.null(res) || input$meta_estimator == "FE") return("N/A (FE Model)")
    pred <- predict(res)
    sprintf("[%.3f, %.3f]", pred$pi.lb, pred$pi.ub)
  })
  
  # Model Summary Output
  output$meta_model_summary <- renderPrint({
    res <- tryCatch(meta_model(), error = function(e) e)
    if (inherits(res, "error")) {
      cat("Gagal mengestimasi model meta-analisis:\n", res$message)
    } else {
      summary(res)
    }
  })
  
  # Academic Interpretation Narrative
  output$meta_interpretation_text <- renderUI({
    res <- tryCatch(meta_model(), error = function(e) NULL)
    if (is.null(res)) {
      return(p("Model belum tersedia atau data belum mencukupi."))
    }
    
    df <- active_data()
    k <- res$k
    pooled_val <- res$b[1]
    ci_lb <- res$ci.lb[1]
    ci_ub <- res$ci.ub[1]
    pval <- res$pval[1]
    
    # Clinical magnitude
    mag <- ifelse(abs(pooled_val) < 0.2, "dapat diabaikan",
                  ifelse(abs(pooled_val) < 0.5, "kecil",
                         ifelse(abs(pooled_val) < 0.8, "sedang", "besar")))
    
    # Statistical significance
    sig_text <- ifelse(pval < 0.05, "signifikan secara statistik", "tidak signifikan secara statistik")
    
    # Heterogeneity interpretation
    i2_val <- ifelse(is.null(res$I2), 0, res$I2)
    hetero_level <- ifelse(i2_val < 25, "rendah (low)",
                           ifelse(i2_val < 50, "moderat (moderate)",
                                  ifelse(i2_val < 75, "substansial (substantial)", "tinggi (considerable)")))
    
    pred <- predict(res)
    pi_lb <- pred$pi.lb
    pi_ub <- pred$pi.ub
    
    tagList(
      h5("Sintesis Temuan Klinis & Implikasi Metodologis", class = "text-primary fw-bold"),
      p(
        "Berdasarkan sintesis bukti terhadap ", strong(paste(k, "studi intervensi klinis")),
        ", besaran efek gabungan (", tags$em("pooled effect size"), ") berada pada titik estimasi ",
        strong(sprintf("Hedges' g = %.3f", pooled_val)), " dengan 95% Confidence Interval ",
        strong(sprintf("[%.3f, %.3f]", ci_lb, ci_ub)), " (", tags$em("z"), sprintf(" = %.3f, p = %.4f", res$zval[1], pval), "). ",
        "Temuan ini mengindikasikan bahwa intervensi klinis terbukti ", strong(sig_text),
        " dalam menurunkan simtom dengan besaran efek berkategori ", strong(mag), " (Cohen, 1988)."
      ),
      p(
        "Evaluasi terhadap heterogenitas antarstudi memperlihatkan nilai ",
        strong(sprintf("I^2 = %.1f%%", i2_val)), " dengan varians antarstudi sejati ",
        strong(sprintf("tau^2 = %.4f (tau = %.3f)", res$tau2, sqrt(res$tau2))), " serta uji Cochran ",
        strong(sprintf("Q(df = %d) = %.2f (p = %.4f)", res$QE_df, res$QE, res$QEp)), ". ",
        "Tingkat heterogenitas ini terkategori ", strong(hetero_level), ", yang menegaskan bahwa variabilitas efek tidak semata-mata berasal dari kesalahan sampling (",
        tags$em("sampling error"), "), melainkan merefleksikan heterogenitas sejati karakteristik klinis atau metodologis antarstudi."
      ),
      if (input$meta_estimator != "FE") {
        p(
          "Lebih lanjut, estimasi ", tags$em("95% Prediction Interval"), " berada pada rentang ",
          strong(sprintf("[%.3f, %.3f]", pi_lb, pi_ub)), ". Hal tersebut mengindikasikan bahwa efektivitas intervensi klinis pada populasi studi sebanding di masa mendatang diproyeksikan berkisar dalam interval tersebut, ",
          ifelse(pi_lb > 0, "menunjukkan konsistensi kemanfaatan intervensi yang kokoh pada implementasi riil.",
                 "dengan kemungkinan ditemukannya studi baru yang menghasilkan efek nol atau tidak efektif pada setting klinis tertentu.")
        )
      }
    )
  })
  
  # Moderator / Subgroup Analysis UI
  output$meta_moderator_ui <- renderUI({
    df <- active_data()
    mod_var <- input$mod_variable
    
    if (mod_var == "none" || !mod_var %in% colnames(df)) {
      return(
        div(
          class = "alert alert-info border",
          h6(class = "fw-bold", icon("info-circle"), " Analisis Moderator Belum Diaktifkan"),
          p("Silakan pilih salah satu variabel moderator studi psikologi pada bilah konfigurasi samping."),
          p(
            class = "mb-0 text-muted small",
            "Variabel moderator penting dalam studi psikologi klinis mencakup: ",
            tags$strong("Format Penghantaran"), " (Individual vs Kelompok vs Digital), ",
            tags$strong("Tipe Kelompok Kontrol"), " (Waitlist vs TAU vs Placebo), ",
            tags$strong("Pendekatan Terapi"), " (CBT vs ACT vs BA vs iCBT), ",
            tags$strong("Populasi Sasaran"), " (Klinis vs Subklinis), ",
            tags$strong("Kualifikasi Terapis"), ", ",
            tags$strong("Risiko Bias Metodologis (RoB 2.0)"), ", serta ",
            tags$strong("Durasi Terapi"), " (Minggu / Sesi)."
          )
        )
      )
    }
    
    res <- tryCatch(meta_model(), error = function(e) NULL)
    if (is.null(res)) {
      return(div(class = "alert alert-danger", "Gagal mengestimasi model meta-regresi."))
    }
    
    is_num <- is.numeric(df[[mod_var]])
    
    qm_val <- sprintf("%.2f (df = %d, p = %.4f)", res$QM, res$QMdf[1], res$QMp)
    qe_val <- sprintf("%.2f (df = %d, p = %.4f)", res$QE, res$QEdf, res$QEp)
    r2_val <- ifelse(!is.null(res$R2), sprintf("%.2f%%", res$R2), "N/A")
    
    tagList(
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box(
          title = "Uji Omnibus Moderator (QM)",
          value = qm_val,
          showcase = icon("balance-scale"),
          theme = ifelse(res$QMp < 0.05, "success", "secondary")
        ),
        value_box(
          title = "Heterogenitas Residual (QE)",
          value = qe_val,
          showcase = icon("wave-square"),
          theme = "info"
        ),
        value_box(
          title = "Varians Dijelaskan (R^2)",
          value = r2_val,
          showcase = icon("chart-pie"),
          theme = "warning"
        )
      ),
      hr(),
      if (!is_num) {
        tagList(
          h6(class = "fw-bold text-primary", icon("table"), " 1. Rangkuman Efek Subkelompok (Subgroup Breakdown Table):"),
          tableOutput("table_subgroup_summary"),
          hr()
        )
      } else {
        tagList(
          h6(class = "fw-bold text-primary", icon("chart-line"), " 1. Meta-Regression Bubble Plot (Ukuran Titik = Bobot Presisi):"),
          plotOutput("meta_regplot_display", height = "420px"),
          hr()
        )
      },
      h6(class = "fw-bold text-dark", icon("terminal"), " 2. Hasil Estimasi Model Meta-Regresi Formal (metafor):"),
      verbatimTextOutput("meta_moderator_print")
    )
  })
  
  # Subgroup Summary Table
  output$table_subgroup_summary <- renderTable({
    df <- active_data()
    mod_var <- input$mod_variable
    req(mod_var != "none", mod_var %in% colnames(df))
    
    categories <- sort(unique(df[[mod_var]]))
    method <- input$meta_estimator
    
    res_list <- lapply(categories, function(cat_name) {
      sub_df <- df[df[[mod_var]] == cat_name, ]
      k_sub <- nrow(sub_df)
      if (k_sub >= 2) {
        sub_mod <- tryCatch(
          rma(yi = yi, vi = vi, data = sub_df, method = method),
          error = function(e) NULL
        )
        if (!is.null(sub_mod)) {
          data.frame(
            Subkelompok = as.character(cat_name),
            k = as.character(k_sub),
            `Pooled Hedges' g` = sprintf("%.3f", sub_mod$b[1]),
            SE = sprintf("%.3f", sub_mod$se[1]),
            `95% CI` = sprintf("[%.3f, %.3f]", sub_mod$ci.lb[1], sub_mod$ci.ub[1]),
            `z-score` = sprintf("%.3f", sub_mod$zval[1]),
            `p-value` = sprintf("%.4f", sub_mod$pval[1]),
            `I^2 (%)` = sprintf("%.1f%%", sub_mod$I2),
            `tau^2` = sprintf("%.4f", sub_mod$tau2),
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
        } else {
          NULL
        }
      } else {
        data.frame(
          Subkelompok = as.character(cat_name),
          k = as.character(k_sub),
          `Pooled Hedges' g` = sprintf("%.3f", sub_df$yi[1]),
          SE = sprintf("%.3f", sqrt(sub_df$vi[1])),
          `95% CI` = sprintf("[%.3f, %.3f]", sub_df$yi[1] - 1.96 * sqrt(sub_df$vi[1]), sub_df$yi[1] + 1.96 * sqrt(sub_df$vi[1])),
          `z-score` = "-",
          `p-value` = "-",
          `I^2 (%)` = "N/A (k=1)",
          `tau^2` = "N/A",
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      }
    })
    do.call(rbind, res_list)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Meta-Regression Bubble Plot
  output$meta_regplot_display <- renderPlot({
    df <- active_data()
    mod_var <- input$mod_variable
    req(mod_var != "none", mod_var %in% colnames(df))
    req(is.numeric(df[[mod_var]]))
    
    res <- tryCatch(meta_model(), error = function(e) NULL)
    req(!is.null(res))
    
    x_var <- df[[mod_var]]
    weights <- 1 / res$vi
    cex_scaled <- 1.0 + 3.0 * (weights - min(weights)) / (max(weights) - min(weights) + 1e-5)
    
    x_seq <- seq(min(x_var, na.rm = TRUE), max(x_var, na.rm = TRUE), length.out = 100)
    preds <- predict(res, newmods = x_seq)
    
    par(mar = c(5, 5, 3, 2))
    plot(
      x_var, res$yi,
      pch = 21,
      bg = rgb(101 / 255, 9 / 255, 21 / 255, 0.65),
      col = "#650915",
      cex = cex_scaled,
      xlab = paste("Variabel Moderator:", mod_var),
      ylab = "Besaran Efek Intervensi (Hedges' g)",
      main = paste("Meta-Regression Bubble Plot (Ukuran Titik = Presisi Studi 1/vi)"),
      las = 1,
      bty = "l",
      ylim = c(min(c(res$yi, preds$ci.lb)) - 0.1, max(c(res$yi, preds$ci.ub)) + 0.1)
    )
    polygon(
      c(x_seq, rev(x_seq)),
      c(preds$ci.lb, rev(preds$ci.ub)),
      col = rgb(197 / 255, 160 / 255, 89 / 255, 0.25),
      border = NA
    )
    lines(x_seq, preds$pred, col = "#650915", lwd = 2.5)
    grid(nx = NULL, ny = NULL, lty = 2, col = "#E5D3B3")
    points(
      x_var, res$yi,
      pch = 21,
      bg = rgb(101 / 255, 9 / 255, 21 / 255, 0.65),
      col = "#650915",
      cex = cex_scaled
    )
    abline(h = 0, lty = 3, col = "gray40")
  })
  
  # Printout Meta-Regression
  output$meta_moderator_print <- renderPrint({
    res <- tryCatch(meta_model(), error = function(e) e)
    if (inherits(res, "error")) {
      cat("Gagal menjalankan meta-regresi:\n", res$message)
    } else {
      print(res)
    }
  })
  
  # Tab 5: Analisis Subkelompok (Subgroup Analysis) Handlers
  
  # Subgroup Model Reactive
  subgroup_models <- reactive({
    df <- active_data()
    req(nrow(df) >= 2)
    grp_var <- input$subgroup_var_select
    req(grp_var %in% colnames(df))
    
    # Filter valid rows
    df_clean <- df[!is.na(df[[grp_var]]) & df[[grp_var]] != "", ]
    req(nrow(df_clean) >= 2)
    
    method <- input$meta_estimator
    
    # Overall pooled model
    res_all <- tryCatch(rma(yi = yi, vi = vi, data = df_clean, method = method), error = function(e) NULL)
    
    # Omnibus moderator test (QB and QE)
    form <- as.formula(paste("~ factor(", grp_var, ")"))
    res_mod <- tryCatch(rma(yi = yi, vi = vi, mods = form, data = df_clean, method = method), error = function(e) NULL)
    
    list(
      data = df_clean,
      grp_var = grp_var,
      res_all = res_all,
      res_mod = res_mod,
      method = method
    )
  })
  
  # Value Boxes in Subgroup Analysis Tab
  output$vb_sub_qb <- renderText({
    m <- tryCatch(subgroup_models(), error = function(e) NULL)
    if (is.null(m) || is.null(m$res_mod)) return("-")
    sprintf("%.2f (df = %d, p = %.4f)", m$res_mod$QM, m$res_mod$QMdf[1], m$res_mod$QMp)
  })
  
  output$vb_sub_qe <- renderText({
    m <- tryCatch(subgroup_models(), error = function(e) NULL)
    if (is.null(m) || is.null(m$res_mod)) return("-")
    sprintf("%.2f (df = %d, p = %.4f)", m$res_mod$QE, m$res_mod$QEdf, m$res_mod$QEp)
  })
  
  output$vb_sub_r2 <- renderText({
    m <- tryCatch(subgroup_models(), error = function(e) NULL)
    if (is.null(m) || is.null(m$res_mod)) return("-")
    if (input$meta_estimator == "FE") return("N/A (FE)")
    if (!is.null(m$res_mod$R2) && !is.na(m$res_mod$R2)) sprintf("%.1f%%", m$res_mod$R2) else "0.0%"
  })
  
  output$vb_sub_k_cats <- renderText({
    m <- tryCatch(subgroup_models(), error = function(e) NULL)
    if (is.null(m)) return("-")
    cats <- unique(m$data[[m$grp_var]])
    sprintf("%d Kategori (k = %d)", length(cats), nrow(m$data))
  })
  
  # Subgroup Forest Plot Rendering Helper
  render_subgroup_forest_internal <- function() {
    m <- subgroup_models()
    req(!is.null(m), !is.null(m$res_all))
    
    df <- m$data
    grp_var <- m$grp_var
    method <- m$method
    
    # Sort by group then effect size
    df <- df[order(df[[grp_var]], df$yi, decreasing = FALSE), ]
    cats <- unique(df[[grp_var]])
    
    row_cursor <- 3
    cat_poly_rows <- list()
    cat_header_rows <- list()
    study_rows <- numeric(nrow(df))
    
    for (cat in rev(cats)) {
      idx <- which(df[[grp_var]] == cat)
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
    
    # Slabs
    if (grp_var == "Control_Type") {
      slabs <- paste0(df$Study, " (", df$Year, ") [", ifelse(df$Control_Type == "Active Control" & !is.na(df$Active_Comparator), df$Active_Comparator, df$Control_Type), "]")
    } else {
      slabs <- paste0(df$Study, " (", df$Year, ")")
    }
    
    cex_val <- input$subgroup_forest_cex
    
    forest(
      df$yi,
      df$vi,
      slab = slabs,
      rows = study_rows,
      ylim = c(-1.5, top_row + 1),
      xlab = "Besaran Efek Intervensi (Hedges' g)",
      header = c("Studi Primer & Subkelompok", "Hedges' g [95% CI]"),
      cex = cex_val,
      col = "#650915",
      refline = 0,
      alim = c(min(-0.5, min(df$yi - 1.96 * sqrt(df$vi)) - 0.2), max(1.5, max(df$yi + 1.96 * sqrt(df$vi)) + 0.2))
    )
    
    # Add Overall Summary Diamond
    addpoly(
      m$res_all,
      row = 1,
      mlab = paste0("Total Pooled (", method, ")"),
      cex = cex_val,
      col = "#650915",
      border = "#650915"
    )
    
    # Add Subgroup Summary Diamonds & Headers
    for (cat in cats) {
      idx <- which(df[[grp_var]] == cat)
      k_cat <- length(idx)
      sub_res <- rma(df$yi[idx], df$vi[idx], method = method)
      
      addpoly(
        sub_res,
        row = cat_poly_rows[[cat]],
        mlab = paste0("Subtotal: ", cat, " (k = ", k_cat, ")"),
        cex = cex_val,
        col = "#C5A059",
        border = "#650915"
      )
      
      text(
        par("usr")[1],
        cat_header_rows[[cat]],
        paste0("Subkelompok: ", cat, " (k = ", k_cat, ")"),
        font = 2,
        pos = 4,
        cex = cex_val
      )
    }
    
    # Footer annotation
    if (!is.null(m$res_mod)) {
      usr <- par("usr")
      text(
        usr[1],
        -0.8,
        sprintf("Uji Beda Subkelompok (QB): QM = %.2f, df = %d, p = %.4f  |  Heterogenitas Residual (QE): QE = %.2f, p = %.4f",
                m$res_mod$QM, m$res_mod$QMdf[1], m$res_mod$QMp, m$res_mod$QE, m$res_mod$QEp),
        pos = 4,
        cex = cex_val * 0.85,
        font = 3
      )
    }
  }
  
  output$subgroup_forest_plot_display <- renderPlot({
    render_subgroup_forest_internal()
  })
  
  # Download Subgroup Forest Plot PNG
  output$btn_dl_subgroup_png <- downloadHandler(
    filename = function() {
      paste0("Subgroup_Forest_Plot_", input$subgroup_var_select, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 3000, height = 2400, res = 300)
      render_subgroup_forest_internal()
      dev.off()
    }
  )
  
  # Download Subgroup Forest Plot PDF
  output$btn_dl_subgroup_pdf <- downloadHandler(
    filename = function() {
      paste0("Subgroup_Forest_Plot_", input$subgroup_var_select, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      pdf(file, width = 11, height = 8.5)
      render_subgroup_forest_internal()
      dev.off()
    }
  )
  
  # Table Subgroup Comprehensive
  output$table_subgroup_comprehensive <- renderTable({
    m <- tryCatch(subgroup_models(), error = function(e) NULL)
    if (is.null(m)) return(data.frame(Status = "Data tidak mencukupi untuk analisis subkelompok."))
    
    df <- m$data
    grp_var <- m$grp_var
    method <- m$method
    cats <- sort(unique(df[[grp_var]]))
    
    res_list <- lapply(cats, function(cat) {
      sub_df <- df[df[[grp_var]] == cat, ]
      k_sub <- nrow(sub_df)
      total_n <- if ("N_Intervention" %in% colnames(sub_df) && "N_Control" %in% colnames(sub_df)) {
        as.character(sum(sub_df$N_Intervention + sub_df$N_Control, na.rm = TRUE))
      } else {
        "-"
      }
      
      sub_res <- tryCatch(rma(yi = yi, vi = vi, data = sub_df, method = method), error = function(e) NULL)
      if (is.null(sub_res)) return(NULL)
      pred <- predict(sub_res)
      
      data.frame(
        Subkelompok = as.character(cat),
        `k (Studi)` = as.character(k_sub),
        `Total N` = total_n,
        `Pooled Hedges' g` = sprintf("%.3f", sub_res$b[1]),
        SE = sprintf("%.3f", sub_res$se[1]),
        `95% CI` = sprintf("[%.3f, %.3f]", sub_res$ci.lb[1], sub_res$ci.ub[1]),
        `95% PI` = if (method != "FE" && k_sub >= 3) sprintf("[%.3f, %.3f]", pred$pi.lb, pred$pi.ub) else "-",
        `z-score` = sprintf("%.3f", sub_res$zval[1]),
        `p-value` = sprintf("%.4f", sub_res$pval[1]),
        `I^2 (%)` = sprintf("%.1f%%", sub_res$I2),
        `tau^2` = sprintf("%.4f", sub_res$tau2),
        `Heterogenitas Qw` = sprintf("%.2f (df = %d, p = %.4f)", sub_res$QE, sub_res$QEdf, sub_res$QEp),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })
    
    do.call(rbind, res_list)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Subgroup Interpretation Narrative
  output$subgroup_interpretation_narrative <- renderUI({
    m <- tryCatch(subgroup_models(), error = function(e) NULL)
    if (is.null(m) || is.null(m$res_mod)) {
      return(p("Model subkelompok belum tersedia."))
    }
    
    df <- m$data
    grp_var <- m$grp_var
    res_mod <- m$res_mod
    cats <- sort(unique(df[[grp_var]]))
    
    qb_sig <- res_mod$QMp < 0.05
    qb_text <- if (qb_sig) {
      paste0(
        "Hasil uji omnibus moderator membuktikan adanya variasi besaran efek antar-subkelompok yang bermakna secara statistik (",
        "<em>QB</em> = ", sprintf("%.2f, df = %d, p = %.4f", res_mod$QM, res_mod$QMdf[1], res_mod$QMp), "). ",
        "Temuan ini mengindikasikan bahwa karakteristik <strong>", grp_var,
        "</strong> berperan sebagai moderator substantif yang menjelaskan heterogenitas efektivitas intervensi antarstudi klinis, ",
        "dengan proporsi varians sejati antarstudi yang berhasil dijelaskan mencapai <strong>",
        sprintf("%.1f%%", ifelse(!is.null(res_mod$R2) && !is.na(res_mod$R2), res_mod$R2, 0)), "</strong>."
      )
    } else {
      paste0(
        "Hasil uji omnibus moderator mengindikasikan bahwa perbedaan besaran efek antar-subkelompok belum mencapai signifikansi statistik konvensional (",
        "<em>QB</em> = ", sprintf("%.2f, df = %d, p = %.4f", res_mod$QM, res_mod$QMdf[1], res_mod$QMp), "). ",
        "Meskipun demikian, variasi parameter deskriptif antar-kategori tetap memberikan gambaran diferensial yang penting secara klinis."
      )
    }
    
    sub_details <- lapply(cats, function(cat) {
      sub_df <- df[df[[grp_var]] == cat, ]
      k_sub <- nrow(sub_df)
      sub_res <- tryCatch(rma(yi = yi, vi = vi, data = sub_df, method = m$method), error = function(e) NULL)
      if (is.null(sub_res)) return(NULL)
      
      mag <- ifelse(abs(sub_res$b[1]) < 0.2, "dapat diabaikan",
                    ifelse(abs(sub_res$b[1]) < 0.5, "kecil",
                           ifelse(abs(sub_res$b[1]) < 0.8, "sedang", "besar")))
      
      tags$li(
        strong(paste0(cat, " (k = ", k_sub, "): ")),
        sprintf("Pooled Hedges' g = %.3f [%.3f, %.3f], p = %.4f (kategori efek %s). ", sub_res$b[1], sub_res$ci.lb[1], sub_res$ci.ub[1], sub_res$pval[1], mag),
        sprintf("Tingkat heterogenitas internal I^2 = %.1f%% (tau^2 = %.4f).", sub_res$I2, sub_res$tau2)
      )
    })
    
    psy_context <- if (grp_var == "Control_Type") {
      p(
        "Secara khusus pada telaah tipe kelompok kontrol, variasi besaran efek mencerminkan fenomena atenuasi akibat komparator aktif: ",
        "uji coba dengan kelompok pembanding ", tags$em("Waitlist"), " secara konsisten menghasilkan estimasi besaran efek intervensi yang paling tinggi. ",
        "Sebaliknya, pengujian terhadap ", tags$em("Active Control"), " (misal: ", tags$em("Relaxation Training"), " atau ", tags$em("Supportive Counseling"), ") ",
        "menghasilkan estimasi besaran efek yang lebih teratenuasi secara substansial. ",
        "Hal tersebut menegaskan bahwa kelompok kontrol aktif berhasil mengendalikan faktor-faktor terapeutik non-spesifik seperti ekspektasi pemulihan dan kontak relasional terapis (Cuijpers et al., 2023)."
      )
    } else if (grp_var == "Format") {
      p(
        "Secara khusus pada telaah format penghantaran intervensi, format ", tags$em("Individual"), " dan ", tags$em("Group"), " tatap muka ",
        "memperlihatkan besaran efek yang relatif lebih tinggi dibandingkan format ", tags$em("Guided Digital"), ". ",
        "Meskipun intervensi digital terpandu menawarkan skalabilitas implementasi yang luas, perbedaan intensitas relasi terapeutik (", tags$em("therapeutic alliance"), ") ",
        "memberikan kontribusi nyata terhadap variabilitas respon klinis partisipan."
      )
    } else {
      NULL
    }
    
    tagList(
      h5("Sintesis Komparatif Subkelompok & Implikasi Klinis", class = "text-primary fw-bold"),
      p(HTML(qb_text)),
      h6("Rincian Estimasi per Subkelompok:", class = "fw-bold mt-3"),
      tags$ul(sub_details),
      psy_context,
      p(
        "Evaluasi terhadap heterogenitas residual menghasilkan ",
        strong(sprintf("QE = %.2f (df = %d, p = %.4f)", res_mod$QE, res_mod$QEdf, res_mod$QEp)), ". ",
        ifelse(res_mod$QEp < 0.05,
               "Heterogenitas residual yang masih signifikan mengindikasikan adanya faktor perancu atau karakteristik metodologis lain yang belum sepenuhnya terakomodasi dalam pengelompokan ini.",
               "Nilai p residual yang tidak signifikan mengindikasikan bahwa sebagian besar heterogenitas antarstudi telah berhasil dieliminasi melalui stratifikasi subkelompok ini.")
      )
    )
  })
  
  # Helper to Prepare Forest Plot Data & Object
  render_forest_internal <- function() {
    df <- active_data()
    req(nrow(df) >= 2)
    
    # Ordering
    if (input$forest_sort == "yi_asc") {
      df <- df[order(df$yi), ]
    } else if (input$forest_sort == "yi_desc") {
      df <- df[order(df$yi, decreasing = TRUE), ]
    } else if (input$forest_sort == "year" && "Year" %in% colnames(df)) {
      df <- df[order(df$Year), ]
    } else if (input$forest_sort == "control" && "Control_Type" %in% colnames(df)) {
      df <- df[order(df$Control_Type), ]
    } else if (input$forest_sort == "format" && "Format" %in% colnames(df)) {
      df <- df[order(df$Format), ]
    } else if (input$forest_sort == "interv" && "Intervention" %in% colnames(df)) {
      df <- df[order(df$Intervention), ]
    } else if (input$forest_sort == "rob" && "Risk_of_Bias" %in% colnames(df)) {
      df <- df[order(df$Risk_of_Bias), ]
    }
    
    # Fit model for ordered dataset
    res <- rma(
      yi = yi,
      vi = vi,
      data = df,
      method = input$meta_estimator,
      level = as.numeric(input$conf_level) * 100
    )
    
    # Study labels
    if (input$forest_sort %in% c("format", "control", "interv", "rob")) {
      mod_tag <- switch(
        input$forest_sort,
        "format" = df$Format,
        "control" = ifelse(df$Control_Type == "Active Control" & "Active_Comparator" %in% colnames(df) & !is.na(df$Active_Comparator),
                           paste0("Active: ", df$Active_Comparator),
                           df$Control_Type),
        "interv" = df$Intervention,
        "rob" = df$Risk_of_Bias
      )
      slabs <- paste0(df$Study, " (", df$Year, ") [", mod_tag, "]")
    } else if ("Year" %in% colnames(df)) {
      slabs <- paste0(df$Study, " (", df$Year, ")")
    } else {
      slabs <- df$Study
    }
    
    forest(
      res,
      slab = slabs,
      xlab = "Besaran Efek Intervensi (Hedges' g)",
      mlab = paste0("Model Efek Acak (", input$meta_estimator, ")"),
      header = c("Studi Primer & Tahun", "Hedges' g [95% CI]"),
      addpred = input$forest_show_pi && input$meta_estimator != "FE",
      showweights = input$forest_show_weights,
      cex = input$forest_cex,
      col = "#650915",
      alim = c(min(-0.5, min(df$yi - 1.96 * sqrt(df$vi)) - 0.2), max(1.5, max(df$yi + 1.96 * sqrt(df$vi)) + 0.2)),
      refline = 0
    )
    
    # Axis sub-labels
    usr <- par("usr")
    text(usr[1] + 0.2, usr[3] - 0.8, input$forest_label_left, pos = 4, cex = input$forest_cex * 0.85, font = 3)
    text(usr[2] - 0.2, usr[3] - 0.8, input$forest_label_right, pos = 2, cex = input$forest_cex * 0.85, font = 3)
  }
  
  # Render Forest Plot
  output$forest_plot_display <- renderPlot({
    render_forest_internal()
  })
  
  # Download Forest Plot PNG
  output$btn_dl_forest_png <- downloadHandler(
    filename = function() {
      paste0("Forest_Plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 2800, height = 2000, res = 300)
      render_forest_internal()
      dev.off()
    }
  )
  
  # Download Forest Plot PDF
  output$btn_dl_forest_pdf <- downloadHandler(
    filename = function() {
      paste0("Forest_Plot_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      pdf(file, width = 10, height = 7.5)
      render_forest_internal()
      dev.off()
    }
  )
  
  # Helper to Render Funnel Plot
  render_funnel_internal <- function() {
    df <- active_data()
    req(nrow(df) >= 2)
    
    res <- rma(yi = yi, vi = vi, data = df, method = input$meta_estimator)
    
    if (input$funnel_type == "contour") {
      funnel(
        res,
        level = c(90, 95, 99),
        shade = c("#FFFFFF", "#FAF8F5", "#F2EBE1"),
        refline = 0,
        legend = TRUE,
        xlab = "Besaran Efek Intervensi (Hedges' g)",
        ylab = "Standar Error (SE)"
      )
    } else {
      funnel(
        res,
        xlab = "Besaran Efek Intervensi (Hedges' g)",
        ylab = "Standar Error (SE)"
      )
    }
  }
  
  # Render Funnel Plot
  output$funnel_plot_display <- renderPlot({
    render_funnel_internal()
  })
  
  # Download Funnel PNG
  output$btn_dl_funnel_png <- downloadHandler(
    filename = function() {
      paste0("Funnel_Plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 2400, height = 1800, res = 300)
      render_funnel_internal()
      dev.off()
    }
  )
  
  # Download Funnel PDF
  output$btn_dl_funnel_pdf <- downloadHandler(
    filename = function() {
      paste0("Funnel_Plot_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      pdf(file, width = 8, height = 6)
      render_funnel_internal()
      dev.off()
    }
  )
  
  # Egger's Test Table
  output$table_egger_test <- renderTable({
    df <- active_data()
    req(nrow(df) >= 3)
    res <- tryCatch(rma(yi = yi, vi = vi, data = df, method = input$meta_estimator), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    
    eg <- tryCatch(regtest(res, model = "rma", predictor = "sei"), error = function(e) NULL)
    if (is.null(eg)) {
      return(data.frame(Status = "Uji Egger tidak dapat dikomputasi (k terlalu kecil)."))
    }
    
    data.frame(
      Metrik = c("Statistik Uji Asimetri (z)", "Nilai p (p-value)", "Estimasi Intersep / Limit (b0)", "Batas 95% CI Intersep", "Kesimpulan Bias Publikasi"),
      Nilai = c(
        sprintf("%.3f", eg$zval),
        sprintf("%.4f", eg$pval),
        sprintf("%.3f", eg$est),
        sprintf("[%.3f, %.3f]", eg$ci.lb, eg$ci.ub),
        ifelse(eg$pval < 0.05, "Terindikasi Asimetri Bermakna (Potensi Bias Publikasi)", "Tidak Terindikasi Asimetri Bermakna")
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Begg's Rank Test Table
  output$table_begg_test <- renderTable({
    df <- active_data()
    req(nrow(df) >= 3)
    res <- tryCatch(rma(yi = yi, vi = vi, data = df, method = input$meta_estimator), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    
    bg <- tryCatch(ranktest(res), error = function(e) NULL)
    if (is.null(bg)) {
      return(data.frame(Status = "Uji Begg tidak dapat dikomputasi."))
    }
    
    data.frame(
      Metrik = c("Statistik Kendall's Tau", "Nilai p (p-value)", "Kesimpulan"),
      Nilai = c(
        sprintf("%.3f", bg$tau),
        sprintf("%.4f", bg$pval),
        ifelse(bg$pval < 0.05, "Terindikasi Korelasi Asimetri Signifikan", "Korelasi Peringkat Tidak Signifikan")
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Trim-and-Fill Summary Table
  output$table_trimfill_summary <- renderTable({
    df <- active_data()
    req(nrow(df) >= 3)
    res <- tryCatch(rma(yi = yi, vi = vi, data = df, method = input$meta_estimator), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    
    tf <- tryCatch(trimfill(res), error = function(e) NULL)
    if (is.null(tf)) {
      return(data.frame(Status = "Analisis Trim-and-Fill tidak dapat dijalankan."))
    }
    
    data.frame(
      Parameter = c(
        "Estimasi Jumlah Studi Hilang (k0)",
        "Sisi Asimetri Imputasi",
        "Pooled Effect Asli (Sebelum Disesuaikan)",
        "Adjusted Pooled Effect (Setelah Imputasi)",
        "Pergeseran Besaran Efek"
      ),
      Nilai = c(
        as.character(tf$k0),
        tf$side,
        sprintf("%.3f [%.3f, %.3f]", res$b[1], res$ci.lb[1], res$ci.ub[1]),
        sprintf("%.3f [%.3f, %.3f]", tf$b[1], tf$ci.lb[1], tf$ci.ub[1]),
        sprintf("%.3f (Atenuasi Efek)", tf$b[1] - res$b[1])
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

# Run Application
shinyApp(ui = ui, server = server)
