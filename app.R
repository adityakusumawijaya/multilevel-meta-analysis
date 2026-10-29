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
    Intervention = c("CBT", "CT", "iCBT", "CBT", "iCBT", "DBT", "CT", "UP", "CBT", "iCBT", "BA", "CBT", "BA", "iCBT"),
    Format = c("Individual", "Individual", "Online", "Group", "Online", "Individual", "Individual", "Group", "Individual", "Online", "Individual", "Group", "Individual", "Online"),
    Duration_Weeks = c(12, 16, 8, 12, 10, 24, 12, 16, 12, 8, 10, 14, 16, 8),
    N_Intervention = c(45, 52, 110, 60, 95, 40, 48, 55, 62, 120, 50, 70, 55, 115),
    N_Control = c(42, 50, 105, 58, 90, 38, 46, 52, 60, 115, 48, 68, 52, 110),
    yi = c(0.820, 0.640, 0.280, 0.540, 0.320, 0.880, 0.700, 0.460, 0.650, 0.240, 0.610, 0.480, 0.660, 0.290),
    vi = c(0.0499, 0.0412, 0.0188, 0.0351, 0.0219, 0.0563, 0.0452, 0.0384, 0.0345, 0.0172, 0.0427, 0.0298, 0.0394, 0.0180),
    sei = c(0.2234, 0.2031, 0.1371, 0.1875, 0.1480, 0.2372, 0.2126, 0.1960, 0.1858, 0.1310, 0.2067, 0.1727, 0.1986, 0.1341),
    stringsAsFactors = FALSE
  )
}

# UI Definition
ui <- page_navbar(
  title = "Intervention Meta-Analysis Studio",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1e3a8a",
    secondary = "#475569",
    success = "#059669",
    info = "#0284c7",
    warning = "#d97706",
    danger = "#dc2626"
  ),
  
  # Tab 1: Kalkulator Effect Size
  nav_panel(
    title = "Kalkulator Effect Size (esc)",
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
        
        h6("Identitas & Moderator Studi:", class = "fw-bold"),
        textInput("study_name", "Nama Peneliti / Studi:", value = "Studi Baru et al."),
        layout_columns(
          col_widths = c(6, 6),
          numericInput("study_year", "Tahun:", value = 2024, min = 1950, max = 2035, step = 1),
          numericInput("study_dur", "Durasi (Mg):", value = 12, min = 1, step = 1)
        ),
        layout_columns(
          col_widths = c(6, 6),
          textInput("study_interv", "Intervensi:", value = "CBT"),
          selectInput("study_format", "Format:", choices = c("Individual", "Group", "Online"))
        ),
        
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
  
  # Tab 2: Dataset Meta-Analisis
  nav_panel(
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
    title = "Pemodelan Meta-Analisis (metafor)",
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
              title = "Analisis Moderator (Meta-Regresi)",
              verbatimTextOutput("meta_moderator_summary")
            )
          )
        )
      )
    )
  ),
  
  # Tab 4: Forest Plot
  nav_panel(
    title = "Forest Plot Publikasi",
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
            "Tahun Publikasi" = "year"
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
    title = "Diagnostik Bias Publikasi & Funnel Plot",
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
  
  # Reactive Data Storage
  active_data <- reactiveVal(create_benchmark_data())
  calc_result <- reactiveVal(NULL)
  
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
        choices <- c("Tanpa Moderator (Overall Pooled)" = "none", setNames(valid_mods, valid_mods))
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

    new_row <- data.frame(
      Study = input$study_name,
      Year = input$study_year,
      Intervention = input$study_interv,
      Format = input$study_format,
      Duration_Weeks = input$study_dur,
      N_Intervention = n1_val,
      N_Control = n2_val,
      yi = round(res$es, 4),
      vi = round(res$var, 5),
      sei = round(res$se, 4),
      stringsAsFactors = FALSE
    )
    
    active_data(rbind(current_df, new_row))
    showNotification(paste("Studi '", input$study_name, "' berhasil ditambahkan ke dataset."), type = "message")
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
  
  # Moderator Model Summary
  output$meta_moderator_summary <- renderPrint({
    if (input$mod_variable == "none") {
      cat("Analisis moderator dinonaktifkan. Pilih salah satu variabel pada bilah konfigurasi samping.")
    } else {
      res <- tryCatch(meta_model(), error = function(e) e)
      if (inherits(res, "error")) {
        cat("Gagal menjalankan meta-regresi:\n", res$message)
      } else {
        print(res)
      }
    }
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
    if ("Year" %in% colnames(df)) {
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
      col = "#1e3a8a",
      border = "#1e3a8a",
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
        shade = c("white", "gray75", "gray90"),
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
