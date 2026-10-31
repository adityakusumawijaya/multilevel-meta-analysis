# Analysis: Effect Size Estimation and Clinical Intervention Meta-Analysis (Shiny Application)
# Data: Interactive Entry / CSV Upload / Intervention Benchmark Data (k = 14)
# Packages: shiny, bslib, metafor, esc, DT, ggplot2, grid

library(shiny)
library(bslib)
library(metafor)
library(esc)
library(DT)
library(ggplot2)
library(grid)

# Benchmark Dataset Generator (k = 14 RCT Trials in Clinical Psychology)
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

# Sample Batch Generator for Calculator (k = 5 — CBT for Depression in Adults with ACEs)
create_calc_sample_batch <- function() {
  data.frame(
    Study = c("Aardal et al", "Ghrun et al", "Krakau et al", "Malkomsen et al", "Wagner et al"),
    Year = c(2026, 2021, 2024, 2025, 2024),
    Intervention = c("CBT", "CBT", "CBT", "CBT", "CBT"),
    Format = c("Interaction", "Main", "Interaction", "Interaction", "Main"),
    Control_Type = c("TAU", "Waitlist", "Active Control", "TAU", "Waitlist"),
    Active_Comparator = c("Treatment as Usual", "None (Waitlist)", "Psychodynamic Therapy", "Treatment as Usual", "None (Waitlist)"),
    Population = c("Non-US", "US", "Non-US", "Non-US", "Non-US"),
    Age_Group = c("Adults", "Adults", "Adults", "Adults", "Adults"),
    Therapist_Level = c("Licensed Clinician", "Licensed Clinician", "Licensed Clinician", "Licensed Clinician", "Licensed Clinician"),
    Risk_of_Bias = c("Low Risk", "Some Concerns", "Low Risk", "Low Risk", "Some Concerns"),
    Duration_Weeks = c(13.5, 12.0, 260.0, 28.0, 6.0),
    Total_Sessions = c(14, 12, 40, 20, 6),
    N_Intervention = c(48, 50, 104, 50, 38),
    N_Control = c(52, 46, 148, 49, 34),
    yi = c(0.5053, -0.0797, -0.3087, 0.2812, 0.8206),
    vi = c(0.2034^2, 0.4099^2, 0.1287^2, 0.2020^2, 0.4807^2),
    sei = c(0.2034, 0.4099, 0.1287, 0.2020, 0.4807),
    stringsAsFactors = FALSE
  )
}

# Dynamic Effect Size Formula Builder for Active Calculator Input
build_active_formula_ui <- function(itype, metric, input_list, res) {
  es_val <- if (!is.null(res) && is.finite(res$es)) res$es else 0.50
  var_val <- if (!is.null(res) && is.finite(res$var)) res$var else 0.05
  se_val <- if (!is.null(res) && is.finite(res$se)) res$se else sqrt(var_val)
  ci_lo <- if (!is.null(res) && is.finite(res$ci.lo)) res$ci.lo else (es_val - 1.96 * se_val)
  ci_hi <- if (!is.null(res) && is.finite(res$ci.hi)) res$ci.hi else (es_val + 1.96 * se_val)
  
  metric_name <- ifelse(metric == "g", "Hedges' g (Unbiased)", "Cohen's d")
  
  if (itype == "mean_sd") {
    n1 <- input_list$grp1n_sd; m1 <- input_list$grp1m_sd; s1 <- input_list$grp1sd_sd
    n2 <- input_list$grp2n_sd; m2 <- input_list$grp2m_sd; s2 <- input_list$grp2sd_sd
    dir_txt <- ifelse(input_list$sd_outcome_dir == "decrease_better", "Skor Lebih Rendah = Efektif (Simtom Menurun)", "Skor Lebih Tinggi = Efektif")
    df_val <- n1 + n2 - 2
    sp_val <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / df_val)
    raw_diff <- if (input_list$sd_outcome_dir == "decrease_better") (m2 - m1) else (m1 - m2)
    d_val <- raw_diff / sp_val
    j_val <- 1 - (3 / (4 * (n1 + n2) - 9))
    g_val <- d_val * j_val
    vd_val <- (n1 + n2) / (n1 * n2) + (d_val^2) / (2 * (n1 + n2))
    vg_val <- if (metric == "g") (j_val^2 * vd_val) else vd_val
    
    tagList(
      div(
        class = "kawe-math-card",
        div(class = "kawe-formula-title", icon("square-root-alt"), " Metode: Post-Test Means & Standard Deviations (Dua Kelompok Independen)"),
        div(class = "kawe-formula-sub", "Rujukan Baku: Cohen (1988); Hedges (1981); Borenstein et al. (2009)."),
        p("Mengestimasi perbedaan rata-rata terstandarisasi antara kelompok intervensi dan kontrol pascates menggunakan deviasi baku gabungan (", tags$em("pooled standard deviation"), ")."),
        
        div(class = "fw-bold text-primary mt-3 mb-1", "1. Landasan Formulasi Matematis:"),
        div(class = "kawe-eq-box",
          HTML("$$\\text{Selisih Rata-rata: } \\Delta\\bar{X} = \\bar{X}_1 - \\bar{X}_2$$"),
          HTML("$$\\text{Deviasi Baku Gabungan: } s_{\\text{pooled}} = \\sqrt{\\frac{(n_1 - 1)s_1^2 + (n_2 - 1)s_2^2}{n_1 + n_2 - 2}}$$"),
          HTML("$$\\text{Cohen's } d = \\frac{\\Delta\\bar{X}}{s_{\\text{pooled}}}$$"),
          HTML("$$\\text{Faktor Koreksi Hedges: } J = 1 - \\frac{3}{4(n_1 + n_2) - 9}$$"),
          HTML("$$\\text{Hedges' } g = J \\times d$$"),
          HTML("$$\\text{Varians Sampling: } v_d = \\frac{n_1 + n_2}{n_1 n_2} + \\frac{d^2}{2(n_1 + n_2)}, \\quad v_g = J^2 \\times v_d$$"),
          HTML("$$\\text{Standar Error: } SE = \\sqrt{v}, \\quad 95\\% \\text{ CI} = [\\text{ES} \\pm 1.96 \\cdot SE]$$")
        ),
        
        div(class = "fw-bold text-primary mt-3 mb-1", "2. Langkah Perhitungan Terbuka & Substitusi Angka Riil:"),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Langkah 1"),
          strong("Ukuran Sampel & Derajat Kebebasan: "),
          sprintf("n1 = %d, n2 = %d, Total N = %d, Derajat Kebebasan (df) = %d + %d - 2 = %d.", n1, n2, n1 + n2, n1, n2, df_val)
        ),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Langkah 2"),
          strong("Selisih Rata-rata Terarah: "),
          sprintf("Mean1 = %.2f, Mean2 = %.2f | Orientasi: %s -> Delta Mean = %.3f.", m1, m2, dir_txt, raw_diff)
        ),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Langkah 3"),
          strong("Deviasi Baku Gabungan (s_pooled): "),
          div(class = "kawe-eq-box",
            HTML(sprintf("$$s_{\\text{pooled}} = \\sqrt{\\frac{(%d - 1)(%.2f^2) + (%d - 1)(%.2f^2)}{%d}} = \\sqrt{\\frac{%.2f + %.2f}{%d}} = %.4f$$",
                         n1, s1, n2, s2, df_val, (n1-1)*s1^2, (n2-1)*s2^2, df_val, sp_val))
          )
        ),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Langkah 4"),
          strong("Standardized Mean Difference (Cohen's d): "),
          div(class = "kawe-eq-box",
            HTML(sprintf("$$d = \\frac{%.3f}{%.4f} = %.4f$$", raw_diff, sp_val, d_val))
          )
        ),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Langkah 5"),
          strong("Faktor Pembobot Koreksi Bias Sampel Kecil Hedges (J): "),
          div(class = "kawe-eq-box",
            HTML(sprintf("$$J = 1 - \\frac{3}{4(%d) - 9} = 1 - \\frac{3}{%d} = %.5f$$", n1 + n2, 4 * (n1 + n2) - 9, j_val))
          ),
          sprintf("Estimasi Titik Akhir: %s = %.4f.", metric_name, es_val)
        ),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Langkah 6"),
          strong("Varians Sampling, Standar Error & 95% Confidence Interval: "),
          div(class = "kawe-eq-box",
            HTML(sprintf("$$v = %.5f, \\quad SE = \\sqrt{%.5f} = %.4f$$", var_val, var_val, se_val)),
            HTML(sprintf("$$95\\%% \\text{ CI} = [%.4f - 1.96(%.4f), \\; %.4f + 1.96(%.4f)] = [%.4f, \\; %.4f]$$",
                         es_val, se_val, es_val, se_val, ci_lo, ci_hi))
          )
        )
      )
    )
  } else if (itype == "mean_se") {
    n1 <- input_list$grp1n_se; m1 <- input_list$grp1m_se; se1 <- input_list$grp1se_se
    n2 <- input_list$grp2n_se; m2 <- input_list$grp2m_se; se2 <- input_list$grp2se_se
    dir_txt <- ifelse(input_list$se_outcome_dir == "decrease_better", "Skor Lebih Rendah = Efektif (Simtom Menurun)", "Skor Lebih Tinggi = Efektif")
    s1_est <- se1 * sqrt(n1)
    s2_est <- se2 * sqrt(n2)
    df_val <- n1 + n2 - 2
    sp_val <- sqrt(((n1 - 1) * s1_est^2 + (n2 - 1) * s2_est^2) / df_val)
    raw_diff <- if (input_list$se_outcome_dir == "decrease_better") (m2 - m1) else (m1 - m2)
    d_val <- raw_diff / sp_val
    j_val <- 1 - (3 / (4 * (n1 + n2) - 9))
    
    tagList(
      div(
        class = "kawe-math-card",
        div(class = "kawe-formula-title", icon("square-root-alt"), " Metode: Means & Standard Errors (Dua Kelompok Independen)"),
        div(class = "kawe-formula-sub", "Rujukan Baku: Higgins et al. (2019); Cochrane Handbook; Borenstein et al. (2009)."),
        p("Ketika deviasi baku (SD) tidak dilaporkan, deviasi baku direkonstruksi matematis dari Standar Error (SE) dan ukuran sampel n."),
        
        div(class = "fw-bold text-primary mt-3 mb-1", "1. Landasan Formulasi Matematis:"),
        div(class = "kawe-eq-box",
          HTML("$$s_1 = SE_1 \\times \\sqrt{n_1}, \\quad s_2 = SE_2 \\times \\sqrt{n_2}$$"),
          HTML("$$s_{\\text{pooled}} = \\sqrt{\\frac{(n_1 - 1)s_1^2 + (n_2 - 1)s_2^2}{n_1 + n_2 - 2}}$$"),
          HTML("$$d = \\frac{\\Delta\\bar{X}}{s_{\\text{pooled}}}, \\quad J = 1 - \\frac{3}{4(n_1 + n_2) - 9}, \\quad g = J \\times d$$")
        ),
        
        div(class = "fw-bold text-primary mt-3 mb-1", "2. Langkah Perhitungan Terbuka & Substitusi Angka Riil:"),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Langkah 1"),
          strong("Rekonstruksi Deviasi Baku: "),
          sprintf("Kelompok 1: s1 = %.3f * sqrt(%d) = %.4f | Kelompok 2: s2 = %.3f * sqrt(%d) = %.4f.", se1, n1, s1_est, se2, n2, s2_est)
        ),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Langkah 2"),
          strong("Deviasi Baku Gabungan (s_pooled): "),
          div(class = "kawe-eq-box",
            HTML(sprintf("$$s_{\\text{pooled}} = \\sqrt{\\frac{(%d - 1)(%.4f^2) + (%d - 1)(%.4f^2)}{%d}} = %.4f$$", n1, s1_est, n2, s2_est, df_val, sp_val))
          )
        ),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Langkah 3"),
          strong("Estimasi Akhir Besaran Efek: "),
          sprintf("Cohen's d = %.4f | Faktor J = %.5f | %s = %.4f (SE = %.4f, 95%% CI [%.4f, %.4f]).",
                  d_val, j_val, metric_name, es_val, se_val, ci_lo, ci_hi)
        )
      )
    )
  } else if (itype == "mean_gain") {
    gsub <- input_list$gain_sub_type
    r_val <- input_list$gain_r
    dir_txt <- ifelse(input_list$gain_outcome_dir == "decrease_better", "Skor Menurun = Membaik", "Skor Meningkat = Membaik")
    
    tagList(
      div(
        class = "kawe-math-card",
        div(class = "kawe-formula-title", icon("square-root-alt"), " Metode: Mean Gain / Perubahan Pre-Post (Repeated Measures Designs)"),
        div(class = "kawe-formula-sub", "Rujukan Baku: Becker (1988); Morris (2008); Rosenthal (1991); Lipsey & Wilson (2001)."),
        p("Mengestimasi besaran efek dari selisih perubahan skor pre-post antara kelompok intervensi dan kontrol dengan mempertimbangkan korelasi berpasangan (", tags$em("paired correlation r"), ")."),
        
        div(class = "fw-bold text-primary mt-3 mb-1", "1. Landasan Formulasi Matematis:"),
        div(class = "kawe-eq-box",
          HTML("$$\\text{Gain}_1 = \\bar{X}_{post, 1} - \\bar{X}_{pre, 1}, \\quad \\text{Gain}_2 = \\bar{X}_{post, 2} - \\bar{X}_{pre, 2}$$"),
          HTML("$$s_{\\text{change}, i} = \\sqrt{s_{pre, i}^2 + s_{post, i}^2 - 2 r s_{pre, i} s_{post, i}}$$"),
          HTML("$$s_{\\text{pooled, change}} = \\sqrt{\\frac{(n_1 - 1)s_{\\text{change}, 1}^2 + (n_2 - 1)s_{\\text{change}, 2}^2}{n_1 + n_2 - 2}}$$"),
          HTML("$$d_{\\text{gain}} = \\frac{\\text{Gain}_1 - \\text{Gain}_2}{s_{\\text{pooled, change}}}, \\quad g = J \\times d$$"),
          HTML("$$v_d = \\frac{2(1 - r)}{n_1} + \\frac{2(1 - r)}{n_2} + \\frac{d^2}{2(n_1 + n_2)}, \\quad v_g = J^2 \\times v_d$$")
        ),
        
        div(class = "fw-bold text-primary mt-3 mb-1", "2. Langkah Perhitungan Terbuka & Substitusi Angka Riil:"),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Parameter"),
          sprintf("Pola Data: %s | Orientasi Klinis: %s | Asumsi Korelasi Pre-Post (r) = %.2f.",
                  ifelse(gsub == "pre_post_full", "Pre & Post Lengkap", "Perubahan Pre-Post Langsung"), dir_txt, r_val)
        ),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Hasil Estimasi"),
          sprintf("Estimasi Titik %s = %.4f | Varians vi = %.5f | SE = %.4f | 95%% CI [%.4f, %.4f].",
                  metric_name, es_val, var_val, se_val, ci_lo, ci_hi)
        )
      )
    )
  } else if (itype == "t_test") {
    t_val <- input_list$t_val
    n1 <- input_list$t_n1; n2 <- input_list$t_n2
    df_val <- n1 + n2 - 2
    d_val <- t_val * sqrt((n1 + n2) / (n1 * n2))
    j_val <- 1 - (3 / (4 * (n1 + n2) - 9))
    g_val <- d_val * j_val
    
    tagList(
      div(
        class = "kawe-math-card",
        div(class = "kawe-formula-title", icon("square-root-alt"), " Metode: Uji-t Dua Kelompok Independen (Independent Student's t-test)"),
        div(class = "kawe-formula-sub", "Rujukan Baku: Dunlap et al. (1996); Rosenthal (1991); Lipsey & Wilson (2001)."),
        p("Mengonversi statistik uji-t independen langsung ke standardized mean difference Cohen's d dan Hedges' g."),
        
        div(class = "fw-bold text-primary mt-3 mb-1", "1. Landasan Formulasi Matematis:"),
        div(class = "kawe-eq-box",
          HTML("$$d = t \\times \\sqrt{\\frac{n_1 + n_2}{n_1 n_2}} = t \\times \\sqrt{\\frac{1}{n_1} + \\frac{1}{n_2}}$$"),
          HTML("$$J = 1 - \\frac{3}{4(n_1 + n_2) - 9}, \\quad g = J \\times d$$"),
          HTML("$$v_d = \\frac{n_1 + n_2}{n_1 n_2} + \\frac{d^2}{2(n_1 + n_2)}, \\quad v_g = J^2 \\times v_d$$")
        ),
        
        div(class = "fw-bold text-primary mt-3 mb-1", "2. Langkah Perhitungan Terbuka & Substitusi Angka Riil:"),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Langkah 1"),
          sprintf("Nilai Statistik t = %.3f | Ukuran Kelompok: n1 = %d, n2 = %d (df = %d).", t_val, n1, n2, df_val)
        ),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Langkah 2"),
          div(class = "kawe-eq-box",
            HTML(sprintf("$$d = %.3f \\times \\sqrt{\\frac{%d + %d}{(%d)(%d)}} = %.3f \\times \\sqrt{%.5f} = %.4f$$",
                         t_val, n1, n2, n1, n2, t_val, (n1+n2)/(n1*n2), d_val))
          )
        ),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Langkah 3"),
          sprintf("Faktor Koreksi J = %.5f -> %s = %.4f (SE = %.4f, 95%% CI [%.4f, %.4f]).",
                  j_val, metric_name, es_val, se_val, ci_lo, ci_hi)
        )
      )
    )
  } else if (itype == "f_test") {
    f_val <- input_list$f_val
    n1 <- input_list$f_n1; n2 <- input_list$f_n2
    t_equiv <- sqrt(f_val)
    if (input_list$f_direction == "neg") t_equiv <- -t_equiv
    d_val <- t_equiv * sqrt((n1 + n2) / (n1 * n2))
    j_val <- 1 - (3 / (4 * (n1 + n2) - 9))
    
    tagList(
      div(
        class = "kawe-math-card",
        div(class = "kawe-formula-title", icon("square-root-alt"), " Metode: Uji-F Satu Arah (One-Way ANOVA df1 = 1)"),
        div(class = "kawe-formula-sub", "Rujukan Baku: Lipsey & Wilson (2001); Borenstein et al. (2009)."),
        p("Memanfaatkan kesetaraan statistik antara distribusi F dengan derajat kebebasan numerator df1 = 1 dan distribusi Student's t: t = sqrt(F)."),
        
        div(class = "fw-bold text-primary mt-3 mb-1", "1. Landasan Formulasi Matematis:"),
        div(class = "kawe-eq-box",
          HTML("$$|t| = \\sqrt{F_{(1, df)}}, \\quad d = \\text{sign} \\times \\sqrt{F \\left(\\frac{n_1 + n_2}{n_1 n_2}\\right)}$$"),
          HTML("$$J = 1 - \\frac{3}{4(n_1 + n_2) - 9}, \\quad g = J \\times d$$")
        ),
        
        div(class = "fw-bold text-primary mt-3 mb-1", "2. Langkah Perhitungan Terbuka & Substitusi Angka Riil:"),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Langkah 1"),
          sprintf("Nilai F = %.3f -> Ekivalen t = sqrt(%.3f) = %.4f (Arah: %s).",
                  f_val, f_val, abs(t_equiv), ifelse(input_list$f_direction == "pos", "Memihak Intervensi", "Memihak Kontrol"))
        ),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Langkah 2"),
          sprintf("Cohen's d = %.4f | Faktor J = %.5f | %s = %.4f (SE = %.4f, 95%% CI [%.4f, %.4f]).",
                  d_val, j_val, metric_name, es_val, se_val, ci_lo, ci_hi)
        )
      )
    )
  } else if (itype == "p_val") {
    p_val <- input_list$p_value
    n1 <- input_list$p_n1; n2 <- input_list$p_n2
    df_val <- n1 + n2 - 2
    t_equiv <- qt(1 - (p_val / 2), df_val)
    if (input_list$p_direction == "neg") t_equiv <- -t_equiv
    d_val <- t_equiv * sqrt((n1 + n2) / (n1 * n2))
    j_val <- 1 - (3 / (4 * (n1 + n2) - 9))
    
    tagList(
      div(
        class = "kawe-math-card",
        div(class = "kawe-formula-title", icon("square-root-alt"), " Metode: Nilai-p Dua Arah & Ukuran Sampel (Inverse Student's t-Distribution)"),
        div(class = "kawe-formula-sub", "Rujukan Baku: Rosenthal (1991); Lipsey & Wilson (2001)."),
        p("Ketika hanya nilai signifikansi statistik (p-value) dan ukuran sampel yang dilaporkan, nilai t direkonstruksi via fungsi inversi kuantil distribusi Student's t."),
        
        div(class = "fw-bold text-primary mt-3 mb-1", "1. Landasan Formulasi Matematis:"),
        div(class = "kawe-eq-box",
          HTML("$$df = n_1 + n_2 - 2, \\quad t = F_{t, df}^{-1}\\left(1 - \\frac{p}{2}\\right)$$"),
          HTML("$$d = \\text{sign} \\times t \\times \\sqrt{\\frac{n_1 + n_2}{n_1 n_2}}, \\quad g = J \\times d$$")
        ),
        
        div(class = "fw-bold text-primary mt-3 mb-1", "2. Langkah Perhitungan Terbuka & Substitusi Angka Riil:"),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Langkah 1"),
          sprintf("Nilai p dua arah = %.4f | Sampel: n1 = %d, n2 = %d (df = %d).", p_val, n1, n2, df_val)
        ),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Langkah 2"),
          sprintf("Rekonstruksi Nilai t: t = qt(1 - %.4f/2, df=%d) = %.4f.", p_val, df_val, abs(t_equiv))
        ),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Langkah 3"),
          sprintf("Cohen's d = %.4f | Faktor J = %.5f | %s = %.4f (SE = %.4f, 95%% CI [%.4f, %.4f]).",
                  d_val, j_val, metric_name, es_val, se_val, ci_lo, ci_hi)
        )
      )
    )
  } else if (itype == "table_2x2") {
    a <- input_list$tab_e1; n1 <- input_list$tab_n1; b <- n1 - a
    c <- input_list$tab_e2; n2 <- input_list$tab_n2; d <- n2 - c
    or_val <- (a * d) / (b * c)
    lor_val <- log(or_val)
    v_lor <- 1/a + 1/b + 1/c + 1/d
    d_chinn <- lor_val * (sqrt(3) / pi)
    vd_chinn <- v_lor * (3 / (pi^2))
    rr_val <- (a / n1) / (c / n2)
    rd_val <- (a / n1) - (c / n2)
    
    tagList(
      div(
        class = "kawe-math-card",
        div(class = "kawe-formula-title", icon("square-root-alt"), " Metode: Tabel Kontingensi 2x2 (Data Kategorikal Biner / Proporsi)"),
        div(class = "kawe-formula-sub", "Rujukan Baku: Chinn (2000); Hasselblad & Hedges (1995); Woolf (1955)."),
        p("Mengonversi rasio odds (Odds Ratio) dari luaran biner ke dalam skala standardized mean difference menggunakan metode transformasi logistik kontinu Chinn (2000)."),
        
        div(class = "fw-bold text-primary mt-3 mb-1", "1. Landasan Formulasi Matematis:"),
        div(class = "kawe-eq-box",
          HTML("$$\\text{Odds Ratio: } OR = \\frac{a \\cdot d}{b \\cdot c}, \\quad \\ln(OR) = \\ln\\left(\\frac{a \\cdot d}{b \\cdot c}\\right)$$"),
          HTML("$$\\text{Varians Log OR: } v_{\\ln(OR)} = \\frac{1}{a} + \\frac{1}{b} + \\frac{1}{c} + \\frac{1}{d}$$"),
          HTML("$$\\text{Transformasi Chinn ke d: } d = \\ln(OR) \\times \\frac{\\sqrt{3}}{\\pi} \\approx \\ln(OR) \\times 0.5513$$"),
          HTML("$$\\text{Varians d: } v_d = v_{\\ln(OR)} \\times \\frac{3}{\\pi^2} \\approx v_{\\ln(OR)} \\times 0.3040$$"),
          HTML("$$\\text{Relative Risk: } RR = \\frac{a / n_1}{c / n_2}, \\quad \\text{Risk Difference: } RD = \\frac{a}{n_1} - \\frac{c}{n_2}$$")
        ),
        
        div(class = "fw-bold text-primary mt-3 mb-1", "2. Langkah Perhitungan Terbuka & Substitusi Angka Riil:"),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Langkah 1"),
          sprintf("Matriks 2x2: Intervensi [Event a=%d, Non-event b=%d, Total n1=%d] | Kontrol [Event c=%d, Non-event d=%d, Total n2=%d].",
                  a, b, n1, c, d, n2)
        ),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Langkah 2"),
          div(class = "kawe-eq-box",
            HTML(sprintf("$$OR = \\frac{(%d)(%d)}{(%d)(%d)} = \\frac{%d}{%d} = %.4f, \\quad \\ln(OR) = %.4f$$",
                         a, d, b, c, a*d, b*c, or_val, lor_val)),
            HTML(sprintf("$$v_{\\ln(OR)} = \\frac{1}{%d} + \\frac{1}{%d} + \\frac{1}{%d} + \\frac{1}{%d} = %.5f$$",
                         a, b, c, d, v_lor))
          )
        ),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Langkah 3"),
          div(class = "kawe-eq-box",
            HTML(sprintf("$$d = %.4f \\times \\frac{\\sqrt{3}}{\\pi} = %.4f \\times 0.5513 = %.4f$$",
                         lor_val, lor_val, d_chinn)),
            HTML(sprintf("$$v_d = %.5f \\times \\frac{3}{\\pi^2} = %.5f$$", v_lor, vd_chinn))
          ),
          sprintf("Estimasi Titik %s = %.4f (SE = %.4f, 95%% CI [%.4f, %.4f]) | RR = %.3f | RD = %.3f (%.1f%%).",
                  metric_name, es_val, se_val, ci_lo, ci_hi, rr_val, rd_val, rd_val * 100)
        )
      )
    )
  } else if (itype == "correlation") {
    r_val <- input_list$corr_r; n_val <- input_list$corr_n
    d_val <- (2 * r_val) / sqrt(1 - r_val^2)
    j_val <- 1 - (3 / (4 * n_val - 9))
    g_val <- d_val * j_val
    z_val <- 0.5 * log((1 + r_val) / (1 - r_val))
    vz_val <- 1 / (n_val - 3)
    
    tagList(
      div(
        class = "kawe-math-card",
        div(class = "kawe-formula-title", icon("square-root-alt"), " Metode: Koefisien Korelasi (Pearson r / Point-Biserial)"),
        div(class = "kawe-formula-sub", "Rujukan Baku: Borenstein et al. (2009); Jacobs & Viechtbauer (2017); Fisher (1921)."),
        p("Mengonversi koefisien korelasi ke standardized mean difference d dan melaporkan transformasi Fisher's z."),
        
        div(class = "fw-bold text-primary mt-3 mb-1", "1. Landasan Formulasi Matematis:"),
        div(class = "kawe-eq-box",
          HTML("$$d = \\frac{2r}{\\sqrt{1 - r^2}}, \\quad J = 1 - \\frac{3}{4N - 9}, \\quad g = J \\times d$$"),
          HTML("$$\\text{Fisher's } z = \\frac{1}{2}\\ln\\left(\\frac{1 + r}{1 - r}\\right) = \\text{arctanh}(r), \\quad v_z = \\frac{1}{N - 3}, \\quad SE_z = \\frac{1}{\\sqrt{N - 3}}$$")
        ),
        
        div(class = "fw-bold text-primary mt-3 mb-1", "2. Langkah Perhitungan Terbuka & Substitusi Angka Riil:"),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Langkah 1"),
          sprintf("Koefisien Korelasi r = %.3f | Ukuran Sampel Total N = %d.", r_val, n_val)
        ),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Langkah 2"),
          div(class = "kawe-eq-box",
            HTML(sprintf("$$d = \\frac{2(%.3f)}{\\sqrt{1 - %.3f^2}} = \\frac{%.3f}{\\sqrt{%.4f}} = %.4f$$",
                         r_val, r_val, 2*r_val, 1 - r_val^2, d_val))
          )
        ),
        div(
          class = "kawe-formula-step",
          tags$span(class = "kawe-step-num", "Langkah 3"),
          sprintf("Faktor J = %.5f -> %s = %.4f (SE = %.4f, 95%% CI [%.4f, %.4f]) | Fisher's z = %.4f (SE_z = %.4f).",
                  j_val, metric_name, es_val, se_val, ci_lo, ci_hi, z_val, sqrt(vz_val))
        )
      )
    )
  }
}

# UI Definition
ui <- page_navbar(
  id = "main_navbar",
  fillable = FALSE,
  title = tags$div(
    class = "kawe-brand-horizontal"
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
        @import url('https://fonts.googleapis.com/css2?family=Cinzel:wght@500;600;700&family=Cormorant+Garamond:ital,wght@0,400;0,500;0,600;0,700;1,400;1,600&family=EB+Garamond:ital,wght@0,400;0,500;0,600;1,400&family=Inter:wght@300;400;500;600;700&family=JetBrains+Mono:wght@400;500&display=swap');

        :root {
          /* Palet Warna KAWE Academic Press */
          --kawe-maroon: #650915;
          --kawe-maroon-deep: #4A050D;
          --kawe-maroon-light: #7E0D1B;
          --kawe-gold: #C5A059;
          --kawe-gold-light: #E5D3B3;
          --kawe-gold-trans: rgba(197, 160, 89, 0.35);
          --kawe-bg-white: #FFFFFF;
          --kawe-bg-paper: #FAF8F5;
          --kawe-text-main: #231618;
          --kawe-text-muted: #5A4B4C;

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
          line-height: 1.65;
          scrollbar-width: thin;
          scrollbar-color: var(--kawe-gold) var(--kawe-bg-paper);
        }

        /* Headings */
        h1, h2, h3, .brand-title {
          font-family: var(--font-logo) !important;
          color: var(--kawe-maroon) !important;
          text-transform: uppercase;
          letter-spacing: 0.18em;
          font-weight: 600;
        }

        h4, h5, h6 {
          font-family: var(--font-serif) !important;
          color: var(--kawe-maroon) !important;
          letter-spacing: 0.06em;
          font-weight: 600;
        }

        .brand-subtitle {
          font-family: var(--font-serif) !important;
          color: var(--kawe-gold) !important;
          text-transform: uppercase;
          letter-spacing: 0.15em;
          font-size: 0.88rem;
          font-weight: 600;
        }

        /* Dividers */
        hr, .gold-divider {
          border: 0 !important;
          border-top: 1px solid var(--kawe-gold) !important;
          opacity: 0.45;
          margin: 1.25rem 0;
        }

        /* Navbar Layout & Styling (Full Flat Maroon Header) */
        .navbar {
          position: sticky !important;
          top: 0 !important;
          z-index: 1050 !important;
          background-color: var(--kawe-maroon) !important;
          border-bottom: 2px solid var(--kawe-gold) !important;
          box-shadow: 0 4px 14px rgba(43, 24, 26, 0.25);
          padding: 0.35rem 1.15rem !important;
        }

        /* Navbar Brand Horizontal (Icon Left, Divider, Text Centered Right) */
        .navbar-brand {
          display: flex !important;
          align-items: center !important;
          padding: 0.15rem 0 !important;
          margin-right: 1.25rem !important;
        }

        .kawe-brand-horizontal {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          gap: 0.85rem; /* Jarak proporsional antara ikon, garis emas, dan grup teks */
          padding: 0.10rem 0.25rem;
          background-color: transparent;
        }

        .kawe-icon {
          height: 38px; /* Ukuran proporsional di tengah vertikal header navbar */
          width: auto;
          object-fit: contain;
          display: block;
          margin: 0 auto;
          filter: drop-shadow(0 1px 3px rgba(0, 0, 0, 0.40));
        }

        .kawe-divider {
          height: 30px;
          width: 1px;
          background-color: #C5A059; /* Antique Gold */
          opacity: 0.70;
          align-self: center;
        }

        .kawe-text-group {
          display: flex;
          flex-direction: column;
          justify-content: center;
          align-items: center;
          text-align: center;
        }

        .kawe-title {
          font-family: 'Cinzel', 'Cormorant Garamond', serif;
          font-size: 1.28rem;
          font-weight: 600;
          color: #FAF8F5 !important; /* Crisp Ivory White pada Maroon */
          letter-spacing: 0.36em;
          padding-left: 0.36em; /* Kompensasi trailing letter-spacing agar KAWE tepat di tengah */
          line-height: 1.0;
          margin-bottom: 0.12rem;
          text-align: center;
          display: block;
          width: 100%;
        }

        .kawe-subtitle {
          font-family: 'Cormorant Garamond', serif;
          font-size: 0.58rem;
          font-weight: 600;
          color: #E5D3B3 !important; /* Soft Antique Gold */
          letter-spacing: 0.12em;
          padding-left: 0.12em;
          line-height: 1.1;
          text-transform: uppercase;
          white-space: nowrap;
          text-align: center;
          display: block;
          width: 100%;
        }

        /* Hero Card Brand Override (Light Background) */
        .kawe-hero-card .kawe-brand-horizontal {
          display: flex;
          justify-content: center;
          align-items: center;
          width: fit-content;
          margin: 0 auto;
        }

        .kawe-hero-card .kawe-title {
          color: var(--kawe-maroon) !important;
        }

        .kawe-hero-card .kawe-subtitle {
          color: var(--kawe-text-main) !important;
        }

        .navbar-nav {
          flex-wrap: wrap !important;
          overflow: visible !important;
        }

        .navbar .nav-link, .navbar-nav > li > a {
          font-family: var(--font-sans);
          font-size: 0.82rem !important;
          font-weight: 500;
          color: #EAE0D8 !important; /* Soft ivory text pada latar marun */
          padding: 0.50rem 0.75rem !important;
          border-bottom: 2px solid transparent;
          white-space: nowrap !important;
          transition: all 0.2s ease;
        }

        .navbar .nav-link:hover, .navbar-nav > li > a:hover {
          color: #FFFFFF !important;
          border-bottom: 2px solid var(--kawe-gold) !important;
          background-color: rgba(255, 255, 255, 0.08) !important;
        }

        .navbar .nav-link.active, .navbar-nav > li.active > a {
          color: #FFFFFF !important;
          font-weight: 600;
          border-bottom: 2px solid var(--kawe-gold) !important;
          background-color: rgba(0, 0, 0, 0.20) !important;
          border-radius: 3px 3px 0 0;
        }

        /* Navbar Dropdown Menu (Tab Meta-Analisis ke bawah) */
        .navbar .dropdown-menu {
          background-color: var(--kawe-maroon) !important;
          border: 1px solid var(--kawe-gold) !important;
          border-radius: 4px !important;
          box-shadow: 0 8px 24px rgba(35, 22, 24, 0.35) !important;
          padding: 0.35rem 0 !important;
          margin-top: 0.25rem !important;
        }

        .navbar .dropdown-menu .dropdown-item {
          font-family: var(--font-sans) !important;
          font-size: 0.82rem !important;
          font-weight: 500 !important;
          color: #EAE0D8 !important;
          padding: 0.55rem 1.15rem !important;
          display: flex !important;
          align-items: center !important;
          gap: 0.55rem !important;
          transition: all 0.18s ease;
        }

        .navbar .dropdown-menu .dropdown-item:hover,
        .navbar .dropdown-menu .dropdown-item:focus {
          background-color: var(--kawe-maroon-deep) !important;
          color: #FFFFFF !important;
          border-left: 3px solid var(--kawe-gold) !important;
        }

        .navbar .dropdown-menu .dropdown-item.active {
          background-color: rgba(197, 160, 89, 0.22) !important;
          color: #FFFFFF !important;
          font-weight: 600 !important;
          border-left: 3px solid var(--kawe-gold) !important;
        }

        /* Sub-tabs inside Cards (navset_card_tab) */
        .card-header .nav-link, .nav-tabs .nav-link, .nav-pills .nav-link {
          font-family: var(--font-sans) !important;
          font-size: 0.82rem !important;
          font-weight: 500;
          color: var(--kawe-text-muted) !important;
          padding: 0.45rem 0.85rem !important;
          border-bottom: 2px solid transparent !important;
          transition: all 0.2s ease;
        }

        .card-header .nav-link:hover, .nav-tabs .nav-link:hover {
          color: var(--kawe-maroon) !important;
          background-color: rgba(101, 9, 21, 0.04) !important;
        }

        .card-header .nav-link.active, .nav-tabs .nav-link.active, .nav-pills .nav-link.active {
          color: var(--kawe-maroon) !important;
          font-weight: 600 !important;
          border-bottom: 2px solid var(--kawe-maroon) !important;
          background-color: rgba(101, 9, 21, 0.07) !important;
          border-radius: 4px 4px 0 0;
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
          letter-spacing: 0.04em;
          padding: 0.65rem 1rem !important;
        }

        /* Value Boxes Academic Press Theming */
        .bslib-value-box, .value-box {
          border: 1px solid var(--kawe-gold-trans) !important;
          border-radius: 4px !important;
          box-shadow: none !important;
        }

        .bslib-value-box.bg-primary {
          background-color: var(--kawe-maroon) !important;
          color: #FAF8F5 !important;
        }

        .bslib-value-box.bg-primary .value-box-title {
          color: #E5D3B3 !important;
          font-family: var(--font-serif) !important;
          letter-spacing: 0.08em;
          text-transform: uppercase;
          font-size: 0.72rem;
        }

        .bslib-value-box.bg-primary .value-box-value {
          color: #FAF8F5 !important;
          font-family: var(--font-logo) !important;
          font-weight: 600;
        }

        .bslib-value-box.bg-secondary, .bslib-value-box.bg-info, .bslib-value-box.bg-warning, .bslib-value-box.bg-success {
          background-color: #FFFFFF !important;
          border: 1px solid var(--kawe-gold-trans) !important;
          color: var(--kawe-maroon) !important;
        }

        .bslib-value-box.bg-secondary .value-box-title,
        .bslib-value-box.bg-info .value-box-title,
        .bslib-value-box.bg-warning .value-box-title,
        .bslib-value-box.bg-success .value-box-title {
          color: var(--kawe-gold) !important;
          font-family: var(--font-serif) !important;
          letter-spacing: 0.08em;
          text-transform: uppercase;
          font-size: 0.72rem;
          font-weight: 600;
        }

        .bslib-value-box.bg-secondary .value-box-value,
        .bslib-value-box.bg-info .value-box-value,
        .bslib-value-box.bg-warning .value-box-value,
        .bslib-value-box.bg-success .value-box-value {
          color: var(--kawe-maroon) !important;
          font-family: var(--font-logo) !important;
          font-weight: 600;
        }

        .value-box-showcase i {
          color: rgba(197, 160, 89, 0.65) !important;
        }

        /* Form Controls */
        .form-control, .form-select {
          border: 1px solid #D6C2A0 !important;
          border-radius: 4px;
          font-size: 0.88rem;
          color: var(--kawe-text-main);
        }

        .form-control:focus, .form-select:focus {
          border-color: var(--kawe-maroon) !important;
          box-shadow: 0 0 0 0.2rem rgba(101, 9, 21, 0.15) !important;
        }

        /* Buttons */
        .btn-primary {
          background-color: var(--kawe-maroon) !important;
          border: 1px solid var(--kawe-gold) !important;
          color: #FFFFFF !important;
          font-weight: 500;
          letter-spacing: 0.03em;
          border-radius: 4px;
          transition: all 0.2s ease;
        }

        .btn-primary:hover, .btn-primary:focus {
          background-color: var(--kawe-maroon-deep) !important;
          border-color: #FFFFFF !important;
          box-shadow: 0 0 0 0.15rem rgba(197, 160, 89, 0.35);
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

        .btn-outline-secondary {
          color: var(--kawe-text-muted) !important;
          border-color: var(--kawe-gold) !important;
          background-color: transparent !important;
        }

        .btn-outline-secondary:hover {
          background-color: var(--kawe-gold) !important;
          color: #FFFFFF !important;
        }

        /* Tables & DataTables */
        table.dataTable thead th {
          background-color: var(--kawe-maroon) !important;
          color: #FFFFFF !important;
          font-family: var(--font-serif) !important;
          letter-spacing: 0.04em;
          border-bottom: 2px solid var(--kawe-gold) !important;
          font-size: 0.86rem;
        }

        table.dataTable tbody tr:hover {
          background-color: #F8F4EE !important;
        }

        /* Hero & Pillar Cards */
        .kawe-hero-card {
          background: linear-gradient(180deg, #FFFFFF 0%, #FAF8F5 100%);
          border: 1px solid var(--kawe-gold-trans);
          border-radius: 4px;
          padding: 2.2rem 1.8rem;
          text-align: center;
        }

        .kawe-pillar-card {
          background-color: #FFFFFF;
          border: 1px solid var(--kawe-gold-trans);
          border-top: 3px solid var(--kawe-maroon);
          border-radius: 4px;
          padding: 1.35rem;
          height: 100%;
          transition: all 0.25s ease;
        }

        .kawe-pillar-card:hover {
          border-top-color: var(--kawe-gold);
          box-shadow: 0 3px 12px rgba(101, 9, 21, 0.08);
        }

        .active-data-pill {
          background-color: #4A050D;
          border: 1px solid #C5A059;
          color: #FAF8F5;
          padding: 0.25rem 0.70rem;
          border-radius: 3px;
          font-size: 0.72rem;
          letter-spacing: 0.04em;
          display: inline-flex;
          align-items: center;
          gap: 0.4rem;
        }

        /* Global Active Dataset & Model Status Bar */
        .kawe-global-bar {
          background-color: #FAF8F5;
          border-bottom: 1px solid var(--kawe-gold-trans);
          padding: 0.40rem 1.25rem;
          font-size: 0.78rem;
          display: flex;
          align-items: center;
          justify-content: space-between;
          flex-wrap: wrap;
          gap: 0.5rem;
        }

        .kawe-clinical-pill {
          background-color: #FFFFFF;
          border: 1px solid var(--kawe-gold-trans);
          border-left: 3px solid var(--kawe-maroon);
          padding: 0.45rem 0.75rem;
          border-radius: 4px;
          font-size: 0.80rem;
        }

        .kawe-step-footer {
          background-color: #FAF8F5;
          border: 1px solid var(--kawe-gold-trans);
          border-radius: 4px;
          padding: 0.65rem 1rem;
          margin-top: 0.85rem;
          display: flex;
          justify-content: space-between;
          align-items: center;
          flex-wrap: wrap;
          gap: 0.5rem;
        }

        /* MathJax & Formula Cards */
        .kawe-math-card {
          background-color: #FFFFFF;
          border: 1px solid var(--kawe-gold-trans);
          border-left: 4px solid var(--kawe-maroon);
          border-radius: 4px;
          padding: 1.15rem 1.35rem;
          margin-bottom: 1.15rem;
          box-shadow: 0 1px 4px rgba(0,0,0,0.03);
        }

        .kawe-formula-step {
          background-color: #FAF8F5;
          border: 1px solid rgba(197, 160, 89, 0.25);
          border-radius: 4px;
          padding: 0.95rem 1.15rem;
          margin-bottom: 0.85rem;
        }

        .kawe-formula-title {
          font-family: var(--font-serif);
          color: var(--kawe-maroon);
          font-weight: 700;
          font-size: 1.05rem;
          margin-bottom: 0.35rem;
          letter-spacing: 0.04em;
        }

        .kawe-formula-sub {
          color: var(--kawe-text-muted);
          font-size: 0.84rem;
          margin-bottom: 0.65rem;
        }

        .kawe-eq-box {
          background-color: #FFFFFF;
          border-radius: 4px;
          padding: 0.75rem 1rem;
          margin: 0.5rem 0;
          border: 1px dashed var(--kawe-gold-trans);
          font-size: 0.96rem;
          overflow-x: auto;
        }

        .kawe-step-num {
          display: inline-block;
          background-color: var(--kawe-maroon);
          color: #FAF8F5;
          font-size: 0.72rem;
          font-weight: 600;
          padding: 0.15rem 0.55rem;
          border-radius: 3px;
          margin-right: 0.45rem;
        }
      ")),
      withMathJax(),
      tags$script(type = "text/x-mathjax-config", HTML("
        MathJax.Hub.Config({
          tex2jax: {
            inlineMath: [['$','$'], ['\\\\(','\\\\)']],
            displayMath: [['$$','$$'], ['\\\\[','\\\\]']],
            processEscapes: true
          }
        });
      ")),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('copy_to_clipboard', function(message) {
          if (navigator.clipboard && message.text) {
            navigator.clipboard.writeText(message.text);
          }
        });
        if ('scrollRestoration' in history) {
          history.scrollRestoration = 'manual';
        }
        function keepScrollAtTop() {
          window.scrollTo(0, 0);
          document.documentElement.scrollTop = 0;
          document.body.scrollTop = 0;
        }
        window.addEventListener('beforeunload', keepScrollAtTop);
        document.addEventListener('DOMContentLoaded', keepScrollAtTop);
        $(document).ready(keepScrollAtTop);
        $(document).on('shiny:connected', keepScrollAtTop);
        $(document).on('shown.bs.tab', function(e) {
          keepScrollAtTop();
        });
        $(document).on('shiny:value', function(event) {
          if (window.MathJax) {
            MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
          }
        });
      "))
    ),
    uiOutput("global_status_banner")
  ),
  
  # Tab 1: Research Hub & Academic Portal
  nav_panel(
    value = "tab_hub",
    icon = icon("university"),
    title = "Home",
    withMathJax(),
    
    # Profil
    div(
      class = "kawe-hero-card w-100 my-3",
      style = "width: 100%; max-width: 100%;",
      div(
        class = "row align-items-center w-100 m-0", # Sebaris, sejajar tengah, & lebar penuh
        
        # Bahagian Kiri (Lebar 3 dari 12 kolom = 25% agar ruang kanan maksimal)
        div(
          class = "col-md-3 kawe-brand-horizontal mb-3 mb-md-0",
          style = "gap: 1.5rem; justify-content: center;",
          img(
            src = "KW.png",
            alt = "KAWE Pic",
            style = "max-height: 420px; width: 100%; object-fit: contain; filter: drop-shadow(0 2px 8px rgba(101, 9, 21, 0.12));"
          )
        ),
        
        # Bahagian Kanan (Lebar 9 dari 12 kolom = 75%)
        div(
          class = "col-md-9",
          
          # Judul & Sub-judul di sebelah kanan (lebar penuh rata kiri)
          div(
            class = "kawe-text-group w-100 mb-3",
            style = "display: flex; flex-direction: column; align-items: flex-start;",
            span(class = "kawe-title", style = "font-size: 1.8rem;", "KAWE : QUANTITATIVE AND PSYCHOMETRICS RESEARCH")
          ),
          
          # Garis emas merentang 100% penuh
          div(class = "gold-divider w-100 my-3"),
          
          # Paragraf rata kanan-kiri (justify) & tanpa batas max-width
          p(
            class = "lead text-dark w-100 mb-0",
            style = "width: 100%; max-width: none; font-size: 1.02rem; line-height: 1.75; text-align: justify;",
            "The KAWE emblem unifies three foundational pillars of our discipline: the classical Greek letter Psi representing psychology, the Gaussian normal distribution curve symbolizing statistical inference, and the logistic S-curve of Item Response Theory (IRT) reflecting modern psychometric precision. Rendered in Deep Maroon and Antique Gold, the dual-toned geometry embodies our core mission—bridging observable behavioral data (manifest variables) with underlying human attributes (latent traits) through rigorous, transparent, and gold-standard quantitative methodology.",
          )
        )
      )
    ),
    
    # Hero Section
    div(
      class = "kawe-hero-card my-3",
      div(
        class = "text-uppercase fw-semibold",
        style = "letter-spacing: 0.18em; font-size: 0.85rem; color: #231618;",
        "Meta Analisis"
      ),
      div(class = "gold-divider w-50 mx-auto my-3"),
      div(
        class = "mt-4 d-flex justify-content-center flex-wrap gap-2",
        actionButton("hero_btn_calc", "Effect Size", class = "btn-primary px-3 py-2", icon = icon("calculator")),
        actionButton("hero_btn_data", "Model Meta Analisis", class = "btn-outline-primary px-3 py-2", icon = icon("chart-pie")),
        actionButton("hero_btn_subgroup", "Forest Plot", class = "btn-outline-secondary px-3 py-2", icon = icon("tree"))
      )
    ),
    # Academic Press Footer
    div(
      class = "text-center py-4 my-2 border-top",
      style = "border-color: #E5D3B3 !important;",
      div(
        class = "brand-title text-muted mb-1",
        style = "font-size: 0.90rem; letter-spacing: 0.18em;",
        "KAWE | QUANTITATIVE & PSYCHOMETRIC RESEARCH"
      ),
      div(
        class = "small text-muted",
        "Aditya Kusumawijaya · Yogyakarta, Indonesia · Versi 0.1"
      )
    )
  ),
  
  # Dropdown Menu: Meta-Analisis
  nav_menu(
    title = "Meta-Analisis",
    icon = icon("chart-pie"),
    
    # Tab 2: Kalkulator Effect Size
    nav_panel(
      value = "tab_calc",
      icon = icon("calculator"),
      title = "Effect Size",
      layout_columns(
        col_widths = c(6, 6),
        
        # Left Column: Input Form (Structured 2-card layout)
        div(
          card(
            class = "mb-3",
            card_header(
              span(icon("calculator"), " HASIL EKSTRAKSI DATA STUDI PRIMER")
            ),
            card_body(
              selectInput(
                "input_type",
                "Format Pelaporan Data Hasil Studi Primer:",
                choices = c(
                  "Perubahan Skor Prates-Pascates (Mean Difference / Gain Score)" = "mean_gain",
                  "Skor Pascates Dua Kelompok (Between-Group Means & SDs)" = "mean_sd",
                  "Rata-Rata & Standar Error (Means & SE)" = "mean_se",
                  "Statistik Uji-t Dua Kelompok Independen (Independent t-test)" = "t_test",
                  "Signifikansi Nilai p Dua Kelompok (Two-tailed p-value & n)" = "p_val",
                  "Statistik ANOVA Satu Arah Dua Kelompok (One-Way F-test)" = "f_test",
                  "Tabel Kontingensi 2×2 (Proporsi Biner / Kejadian Dikotomi)" = "table_2x2",
                  "Koefisien Korelasi Bivariat (Pearson's r)" = "correlation"
                ),
                selected = "mean_gain"
              ),
              
              # Sub-panel for esc_mean_gain
              conditionalPanel(
                condition = "input.input_type == 'mean_gain'",
                layout_columns(
                  col_widths = c(6, 6),
                  radioButtons(
                    "gain_sub_type",
                    "Format Pelaporan Data Pengukuran:",
                    choices = c(
                      "Prates & Pascates Lengkap (Means, SDs, n)" = "pre_post_full",
                      "Selisih Perubahan Langsung (Mean Gain / Change Score)" = "gain_direct"
                    ),
                    selected = "pre_post_full"
                  ),
                  selectInput(
                    "gain_outcome_dir",
                    "Arah Efektivitas Skala Pengukuran:",
                    choices = c(
                      "Skor Pascates Menurun = Pemulihan (misal: Depresi, Kecemasan)" = "decrease_better",
                      "Skor Pascates Meningkat = Pemulihan (misal: Resiliensi, Efikasi)" = "increase_better"
                    )
                  )
                ),
                
                conditionalPanel(
                  condition = "input.gain_sub_type == 'pre_post_full'",
                  h6("Kelompok Eksperimen / Intervensi (Treatment Group):", class = "text-primary fw-bold mt-2"),
                  layout_columns(
                    col_widths = c(4),
                    numericInput("pre1_n", "Ukuran Sampel Eksperimen (n1):", value = 40, min = 2, step = 1)
                  ),
                  layout_columns(
                    col_widths = c(6, 6),
                    numericInput("pre1_m", "Rata-Rata Prates (Pre Mean):", value = 28.4, step = 0.1),
                    numericInput("pre1_sd", "Standar Deviasi Prates (Pre SD):", value = 5.2, min = 0.01, step = 0.1)
                  ),
                  layout_columns(
                    col_widths = c(6, 6),
                    numericInput("post1_m", "Rata-Rata Pascates (Post Mean):", value = 16.2, step = 0.1),
                    numericInput("post1_sd", "Standar Deviasi Pascates (Post SD):", value = 4.8, min = 0.01, step = 0.1)
                  ),
                  h6("Kelompok Pembanding / Kontrol (Control Group):", class = "text-primary fw-bold mt-2"),
                  layout_columns(
                    col_widths = c(6),
                    numericInput("pre2_n", "Ukuran Sampel Kontrol (n2):", value = 40, min = 2, step = 1)
                  ),
                  layout_columns(
                    col_widths = c(6, 6),
                    numericInput("pre2_m", "Rata-Rata Prates (Pre Mean):", value = 27.9, step = 0.1),
                    numericInput("pre2_sd", "Standar Deviasi Prates (Pre SD):", value = 5.0, min = 0.01, step = 0.1)
                  ),
                  layout_columns(
                    col_widths = c(6, 6),
                    numericInput("post2_m", "Rata-Rata Pascates (Post Mean):", value = 24.1, step = 0.1),
                    numericInput("post2_sd", "Standar Deviasi Pascates (Post SD):", value = 5.1, min = 0.01, step = 0.1)
                  )
                ),
                
                conditionalPanel(
                  condition = "input.gain_sub_type == 'gain_direct'",
                  h6("Kelompok Eksperimen / Intervensi (Treatment Group):", class = "text-primary fw-bold mt-2"),
                  layout_columns(
                    col_widths = c(4, 4, 4),
                    numericInput("gain1_m", "Rata-Rata Selisih (Gain Mean):", value = 12.2, step = 0.1),
                    numericInput("gain1_sd", "Standar Deviasi Selisih (Gain SD):", value = 4.5, min = 0.01, step = 0.1),
                    numericInput("gain1_n", "Ukuran Sampel (n1):", value = 40, min = 2, step = 1)
                  ),
                  h6("Kelompok Pembanding / Kontrol (Control Group):", class = "text-secondary fw-bold mt-2"),
                  layout_columns(
                    col_widths = c(4, 4, 4),
                    numericInput("gain2_m", "Rata-Rata Selisih (Gain Mean):", value = 3.8, step = 0.1),
                    numericInput("gain2_sd", "Standar Deviasi Selisih (Gain SD):", value = 4.2, min = 0.01, step = 0.1),
                    numericInput("gain2_n", "Ukuran Sampel (n2):", value = 40, min = 2, step = 1)
                  )
                ),
                
                layout_columns(
                  col_widths = c(6),
                  numericInput("gain_r", "Korelasi Prates-Pascates (r):", value = 0.50, min = -0.99, max = 0.99, step = 0.05)
                ),
                
                layout_columns(
                  col_widths = c(6),
                  div(class = "small text-muted pt-9", "Asumsi Rosenthal (1991) r = 0.50 bila korelasi prates-pascates tidak dilaporkan dalam artikel primer.")
                )
              ),
              
              # Means & SDs
              conditionalPanel(
                condition = "input.input_type == 'mean_sd'",
                selectInput(
                  "sd_outcome_dir",
                  "Arah Efektivitas Skala Pengukuran:",
                  choices = c(
                    "Skor Pascates Menurun = Pemulihan (misal: Depresi, Kecemasan)" = "decrease_better",
                    "Skor Pascates Meningkat = Pemulihan (misal: Resiliensi, Kesejahteraan)" = "increase_better"
                  ),
                  selected = "decrease_better"
                ),
                h6("Kelompok Eksperimen / Intervensi (Treatment Group):", class = "text-primary fw-bold"),
                layout_columns(
                  col_widths = c(4, 4, 4),
                  numericInput("grp1m_sd", "Rata-Rata (Mean):", value = 14.5, step = 0.1),
                  numericInput("grp1sd_sd", "Standar Deviasi (SD):", value = 3.8, min = 0.01, step = 0.1),
                  numericInput("grp1n_sd", "Ukuran Sampel (n1):", value = 50, min = 2, step = 1)
                ),
                h6("Kelompok Pembanding / Kontrol (Control Group):", class = "text-secondary fw-bold mt-2"),
                layout_columns(
                  col_widths = c(4, 4, 4),
                  numericInput("grp2m_sd", "Rata-Rata (Mean):", value = 18.2, step = 0.1),
                  numericInput("grp2sd_sd", "Standar Deviasi (SD):", value = 4.1, min = 0.01, step = 0.1),
                  numericInput("grp2n_sd", "Ukuran Sampel (n2):", value = 48, min = 2, step = 1)
                )
              ),
              
              # Means & SE
              conditionalPanel(
                condition = "input.input_type == 'mean_se'",
                selectInput(
                  "se_outcome_dir",
                  "Arah Efektivitas Skala Pengukuran:",
                  choices = c(
                    "Skor Pascates Menurun = Pemulihan (misal: Depresi, Kecemasan)" = "decrease_better",
                    "Skor Pascates Meningkat = Pemulihan (misal: Resiliensi, Kesejahteraan)" = "increase_better"
                  ),
                  selected = "decrease_better"
                ),
                h6("Kelompok Eksperimen / Intervensi (Treatment Group):", class = "text-primary fw-bold"),
                layout_columns(
                  col_widths = c(4, 4, 4),
                  numericInput("grp1m_se", "Rata-Rata (Mean):", value = 14.5, step = 0.1),
                  numericInput("grp1se_se", "Standar Error (SE):", value = 0.54, min = 0.001, step = 0.01),
                  numericInput("grp1n_se", "Ukuran Sampel (n1):", value = 50, min = 2, step = 1)
                ),
                h6("Kelompok Pembanding / Kontrol (Control Group):", class = "text-secondary fw-bold mt-2"),
                layout_columns(
                  col_widths = c(4, 4, 4),
                  numericInput("grp2m_se", "Rata-Rata (Mean):", value = 18.2, step = 0.1),
                  numericInput("grp2se_se", "Standar Error (SE):", value = 0.59, min = 0.001, step = 0.01),
                  numericInput("grp2n_se", "Ukuran Sampel (n2):", value = 48, min = 2, step = 1)
                )
              ),
              
              # t-test
              conditionalPanel(
                condition = "input.input_type == 't_test'",
                numericInput("t_val", "Nilai Statistik Uji-t (Independent t):", value = 2.85, step = 0.01),
                layout_columns(
                  col_widths = c(6, 6),
                  numericInput("t_n1", "Ukuran Sampel Intervensi (n1):", value = 45, min = 2, step = 1),
                  numericInput("t_n2", "Ukuran Sampel Kontrol (n2):", value = 45, min = 2, step = 1)
                )
              ),
              
              # p-value
              conditionalPanel(
                condition = "input.input_type == 'p_val'",
                layout_columns(
                  col_widths = c(6, 6),
                  numericInput("p_value", "Nilai Signifikansi p (Two-tailed p):", value = 0.015, min = 0.0001, max = 0.9999, step = 0.005),
                  selectInput("p_direction", "Arah Efek Intervensi:", choices = c("Memihak Kelompok Intervensi" = "pos", "Memihak Kelompok Kontrol" = "neg"))
                ),
                layout_columns(
                  col_widths = c(6, 6),
                  numericInput("p_n1", "Ukuran Sampel Intervensi (n1):", value = 50, min = 2, step = 1),
                  numericInput("p_n2", "Ukuran Sampel Kontrol (n2):", value = 50, min = 2, step = 1)
                )
              ),
              
              # F-test
              conditionalPanel(
                condition = "input.input_type == 'f_test'",
                layout_columns(
                  col_widths = c(6, 6),
                  numericInput("f_val", "Nilai Statistik Uji-F (df1 = 1):", value = 7.42, min = 0.001, step = 0.1),
                  selectInput("f_direction", "Arah Efek Intervensi:", choices = c("Memihak Kelompok Intervensi" = "pos", "Memihak Kelompok Kontrol" = "neg"))
                ),
                layout_columns(
                  col_widths = c(6, 6),
                  numericInput("f_n1", "Ukuran Sampel Intervensi (n1):", value = 50, min = 2, step = 1),
                  numericInput("f_n2", "Ukuran Sampel Kontrol (n2):", value = 50, min = 2, step = 1)
                )
              ),
              
              # 2x2 table
              conditionalPanel(
                condition = "input.input_type == 'table_2x2'",
                h6("Kelompok Eksperimen / Intervensi (Treatment Group):", class = "text-primary fw-bold"),
                layout_columns(
                  col_widths = c(6, 6),
                  numericInput("tab_e1", "Jumlah Kejadian / Respons (Event):", value = 35, min = 0, step = 1),
                  numericInput("tab_n1", "Total Ukuran Sampel (n1):", value = 50, min = 1, step = 1)
                ),
                h6("Kelompok Pembanding / Kontrol (Control Group):", class = "text-secondary fw-bold mt-2"),
                layout_columns(
                  col_widths = c(6, 6),
                  numericInput("tab_e2", "Jumlah Kejadian / Respons (Event):", value = 18, min = 0, step = 1),
                  numericInput("tab_n2", "Total Ukuran Sampel (n2):", value = 50, min = 1, step = 1)
                )
              ),
              
              # Correlation
              conditionalPanel(
                condition = "input.input_type == 'correlation'",
                layout_columns(
                  col_widths = c(6, 6),
                  numericInput("corr_r", "Koefisien Korelasi Pearson (r):", value = 0.35, min = -0.999, max = 0.999, step = 0.01),
                  numericInput("corr_n", "Total Ukuran Sampel (N):", value = 90, min = 4, step = 1)
                )
              )
            )
          ),
          
          card(
            card_header(
              span(icon("tag"), " IDENTIFIKASI STUDI PRIMER & KODE MODERATOR")
            ),
            card_body(
              layout_columns(
                col_widths = c(6, 6),
                textInput("study_name", "Label Studi (Peneliti & Tahun / Author et al.):", value = "Studi Baru et al."),
                numericInput("study_year", "Tahun Publikasi (Publication Year):", value = 2024, min = 1950, max = 2035, step = 1)
              ),
              layout_columns(
                col_widths = c(6, 6),
                selectInput(
                  "study_interv",
                  "Modalitas Intervensi (Intervention):",
                  choices = c(
                    "CBT (Cognitive Behavioral Therapy)" = "CBT",
                    "ACT (Acceptance and Commitment Therapy)" = "ACT",
                    "BA (Behavioral Activation)" = "BA",
                    "iCBT (Internet-based CBT)" = "iCBT",
                    "MBI (Mindfulness-Based Intervention)" = "Mindfulness",
                    "IPT (Interpersonal Psychotherapy)" = "IPT",
                    "Psychodynamic Therapy" = "Psychodynamic",
                    "Lainnya (Other Interventions)" = "Lainnya"
                  )
                ),
                selectInput(
                  "study_format",
                  "Format Pengantaran (Delivery Format):",
                  choices = c(
                    "Individual (Face-to-Face)" = "Individual",
                    "Kelompok (Group Face-to-Face)" = "Group",
                    "Digital Terpandu (Guided Digital / iCBT)" = "Guided Digital",
                    "Mandiri Tanpa Panduan (Unguided Self-Help)" = "Unguided Self-Help"
                  )
                )
              ),
              layout_columns(
                col_widths = c(6, 6),
                selectInput(
                  "study_ctrl",
                  "Kondisi Pembanding / Kontrol (Control Condition):",
                  choices = c(
                    "Daftar Tunggu (Waitlist / WL)" = "Waitlist",
                    "Perawatan Rutin (Treatment as Usual / TAU)" = "TAU",
                    "Plasebo / Kontrol Atensi (Placebo / Attention)" = "Placebo",
                    "Komparator Terapi Aktif (Active Comparator / Control)" = "Active Control"
                  )
                ),
                conditionalPanel(
                  condition = "input.study_ctrl == 'Active Control'",
                  textInput("study_active_comparator", "Spesifikasi Komparator Aktif:", value = "Supportive Counseling", placeholder = "Misal: Supportive Counseling, Relaxation, Psikoedukasi")
                ),
                conditionalPanel(
                  condition = "input.study_ctrl != 'Active Control'",
                  p(class = "small text-muted pt-4 mb-0", "Komparator standar tanpa komponen perlakuan psikologis aktif.")
                )
              ),
              layout_columns(
                col_widths = c(6, 6),
                selectInput(
                  "study_pop",
                  "Karakteristik Populasi Klinis (Population):",
                  choices = c(
                    "Klinis: Gangguan Depresi Mayor (Clinical MDD)" = "Clinical (MDD)",
                    "Klinis: Gangguan Kecemasan (Clinical Anxiety)" = "Clinical (Anxiety)",
                    "Klinis: Gangguan Stres Pascatrauma (Clinical PTSD)" = "Clinical (PTSD / Trauma)",
                    "Subklinis (Subclinical Elevated Symptoms)" = "Subclinical",
                    "Populasi Umum / Komunitas (Community / Non-clinical)" = "Community"
                  )
                ),
                selectInput(
                  "study_age",
                  "Kohort Usia Partisipan (Age Cohort):",
                  choices = c(
                    "Dewasa (Adults, 18–64 tahun)" = "Adults",
                    "Remaja (Adolescents, 12–17 tahun)" = "Adolescents",
                    "Lansia (Older Adults, ≥ 65 tahun)" = "Older Adults",
                    "Anak-anak (Children, < 12 tahun)" = "Children"
                  ),
                  selected = "Adults"
                )
              ),
              layout_columns(
                col_widths = c(4, 4, 4),
                selectInput(
                  "study_therapist",
                  "Kualifikasi Terapis / Fasilitator:",
                  choices = c(
                    "Klinisi Berlisensi (Licensed Clinician)" = "Licensed Clinician",
                    "Terapis Tersupervisi (Supervised Trainee)" = "Supervised Trainee",
                    "Sistem Digital Otomatis (Automated Digital)" = "Automated Digital"
                  )
                ),
                selectInput(
                  "study_rob",
                  "Risiko Bias Metodologis (RoB 2):",
                  choices = c(
                    "Risiko Rendah (Low Risk of Bias)" = "Low Risk",
                    "Kekhawatiran Terbatas (Some Concerns)" = "Some Concerns",
                    "Risiko Tinggi (High Risk of Bias)" = "High Risk"
                  )
                ),
                numericInput("study_dur", "Durasi Intervensi (Minggu):", value = 12, min = 1, step = 1)
              ),
              layout_columns(
                col_widths = c(6, 6),
                numericInput("study_sessions", "Jumlah Sesi Terapi (Total Sessions):", value = 12, min = 1, step = 1),
                radioButtons(
                  "es_metric",
                  "Metrik Standardized Mean Difference (SMD):",
                  choices = c(
                    "Hedges' g (Terkoreksi Bias / Unbiased)" = "g",
                    "Cohen's d (Estimasi Bias Sampel Kecil)" = "d"
                  ),
                  selected = "g",
                  inline = TRUE
                )
              ),
              div(
                class = "d-flex gap-2 mt-2",
                actionButton("btn_calculate", "Hitung Parameter Efek", class = "btn-outline-primary flex-grow-1", icon = icon("calculator")),
                actionButton("btn_append_study", "Simpan ke Matriks Studi", class = "btn-primary flex-grow-1", icon = icon("plus-circle")),
                actionButton("btn_reset_calc_form", "Atur Ulang Formulir", class = "btn-outline-secondary", icon = icon("redo"))
              )
            )
          )
        ),
        
        # Right Column: Output & Realtime History
        div(
          card(
            card_header(
              span(icon("chart-line"), " BESARAN EFEK/EFFECT SIZE")
            ),
            card_body(
              navset_tab(
                id = "calc_results_navset",
                
                # Panel 1: Hasil Estimasi & Ringkasan
                nav_panel(
                  title = span(icon("poll"), " Ringkasan Estimasi"),
                  value = "panel_calc_results",
                  layout_columns(
                    col_widths = c(4, 4, 4),
                    div(
                      class = "kawe-kpi-card text-center p-3 rounded border",
                      style = "background: #fdfcfb; border: 1px solid #d9d2c7 !important; border-radius: 8px;",
                      div(
                        class = "d-flex justify-content-center align-items-center mb-1 gap-2",
                        span(icon("balance-scale"), style = "font-size: 1.15rem; color: #6b1d2f;"),
                        span(class = "fw-bold text-uppercase small", style = "letter-spacing: 0.5px; color: #495057;", "Besaran Efek / Effect Size")
                      ),
                      div(
                        class = "fw-bold",
                        style = "font-size: 1.9rem; line-height: 1.2; color: #6b1d2f; font-family: 'Cinzel', serif, Georgia;",
                        textOutput("vb_calc_es", inline = TRUE)
                      )
                    ),
                    div(
                      class = "kawe-kpi-card text-center p-3 rounded border",
                      style = "background: #fdfcfb; border: 1px solid #d9d2c7 !important; border-radius: 8px;",
                      div(
                        class = "d-flex justify-content-center align-items-center mb-1 gap-2",
                        span(icon("wave-square"), style = "font-size: 1.15rem; color: #8c6d3b;"),
                        span(class = "fw-bold text-uppercase small", style = "letter-spacing: 0.5px; color: #495057;", "Varians Sampling (vi)")
                      ),
                      div(
                        class = "fw-bold",
                        style = "font-size: 1.9rem; line-height: 1.2; color: #6b1d2f; font-family: 'Cinzel', serif, Georgia;",
                        textOutput("vb_calc_var", inline = TRUE)
                      )
                    ),
                    div(
                      class = "kawe-kpi-card text-center p-3 rounded border",
                      style = "background: #fdfcfb; border: 1px solid #d9d2c7 !important; border-radius: 8px;",
                      div(
                        class = "d-flex justify-content-center align-items-center mb-1 gap-2",
                        span(icon("arrows-alt-h"), style = "font-size: 1.15rem; color: #31708f;"),
                        span(class = "fw-bold text-uppercase small", style = "letter-spacing: 0.5px; color: #495057;", "Standar Error (SE)")
                      ),
                      div(
                        class = "fw-bold",
                        style = "font-size: 1.9rem; line-height: 1.2; color: #6b1d2f; font-family: 'Cinzel', serif, Georgia;",
                        textOutput("vb_calc_se", inline = TRUE)
                      )
                    )
                  ),
                  
                  hr(class = "my-2"),
                  
                  h6("Visualisasi Effect Size dengan 95% CI:", class = "fw-bold text-dark mb-1"),
                  plotOutput("calc_visual_preview_plot", height = "135px"),
                  
                  h6("Hasil Effect Size:", class = "fw-bold text-dark mt-2"),
                  tableOutput("calc_summary_table"),
                  
                  # Quick callout for active formula
                  uiOutput("calc_quick_formula_callout"),
                  
                  div(
                    class = "mt-3",
                    actionButton("btn_view_calc_compendium", "Buka Kumpulan Rumus dan Panduan Perhitungan Lengkap →", class = "btn-sm btn-outline-secondary w-100", icon = icon("book-open"))
                  )
                ),
                
                # Panel 2: Kompendium Seluruh Rumus Effect Size
                nav_panel(
                  title = span(icon("book-open"), " Kumpulan Rumus dan Panduan Perhitungan"),
                  value = "panel_formula_catalog",
                  div(
                    class = "pt-2",
                    div(
                      class = "mb-2",
                      actionButton("btn_back_to_calc_results2", "← Kembali ke Ringkasan", class = "btn-sm btn-outline-secondary", icon = icon("arrow-left"))
                    ),
                    accordion(
                      id = "acc_all_formulas",
                      open = FALSE,
                      
                      # 1. Post-Test Means & SDs
                      accordion_panel(
                        title = span(icon("chart-bar"), " 1. Standardized Mean Difference: Rata-rata Pascates & Deviasi Baku (Post-Test Means & SDs)"),
                        value = "acc_post_means",
                        div(
                          p("Desain eksperimen dua kelompok independen (treatment vs control) pascates mengevaluasi perbedaan rata-rata terstandarisasi. Formulasi baku didasarkan pada Cohen (1988) dan Hedges (1981):"),
                          div(class = "kawe-eq-box",
                            HTML("$$\\text{Selisih Rata-rata: } \\Delta\\bar{X} = \\bar{X}_1 - \\bar{X}_2$$"),
                            HTML("$$\\text{Deviasi Baku Gabungan: } s_{\\text{pooled}} = \\sqrt{\\frac{(n_1 - 1)s_1^2 + (n_2 - 1)s_2^2}{n_1 + n_2 - 2}}$$"),
                            HTML("$$\\text{Cohen's } d = \\frac{\\bar{X}_1 - \\bar{X}_2}{s_{\\text{pooled}}}$$"),
                            HTML("$$\\text{Faktor Koreksi Bias Hedges: } J = 1 - \\frac{3}{4(n_1 + n_2) - 9} = 1 - \\frac{3}{4df - 1}$$"),
                            HTML("$$\\text{Hedges' } g = J \\times d$$"),
                            HTML("$$\\text{Varians Sampling: } v_d = \\frac{n_1 + n_2}{n_1 n_2} + \\frac{d^2}{2(n_1 + n_2)}, \\quad v_g = J^2 \\times v_d$$"),
                            HTML("$$\\text{Standar Error: } SE_g = \\sqrt{v_g}, \\quad 95\\% \\text{ CI} = [g - 1.96 \\cdot SE_g, \\; g + 1.96 \\cdot SE_g]$$")
                          ),
                          p(class = "small text-muted mt-2",
                            strong("Asumsi & Konvensi: "),
                            "Diasumsikan varians homogen antar-kelompok (homoscedasticity). Pada ukuran sampel kecil (n < 50), penggunaan Hedges' g wajib menggantikan Cohen's d guna mengoreksi upward positive bias (Hedges, 1981; Cochrane Handbook 2023)."
                          )
                        )
                      ),
                      
                      # 2. Mean Gain Scores
                      accordion_panel(
                        title = span(icon("exchange-alt"), " 2. Mean Gain / Perubahan Pre-Post (Repeated Measures Designs)"),
                        value = "acc_gain",
                        div(
                          p("Pada studi klinis dengan desain pretest-posttest-control group (PPCR), besaran efek dihitung dari selisih perubahan skor (gain) antara kelompok intervensi dan kontrol (Becker, 1988; Morris, 2008):"),
                          div(class = "kawe-eq-box",
                            HTML("$$\\text{Gain Intervensi: } \\text{Gain}_1 = \\bar{X}_{post,1} - \\bar{X}_{pre,1}, \\quad \\text{Gain Kontrol: } \\text{Gain}_2 = \\bar{X}_{post,2} - \\bar{X}_{pre,2}$$"),
                            HTML("$$\\text{Deviasi Baku Perubahan: } s_{\\text{change}, i} = \\sqrt{s_{pre, i}^2 + s_{post, i}^2 - 2 r s_{pre, i} s_{post, i}}$$"),
                            HTML("$$\\text{Deviasi Baku Gabungan: } s_{\\text{pooled, change}} = \\sqrt{\\frac{(n_1 - 1)s_{\\text{change},1}^2 + (n_2 - 1)s_{\\text{change},2}^2}{n_1 + n_2 - 2}}$$"),
                            HTML("$$d_{\\text{gain}} = \\frac{\\text{Gain}_1 - \\text{Gain}_2}{s_{\\text{pooled, change}}}, \\quad g_{\\text{gain}} = J \\times d_{\\text{gain}}$$"),
                            HTML("$$v_d = \\frac{2(1 - r_1)}{n_1} + \\frac{2(1 - r_2)}{n_2} + \\frac{d^2}{2(n_1 + n_2)}, \\quad v_g = J^2 \\times v_d$$")
                          ),
                          p(class = "small text-muted mt-2",
                            strong("Asumsi Korelasi Pre-Post (r): "),
                            "Bila artikel primer tidak melaporkan r pre-post secara eksplisit, konvensi Rosenthal (1991) menetapkan nilai default konservatif r = 0.50 (Morris, 2008)."
                          )
                        )
                      ),
                      
                      # 3. Means & SE
                      accordion_panel(
                        title = span(icon("arrows-alt-h"), " 3. Rekonstruksi dari Mean & Standar Error (SE)"),
                        value = "acc_mean_se",
                        div(
                          p("Jika studi primer hanya melaporkan Standar Error (SE) dan ukuran sampel (n), deviasi baku kelompok (s) direkonstruksi matematis:"),
                          div(class = "kawe-eq-box",
                            HTML("$$s_1 = SE_1 \\times \\sqrt{n_1}, \\quad s_2 = SE_2 \\times \\sqrt{n_2}$$"),
                            HTML("$$s_{\\text{pooled}} = \\sqrt{\\frac{(n_1 - 1)(SE_1^2 \\cdot n_1) + (n_2 - 1)(SE_2^2 \\cdot n_2)}{n_1 + n_2 - 2}}$$"),
                            HTML("$$d = \\frac{\\bar{X}_1 - \\bar{X}_2}{s_{\\text{pooled}}}, \\quad g = J \\times d$$")
                          )
                        )
                      ),
                      
                      # 4. Inferential t, F, p
                      accordion_panel(
                        title = span(icon("balance-scale"), " 4. Konversi dari Statistik Inferensial Parametrik (Uji-t, Uji-F, Nilai-p)"),
                        value = "acc_inferential",
                        div(
                          p("Ketika mean dan deviasi baku tidak dilaporkan, besaran efek direkonstruksi dari statistik uji parametrik (Dunlap et al., 1996; Lipsey & Wilson, 2001):"),
                          div(class = "kawe-eq-box",
                            HTML("$$\\text{Uji-t Independen: } d = t \\times \\sqrt{\\frac{n_1 + n_2}{n_1 n_2}}, \\quad v_d = \\frac{n_1 + n_2}{n_1 n_2} + \\frac{d^2}{2(n_1 + n_2)}$$"),
                            HTML("$$\\text{Uji-F Satu Arah (} df_1=1 \\text{): } |t| = \\sqrt{F}, \\quad d = \\pm \\sqrt{F \\left(\\frac{n_1 + n_2}{n_1 n_2}\\right)}$$"),
                            HTML("$$\\text{Nilai-p Dua Arah: } t = F_{t, df}^{-1}(1 - p/2), \\quad df = n_1 + n_2 - 2$$"),
                            HTML("$$g = J \\times d, \\quad v_g = J^2 \\times v_d$$")
                          ),
                          p(class = "small text-muted mt-2",
                            strong("Arah Efek: "),
                            "Statistik F dan nilai-p tidak memuat informasi tanda matematis (+/-); penentuan arah keunggulan intervensi wajib diverifikasi dari laporan deskriptif naratif studi primer."
                          )
                        )
                      ),
                      
                      # 5. Binary & 2x2 Table
                      accordion_panel(
                        title = span(icon("th-large"), " 5. Luaran Dikotomis / Tabel Kontingensi 2x2 (Odds Ratio ke SMD)"),
                        value = "acc_binary",
                        div(
                          p("Konversi rasio odds (OR) dari luaran biner (misalnya respon klinis, remisi) ke skala standardized mean difference d (Chinn, 2000; Hasselblad & Hedges, 1995):"),
                          div(class = "kawe-eq-box",
                            HTML("$$\\text{Odds Ratio: } OR = \\frac{a \\cdot d}{b \\cdot c}, \\quad \\ln(OR) = \\ln(a) - \\ln(b) - \\ln(c) + \\ln(d)$$"),
                            HTML("$$\\text{Varians Woolf: } v_{\\ln(OR)} = \\frac{1}{a} + \\frac{1}{b} + \\frac{1}{c} + \\frac{1}{d}$$"),
                            HTML("$$\\text{Konversi ke } d: d = \\ln(OR) \\times \\frac{\\sqrt{3}}{\\pi} \\approx 0.5513 \\times \\ln(OR)$$"),
                            HTML("$$\\text{Varians } d: v_d = v_{\\ln(OR)} \\times \\frac{3}{\\pi^2} \\approx 0.3040 \\times v_{\\ln(OR)}, \\quad g = J \\times d$$")
                          ),
                          p(class = "small text-muted mt-2",
                            strong("Landasan Teoretis: "),
                            "Transformasi ini didasarkan pada asumsi variabel laten logistik kontinu yang mendasari dikotomisasi ambang batas luaran klinis (Chinn, 2000)."
                          )
                        )
                      ),
                      
                      # 6. Correlation r
                      accordion_panel(
                        title = span(icon("project-diagram"), " 6. Koefisien Asosiasi & Korelasi (r & Fisher z)"),
                        value = "acc_corr",
                        div(
                          p("Transformasi korelasi Pearson atau point-biserial r ke skala standardized mean difference d (Borenstein et al., 2009; Jacobs & Viechtbauer, 2017):"),
                          div(class = "kawe-eq-box",
                            HTML("$$d = \\frac{2r}{\\sqrt{1 - r^2}}, \\quad v_d = \\frac{4 v_r}{(1 - r^2)^3}, \\quad v_r = \\frac{(1 - r^2)^2}{N - 1}$$"),
                            HTML("$$\\text{Transformasi Fisher's } z: z = \\frac{1}{2}\\ln\\left(\\frac{1 + r}{1 - r}\\right) = \\text{arctanh}(r), \\quad v_z = \\frac{1}{N - 3}$$"),
                            HTML("$$g = J \\times d, \\quad J = 1 - \\frac{3}{4N - 9}$$")
                          )
                        )
                      ),
                      
                      # 7. J-Factor & Varians
                      accordion_panel(
                        title = span(icon("sliders-h"), " 7. Faktor Koreksi Bias Sampel Kecil Hedges (J) & Formulasi Varians Asimtotik"),
                        value = "acc_j_var",
                        div(
                          p("Formulasi eksak dan aproksimasi koreksi bias sampel kecil Hedges (1981):"),
                          div(class = "kawe-eq-box",
                            HTML("$$\\text{Faktor Koreksi Eksak (Gamma): } J_{\\text{exact}} = \\frac{\\Gamma(df/2)}{\\sqrt{df/2} \\cdot \\Gamma((df - 1)/2)}, \\quad df = n_1 + n_2 - 2$$"),
                            HTML("$$\\text{Aproksimasi Polinomial Hedges (1981): } J = 1 - \\frac{3}{4(n_1 + n_2) - 9} = 1 - \\frac{3}{4df - 1}$$"),
                            HTML("$$\\text{Varians Sampling Asimtotik: } v_d = \\frac{n_1 + n_2}{n_1 n_2} + \\frac{d^2}{2(n_1 + n_2)}$$"),
                            HTML("$$\\text{Varians Hedges' g: } v_g = J^2 \\times v_d, \\quad \\text{Bobot Studi (Weight): } w_i = \\frac{1}{v_g}$$")
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          
          card(
            class = "mt-3",
            card_header(
              class = "d-flex justify-content-between align-items-center",
              span(icon("history"), " STUDI TERSIMPAN"),
              uiOutput("calc_saved_count_badge")
            ),
            card_body(
              p(class = "small text-muted mb-2", "Berikut adalah daftar studi yang telah terkumpul dari kalkulator effect size"),
              tableOutput("table_calc_recent_preview"),
              div(
                class = "d-flex gap-2 mt-2",
                actionButton("btn_goto_compiled", "Buka Matriks Gabungan →", class = "btn-sm btn-outline-primary flex-grow-1", icon = icon("layer-group")),
                actionButton("btn_calc_direct_model", "Langsung ke Pemodelan Sintesis →", class = "btn-sm btn-primary flex-grow-1", icon = icon("chart-pie"))
              )
            )
          )
        )
      )
    ),
    # Tab 3: Kumpulan Effect Size
    nav_panel(
      value = "tab_compiled",
      icon = icon("layer-group"),
      title = "Kumpulan Effect Size",
      card(
        card_header(
          class = "bg-light d-flex justify-content-between align-items-center flex-wrap gap-2",
          span(class = "fw-bold text-primary", icon("layer-group"), " Matriks Studi Hasil Kompilasi Kalkulator"),
          div(
            class = "d-flex gap-1 flex-wrap",
            actionButton("btn_use_calc_as_active", "Jadikan Dataset Analisis Utama", class = "btn-sm btn-primary", icon = icon("check-circle")),
            actionButton("btn_calc_to_model", "Analisis di Pemodelan Meta-Analisis →", class = "btn-sm btn-primary", icon = icon("chart-pie")),
            actionButton("btn_append_calc_to_active", "Gabungkan ke Data Aktif", class = "btn-sm btn-outline-primary", icon = icon("plus")),
            actionButton("btn_load_calc_sample", "Muat Contoh Batch (k=5)", class = "btn-sm btn-outline-secondary", icon = icon("magic")),
            actionButton("btn_delete_calc_row", "Hapus Baris Terpilih", class = "btn-sm btn-outline-danger", icon = icon("minus-circle")),
            actionButton("btn_clear_calc_compiled", "Kosongkan Semua", class = "btn-sm btn-outline-danger", icon = icon("trash")),
            downloadButton("btn_download_calc_csv", "Unduh CSV", class = "btn-sm btn-outline-success")
          )
        ),
        card_body(
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            value_box(
              title = "Studi Terkompilasi",
              value = textOutput("vb_calc_comp_k"),
              showcase = icon("database"),
              theme = "primary"
            ),
            value_box(
              title = "Rata-rata Effect Size",
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
              title = "Total Partisipan",
              value = textOutput("vb_calc_comp_n"),
              showcase = icon("users"),
              theme = "success"
            )
          ),
          
          uiOutput("calc_compiled_status_alert"),
          
          DTOutput("table_calc_compiled")
        )
      )
    ),
    
    # Tab 4: Data Sintesis
    nav_panel(
      value = "tab_data",
      icon = icon("database"),
      title = "Data Sintesis",
      card(
        card_header(
          class = "bg-light d-flex justify-content-between align-items-center flex-wrap gap-2",
          span(class = "fw-bold text-primary", icon("database"), " DATASET ANALISIS"),
          div(
            class = "d-flex gap-1 flex-wrap",
            actionButton("btn_load_benchmark", "Muat Data Benchmark (k=14)", class = "btn-sm btn-outline-primary", icon = icon("database")),
            actionButton("btn_delete_active_row", "Hapus Studi Terpilih", class = "btn-sm btn-outline-danger", icon = icon("minus-circle")),
            actionButton("btn_clear_data", "Reset Dataset", class = "btn-sm btn-outline-danger", icon = icon("trash")),
            downloadButton("btn_download_csv", "Unduh Data Aktif (.csv)", class = "btn-sm btn-outline-success"),
            downloadButton("btn_download_template_csv", "Unduh Template CSV", class = "btn-sm btn-outline-secondary"),
            actionButton("btn_data_to_model", "Lanjut ke Pemodelan Sintesis →", class = "btn-sm btn-primary", icon = icon("chart-pie"))
          )
        ),
        card_body(
          layout_columns(
            col_widths = c(4, 4, 4),
            div(
              h6("1. Pilih Sumber Dataset Aktif:", class = "fw-bold text-primary mb-2"),
              radioButtons(
                "dataset_source_choice",
                NULL,
                choices = c(
                  "Dataset Aplikasi (14 Uji Klinis RCT Psikologi)" = "benchmark",
                  "Kompilasi Hasil Kalkulator Effect Size" = "calculator",
                  "Unggah Berkas Mandiri (.csv / .xlsx)" = "upload"
                ),
                selected = "benchmark"
              ),
              conditionalPanel(
                condition = "input.dataset_source_choice == 'upload'",
                fileInput("upload_file", "Pilih Berkas (.csv / .xlsx):", accept = c(".csv", ".xlsx", ".xls"))
              )
            ),
            div(
              h6("2. Penyaring Cepat / Sub-Analisis Sensitivitas:", class = "fw-bold text-primary mb-2"),
              selectInput(
                "quick_sensitivity_filter",
                "Filter Inklusi Studi Efektivitas:",
                choices = c(
                  "Seluruh Studi dalam Dataset (Tanpa Filter)" = "all",
                  "Hanya Studi Risiko Bias Rendah (Low Risk of Bias)" = "low_rob",
                  "Kecualikan Kontrol Daftar Tunggu (Tanpa Waitlist)" = "no_waitlist",
                  "Hanya Populasi Klinis Terdiagnosis (Clinical MDD/Anxiety)" = "clinical_only"
                ),
                selected = "all"
              ),
              p(class = "small text-muted mb-0", "Klik dua kali pada sel tabel di bawah untuk menyunting nilai secara langsung, atau pilih baris dan klik 'Hapus Studi Terpilih'.")
            ),
            div(
              class = "p-3 bg-light rounded border",
              strong("Ketentuan Kolom Data Sintesis Bukti:"),
              tags$ul(
                class = "mb-0 ps-3 small text-muted",
                tags$li("Kolom besaran efek wajib: ", tags$code("yi"), " (atau ", tags$code("es"), ", ", tags$code("hedges_g"), ")"),
                tags$li("Kolom presisi wajib: ", tags$code("vi"), " (varians) atau ", tags$code("sei"), " (standar error)"),
                tags$li("Kolom moderator psikologi: ", tags$code("Control_Type"), ", ", tags$code("Format"), ", ", tags$code("Intervention"), ", ", tags$code("Population"), ", ", tags$code("Duration_Weeks"))
              )
            )
          ),
          
          hr(class = "my-3"),
          
          DTOutput("table_active_dataset")
        )
      )
    ),
    
    # Tab 5: Model Meta Analisis
    nav_panel(
      value = "tab_model",
      icon = icon("chart-pie"),
      title = "Model Meta Analisis",
      layout_sidebar(
        sidebar = sidebar(
          width = 330,
          title = "Spesifikasi Model Sintesis",
          
          selectInput(
            "meta_estimator",
            "Estimator Varians (tau^2):",
            choices = c(
              "Restricted Maximum Likelihood (REML)" = "REML",
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
            "meta_test_type",
            "Metode Inferensi & Interval Kepercayaan:",
            choices = c(
              "Distribusi Normal Standar (z-test)" = "z",
              "Koreksi Hartung-Knapp / HKSJ (t-test)" = "knha"
            ),
            selected = "z"
          ),
          
          selectInput(
            "conf_level",
            "Tingkat Kepercayaan (CI):",
            choices = c("95%" = 0.95, "99%" = 0.99),
            selected = 0.95
          ),
          
          hr(),
          
          h6("Meta-Regresi & Klaster Multilevel:", class = "fw-bold text-dark"),
          selectInput(
            "meta_continuous_covar",
            "Kovariat Kontinu (Meta-Regresi):",
            choices = c("Tanpa Kovariat Kontinu" = "none")
          ),
          selectInput(
            "ml_cluster_var",
            "Variabel Klaster Level-3 (rma.mv):",
            choices = c(
              "Pendekatan Intervensi (Intervention)" = "Intervention",
              "Populasi Klinis (Population)" = "Population",
              "Tipe Kelompok Kontrol (Control_Type)" = "Control_Type",
              "Format Penghantaran (Format)" = "Format",
              "Identitas Studi (Study)" = "Study"
            ),
            selected = "Intervention"
          )
        ),
        
        card(
          card_header(
            class = "bg-light d-flex justify-content-between align-items-center flex-wrap gap-2",
            span(class = "fw-bold text-primary", icon("chart-pie"), " Rangkuman Metrik Sintesis & Heterogenitas"),
            div(
              class = "d-flex gap-1",
              actionButton("btn_model_to_subgroup", "Analisis Subkelompok →", class = "btn-sm btn-outline-primary", icon = icon("object-group")),
              actionButton("btn_model_to_forest", "Forest Plot →", class = "btn-sm btn-outline-primary", icon = icon("tree")),
              actionButton("btn_model_to_bias", "Diagnostik Bias →", class = "btn-sm btn-outline-secondary", icon = icon("shield-alt"))
            )
          ),
          card_body(
            layout_columns(
              col_widths = c(3, 3, 3, 3),
              value_box(
                title = "Pooled Effect Size (Hedges' g)",
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
            
            hr(class = "my-3"),
            
            navset_card_tab(
              nav_panel(
                title = "Estimasi Model Formal (metafor)",
                div(
                  h6("Luaran Estimasi Formal Model Sintesis (metafor::rma):", class = "fw-bold text-primary mb-2"),
                  verbatimTextOutput("meta_model_summary")
                )
              ),
              nav_panel(
                title = "Meta-Regresi Kovariat Kontinu",
                uiOutput("meta_continuous_covar_ui")
              ),
              nav_panel(
                title = "Sensitivitas (Leave-One-Out) & Influensial",
                layout_columns(
                  col_widths = c(6, 6),
                  div(
                    h6("Diagnostik Studi Influensial & Outlier (Baujat Plot):", class = "fw-bold text-primary"),
                    plotOutput("meta_baujat_plot", height = "360px"),
                    p(class = "small text-muted mt-1", "Studi di kuadran kanan atas menyumbang heterogenitas Cochran Q terbesar sekaligus memberikan pengaruh paling kuat terhadap pergeseran efek gabungan.")
                  ),
                  div(
                    h6("Analisis Sensitivitas Leave-One-Out (Eksklusi Sekuensial):", class = "fw-bold text-primary"),
                    div(style = "max-height: 390px; overflow-y: auto;", tableOutput("table_leave1out"))
                  )
                )
              ),
              nav_panel(
                title = "Pemodelan Multilevel (3-Level rma.mv)",
                layout_columns(
                  col_widths = c(6, 6),
                  div(
                    h6("Dekomposisi Varians Tiga Aras (Cheung, 2014):", class = "fw-bold text-primary"),
                    tableOutput("table_multilevel_decomp"),
                    hr(class = "my-2"),
                    h6("Ringkasan Model Multilevel (rma.mv):", class = "fw-bold text-primary"),
                    verbatimTextOutput("multilevel_model_summary")
                  ),
                  div(
                    h6("Proporsi Varians Multilevel (Level 1 vs Level 2 vs Level 3):", class = "fw-bold text-primary"),
                    plotOutput("multilevel_variance_plot", height = "360px")
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # Tab 6: Forest Plot
    nav_panel(
      value = "tab_forest",
      icon = icon("tree"),
      title = "Forest Plot",
      layout_sidebar(
        sidebar = sidebar(
          width = 340,
          title = "Konfigurasi Visual Forest Plot",
          
          radioButtons(
            "forest_style",
            "Format Layout Publikasi:",
            choices = c(
              "Gaya Standar (meta::forest)" = "meta_style",
              "Gaya Klasik (metafor::forest)" = "metafor_style"
            ),
            selected = "meta_style"
          ),
          
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
          
          checkboxInput("forest_show_pi", "Tampilkan Prediction Interval (Bar Merah)", value = TRUE),
          checkboxInput("forest_show_weights", "Tampilkan Kolom Bobot Studi (Weight %)", value = TRUE),
          
          conditionalPanel(
            condition = "input.forest_style == 'metafor_style'",
            textInput("forest_label_left", "Label Sumbu Kiri (Kontrol Unggul):", value = "Control Favorable"),
            textInput("forest_label_right", "Label Sumbu Kanan (Intervensi Unggul):", value = "Intervention Favorable")
          ),
          
          sliderInput("forest_cex", "Skala Ukuran Teks / Titik:", min = 0.75, max = 1.30, value = 0.95, step = 0.05),
          
          hr(),
          
          downloadButton("btn_dl_forest_png", "Unduh PNG", class = "btn-primary w-100 mb-2"),
          downloadButton("btn_dl_forest_pdf", "Unduh PDF", class = "btn-secondary w-100")
        ),
        
        card(
          card_header(
            class = "bg-light fw-bold text-primary",
            icon("tree"), " FOREST PLOT "
          ),
          card_body(
            plotOutput("forest_plot_display", height = "680px")
          )
        )
      )
    ),
    
    # Tab 7: Bias Diagnostic
    nav_panel(
      value = "tab_bias",
      icon = icon("shield-alt"),
      title = "Bias Diagnostic",
      layout_sidebar(
        sidebar = sidebar(
          width = 330,
          title = "Pengaturan Plot Corong & Bias",
          
          radioButtons(
            "funnel_type",
            "Jenis Funnel Plot:",
            choices = c(
              "Contour-Enhanced (Zona Signifikansi)" = "contour",
              "Standard Funnel Plot (Pseudo 95% CI)" = "standard",
              "Trim and Fill Funnel Plot (Studi Imputasi)" = "trimfill"
            ),
            selected = "contour"
          ),
          
          numericInput(
            "bias_k_min",
            "Paksa Minimal Studi (k.min Egger & Fail-Safe N):",
            value = 5,
            min = 2,
            max = 10,
            step = 1
          ),
          
          numericInput(
            "fsn_orwin_target",
            "Target Besaran Efek Orwin Fail-Safe N (g):",
            value = 0.20,
            min = 0.05,
            max = 1.00,
            step = 0.05
          ),
          
          div(
            class = "small text-muted mb-3",
            tags$em("Contour-enhanced funnel plot"), " menampilkan zona signifikansi statistik (p < 0.10, p < 0.05, p < 0.01). Parameter ", tags$code("k.min = 5"), " memaksa komputasi uji asimetri Egger dan indeks ", tags$em("Fail-Safe N"), " (Rosenthal, Orwin, Rosenberg) tetap menghasilkan nilai meskipun jumlah studi terbatas (k >= 5)."
          ),
          
          hr(),
          
          downloadButton("btn_dl_funnel_png", "Unduh Funnel Plot PNG", class = "btn-primary w-100 mb-2"),
          downloadButton("btn_dl_funnel_pdf", "Unduh Funnel Plot PDF", class = "btn-secondary w-100")
        ),
        
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header(class = "bg-light fw-bold text-primary", icon("filter"), " FUNNEL PLOT"),
            card_body(
              plotOutput("funnel_plot_display", height = "560px")
            )
          ),
          card(
            card_header(class = "bg-light fw-bold text-primary", icon("shield-alt"), " UJI ASIMETRI & KETAHANAN PUBLIKASI"),
            card_body(
              h6("1. Uji Regresi Linier Egger (Egger's Test, k.min = 5):", class = "fw-bold text-dark"),
              tableOutput("table_egger_test"),
              
              hr(class = "my-2"),
              
              h6("2. Uji Korelasi Peringkat Begg & Mazumdar:", class = "fw-bold text-dark"),
              tableOutput("table_begg_test"),
              
              hr(class = "my-2"),
              
              h6("3. Analisis Imputasi Trim and Fill (Duval & Tweedie):", class = "fw-bold text-dark"),
              tableOutput("table_trimfill_summary"),
              
              hr(class = "my-2"),
              
              h6("4. Indeks Ketahanan Bukti Fail-Safe N (Rosenthal, Orwin & Rosenberg — Paksa k.min = 5):", class = "fw-bold text-dark"),
              tableOutput("table_failsafe_n"),
              
              div(
                class = "alert alert-light border mt-2 small",
                "Metode ", tags$em("Trim and Fill"), " mengestimasi jumlah studi yang hilang akibat bias publikasi, sedangkan ", tags$em("Fail-Safe N"), " (Rosenthal, Orwin, Rosenberg) mengevaluasi ketahanan sintesis bukti terhadap efek laci arsip (", tags$em("file-drawer problem"), ")."
              )
            )
          )
        )
      )
    ),
    
    # Tab 8: Sub Group & Multi Level Analysis
    nav_panel(
      value = "tab_subgroup",
      icon = icon("object-group"),
      title = "Sub Group & Multi Level Analysis",
      layout_sidebar(
        sidebar = sidebar(
          width = 330,
          title = "Konfigurasi Subkelompok",
          
          radioButtons(
            "subgroup_forest_style",
            "Format Layout Subgroup Forest:",
            choices = c(
              "Gaya Standar meta::forest (Author, Year, g, SE + Red PI)" = "meta_style",
              "Gaya Klasik metafor::forest (KAWE Academic Press)" = "metafor_style"
            ),
            selected = "meta_style"
          ),
          
          selectInput(
            "subgroup_var_select",
            "Variabel Subkelompok Klinis:",
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
          
          sliderInput("subgroup_forest_cex", "Skala Ukuran Teks / Titik:", min = 0.7, max = 1.3, value = 0.90, step = 0.05),
          
          hr(),
          
          downloadButton("btn_dl_subgroup_png", "Unduh PNG Subgroup", class = "btn-primary w-100 mb-2"),
          downloadButton("btn_dl_subgroup_pdf", "Unduh PDF Subgroup", class = "btn-secondary w-100 mb-2"),
          downloadButton("btn_dl_subgroup_csv", "Unduh Tabel Komparasi (.csv)", class = "btn-outline-primary w-100")
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
            
            hr(class = "my-3"),
            
            navset_card_tab(
              nav_panel(
                title = "Subgroup Forest Plot",
                plotOutput("subgroup_forest_plot_display", height = "720px")
              ),
              nav_panel(
                title = "Tabel Komparasi Subkelompok",
                tableOutput("table_subgroup_comprehensive")
              )
            )
          )
        )
      )
    )
  ),
  
  # Tab 9: Pedoman Metodologis
  nav_panel(
    value = "tab_guide",
    icon = icon("book"),
    title = "Pedoman Metodologis",
    div(
      class = "container-fluid py-3",
      style = "max-width: 1400px; margin: 0 auto;",
      
      # Header Card
      card(
        class = "mb-4 border-0 shadow-sm",
        style = "background: linear-gradient(135deg, #FAF8F5 0%, #FFFFFF 100%); border-left: 5px solid #650915 !important;",
        card_body(
          class = "py-3 px-4",
          div(
            class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
            div(
              h4(class = "fw-bold mb-1", style = "color: #650915; font-family: 'Cinzel', serif; letter-spacing: 0.05em;",
                 icon("book-open"), " PANDUAN SINTESIS BUKTI KLINIS"),
            ),
            div(
              span(class = "badge", style = "background-color: #650915; color: #FAF8F5; font-size: 0.8rem; padding: 0.45rem 0.75rem;", "Gold Standard EBP")
            )
          )
        )
      ),
      
      # Bagian 1: Estimasi Besaran Efek (d vs g)
      card(
        class = "mb-4 shadow-sm",
        card_header(
          class = "bg-light d-flex align-items-center",
          span(class = "fw-bold text-primary", icon("balance-scale"), " 1. ESTIMASI BESARAN EFEK TERSTANDARISASI (COHEN'S d vs HEDGES' g)")
        ),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            div(
              h6("Standardized Mean Difference: Koreksi Bias Sampel Kecil", class = "text-primary fw-bold mb-2"),
              p(
                "Pada uji klinis psikologi dengan ukuran sampel per kelompok kecil (", tags$code("n < 50"), "), ",
                "estimator standar ", tags$em("Cohen's d"), " secara matematis mengalami bias positif (", tags$em("upward positive bias"), "). ",
                "Kondisi ini menyebabkan estimasi efektivitas intervensi tampak lebih tinggi daripada parameter populasi sebenarnya."
              ),
              p(
                "Hedges (1981) memperkenalkan faktor pembobot koreksi ", tags$em("J"), " berbasis fungsi Gamma dan derajat kebebasan (", tags$code("df = n1 + n2 - 2"), "). ",
                "Faktor ini menghasilkan ", tags$em("unbiased standardized mean difference"), " (", tags$em("Hedges' g"), "). ",
                "Berdasarkan pedoman Cochrane Handbook dan PRISMA 2020, pelaporan ", tags$em("Hedges' g"), " merupakan standard emas yang wajib diutamakan pada seluruh sintesis bukti kuantitatif."
              )
            ),
            div(
              h6("Formulasi Matematis & Notasi:", class = "fw-bold text-dark mb-2"),
              div(class = "kawe-eq-box mb-2",
                HTML("$$d = \\frac{\\bar{X}_1 - \\bar{X}_2}{s_{\\text{pooled}}}, \\quad s_{\\text{pooled}} = \\sqrt{\\frac{(n_1 - 1)s_1^2 + (n_2 - 1)s_2^2}{n_1 + n_2 - 2}}$$"),
                HTML("$$J = 1 - \\frac{3}{4(n_1 + n_2) - 9}, \\quad g = J \\times d, \\quad v_g = J^2 \\left[ \\frac{n_1 + n_2}{n_1 n_2} + \\frac{d^2}{2(n_1 + n_2)} \\right]$$")
              ),
              h6("Pedoman Magnitudo Efek (Cohen, 1988):", class = "fw-bold text-dark mt-2 mb-1"),
              tags$ul(
                class = "small text-muted mb-0",
                tags$li(strong("|g| < 0.20: "), "Efek dapat diabaikan (negligible effect)"),
                tags$li(strong("0.20 <= |g| < 0.50: "), "Besaran efek kecil (small effect)"),
                tags$li(strong("0.50 <= |g| < 0.80: "), "Besaran efek sedang (medium effect)"),
                tags$li(strong("|g| >= 0.80: "), "Besaran efek besar (large clinical effect)")
              )
            )
          )
        )
      ),
      
      # Bagian 2: Spesifikasi Model Sintesis (FE vs RE)
      card(
        class = "mb-4 shadow-sm",
        card_header(
          class = "bg-light d-flex align-items-center",
          span(class = "fw-bold text-primary", icon("cubes"), " 2. SPESIFIKASI MODEL SINTESIS (FIXED-EFFECT vs RANDOM-EFFECTS)")
        ),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            div(
              h6("Model Efek Tetap (Fixed-Effect / FE) vs Efek Acak (Random-Effects / RE)", class = "text-primary fw-bold mb-2"),
              p(
                strong("Model Efek Tetap (Fixed-Effect / FE): "),
                "Mengasumsikan bahwa seluruh studi membagikan satu besaran efek sejati tunggal (", tags$code("theta"), "). ",
                "Seluruh variabilitas yang teramati antarstudi diasumsikan murni akibat kesalahan pengambilan sampel (", tags$em("sampling error"), "). ",
                "Asumsi ini hampir selalu tidak realistis pada uji intervensi klinis psikologis di mana karakteristik subjek, kualifikasi klinisi, dan format terapi bervariasi antarstudi."
              ),
              p(
                strong("Model Efek Acak (Random-Effects / RE): "),
                "Mengasumsikan bahwa besaran efek sejati bervariasi antarpopulasi dan berdistribusi normal di sekitar rata-rata populasi (", tags$code("mu"), ") dengan varians antarstudi sejati ",
                tags$code("tau^2"), ". Inferensi RE memungkinkan generalisasi eksternal melampaui studi-studi primer yang teramati."
              )
            ),
            div(
              h6("Formulasi & Struktur Pembobotan:", class = "fw-bold text-dark mb-2"),
              div(class = "kawe-eq-box mb-2",
                HTML("$$\\text{Model FE: } y_i = \\theta + e_i, \\quad w_i = \\frac{1}{v_i}, \\quad SE(\\hat{\\theta}) = \\sqrt{\\frac{1}{\\sum w_i}}$$"),
                HTML("$$\\text{Model RE: } y_i = \\mu + u_i + e_i, \\quad w_i^* = \\frac{1}{v_i + \\hat{\\tau}^2}, \\quad SE(\\hat{\\mu}) = \\sqrt{\\frac{1}{\\sum w_i^*}}$$")
              ),
              div(
                class = "alert alert-light border small mb-0 py-2",
                strong("Rekomendasi Metodologis: "),
                "Gunakan Model Efek Acak (RE) dengan estimator REML (Restricted Maximum Likelihood) atau Paule-Mandel, disertai koreksi Knapp-Hartung (Hartung-Knapp/Sidik-Jonkman) untuk mengontrol tingkat kesalahan Tipe I bila jumlah studi primer k terbatas."
              )
            )
          )
        )
      ),
      
      # Bagian 3: Heterogenitas & Prediction Interval
      card(
        class = "mb-4 shadow-sm",
        card_header(
          class = "bg-light d-flex align-items-center",
          span(class = "fw-bold text-primary", icon("chart-area"), " 3. EVALUASI HETEROGENITAS & PREDICTION INTERVAL")
        ),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            div(
              h6("Demistifikasi Heterogenitas: tau^2, I^2 & Interval Prediksi (PI)", class = "text-primary fw-bold mb-2"),
              p(
                "Statistik ", tags$em("I^2"), " bukan merupakan ukuran absolut dari besaran varians antarstudi, melainkan rasio variabilitas sejati terhadap variabilitas total. ",
                "Nilai ", tags$em("I^2"), " sangat dipengaruhi oleh presisi sampling studi primer; peningkatan ukuran sampel N secara otomatis menaikkan ", tags$em("I^2"), " kendati dispersi sejati tetap konstan (Borenstein et al., 2017)."
              ),
              p(
                "Oleh karena itu, varians antarstudi sejati ", tags$code("tau^2"), " (atau deviasi baku antarstudi ", tags$code("tau"), ") dan ",
                tags$strong("Interval Prediksi 95% (Prediction Interval / PI)"), " wajib dilaporkan. ",
                "Interval Prediksi menggambarkan rentang di mana besaran efek intervensi yang sesungguhnya diharapkan berada pada populasi klinis baru di masa depan."
              )
            ),
            div(
              h6("Formulasi Statistik Heterogenitas:", class = "fw-bold text-dark mb-2"),
              div(class = "kawe-eq-box mb-2",
                HTML("$$Q = \\sum_{i=1}^k w_i(y_i - \\hat{\\theta}_{\\text{FE}})^2 \\sim \\chi^2_{k-1}, \\quad I^2 = \\max\\left(0, \\frac{Q - (k-1)}{Q}\\right) \\times 100\\%$$"),
                HTML("$$\\text{Prediction Interval 95\\%: } \\hat{\\mu} \\pm t_{k-2, 0.975} \\sqrt{\\hat{\\tau}^2 + SE(\\hat{\\mu})^2}$$")
              ),
              h6("Klasifikasi Heterogenitas Higgins et al. (2003):", class = "fw-bold text-dark mt-2 mb-1"),
              tags$ul(
                class = "small text-muted mb-0",
                tags$li(strong("I^2 < 25%: "), "Heterogenitas rendah (low)"),
                tags$li(strong("25% <= I^2 < 50%: "), "Heterogenitas moderat (moderate)"),
                tags$li(strong("50% <= I^2 < 75%: "), "Heterogenitas substansial (substantial)"),
                tags$li(strong("I^2 >= 75%: "), "Heterogenitas tinggi (considerable heterogeneity)")
              )
            )
          )
        )
      ),
      
      # Bagian 4: Bias Publikasi & Egger
      card(
        class = "mb-4 shadow-sm",
        card_header(
          class = "bg-light d-flex align-items-center",
          span(class = "fw-bold text-primary", icon("shield-alt"), " 4. EVALUASI BIAS PUBLIKASI & ANALISIS SENSITIVITAS")
        ),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            div(
              h6("Evaluasi Bias Publikasi & Efek Studi Kecil (Small-Study Effects)", class = "text-primary fw-bold mb-2"),
              p(
                "Asimetri pada ", tags$em("funnel plot"), " tidak selalu identik dengan bias publikasi; asimetri dapat mencerminkan heterogenitas metodologis atau efek studi kecil (", tags$em("small-study effects"), "). ",
                tags$em("Contour-enhanced funnel plot"), " memfasilitasi pembuktian apakah studi-studi kecil yang hilang terkonsentrasi secara selektif pada area ketidaksignifikanan statistik (", tags$code("p > 0.05"), ") versus area signifikan (Peters et al., 2008)."
              ),
              p(
                "Uji formal Egger mengevaluasi asimetri corong secara matematis melalui model regresi terbobot. ",
                "Metode ", tags$em("Trim and Fill"), " mengestimasi jumlah studi yang hilang serta memodelkan efek gabungan terkoreksi, sementara ",
                tags$em("Fail-Safe N"), " (Rosenthal, Orwin, Rosenberg) mengevaluasi ketahanan terhadap ", tags$em("file-drawer problem"), "."
              )
            ),
            div(
              h6("Formulasi Regresi Uji Asimetri Egger:", class = "fw-bold text-dark mb-2"),
              div(class = "kawe-eq-box mb-2",
                HTML("$$\\frac{y_i}{SE_i} = \\beta_0 + \\beta_1 \\left(\\frac{1}{SE_i}\\right) + \\epsilon_i, \\quad H_0: \\beta_0 = 0$$")
              ),
              h6("Ambang Batas Ketahanan Rosenthal Fail-Safe N:", class = "fw-bold text-dark mt-2 mb-1"),
              div(class = "kawe-eq-box mb-2",
                HTML("$$N_{\\text{fs}} = \\frac{\\left(\\sum z_i\\right)^2}{2.706} - k, \\quad \\text{Kriteria Kuat: } N_{\\text{fs}} > 5k + 10$$")
              ),
              p(class = "small text-muted mb-0", "Bila N_fs melampaui ambang 5k + 10, kesimpulan sintesis bukti terbukti tahan terhadap potensi studi yang tidak terpublikasi.")
            )
          )
        )
      ),
      
      # Bagian 5: Taksonomi Rumus Effect Size (Ditulis Mengalir Kebawah Penuh)
      card(
        class = "mb-4 shadow-sm",
        card_header(
          class = "bg-light d-flex align-items-center",
          span(class = "fw-bold text-primary", icon("square-root-alt"), " 5. TAKSONOMI FORMULASI BESARAN EFEK (EFFECT SIZES)")
        ),
        card_body(
          p(class = "text-muted mb-3",
            "Berikut merupakan kompendium formulasi matematis baku untuk konversi berbagai format data hasil studi primer ke dalam metrik Standardized Mean Difference (SMD: Cohen's d & Hedges' g) dan varians samplingnya (vi), sesuai rujukan baku Doing Meta-Analysis in R (Harrer et al., 2021; Bab 3)."),
          
          # 5.1 Post-Test Means & SDs
          div(
            class = "p-3 mb-3 rounded border",
            style = "background-color: #FAF8F5; border-color: rgba(197, 160, 89, 0.3) !important;",
            layout_columns(
              col_widths = c(6, 6),
              div(
                h6(class = "fw-bold mb-1", style = "color: #650915;", "5.1 Skor Pascates Dua Kelompok Independen (Between-Group Means & SDs)"),
                p(class = "small mb-1", "Diterapkan pada uji acak terkontrol (RCT) standar yang melaporkan rerata (M), deviasi baku (SD), dan ukuran sampel (n) per kelompok pascates (Cohen, 1988; Hedges, 1981):"),
                tags$ul(
                  class = "small text-muted mb-0",
                  tags$li("Membagi selisih rerata terarah dengan deviasi baku gabungan (s_pooled)."),
                  tags$li("Koreksi Hedges J wajib diterapkan untuk menghilangkan bias positif sampel kecil."),
                  tags$li("Varians sampling vg dihitung untuk pembobotan presisi sintesis.")
                )
              ),
              div(
                div(class = "kawe-eq-box my-0",
                  HTML("$$s_{\\text{pooled}} = \\sqrt{\\frac{(n_1 - 1)s_1^2 + (n_2 - 1)s_2^2}{n_1 + n_2 - 2}}, \\quad d = \\frac{\\bar{X}_1 - \\bar{X}_2}{s_{\\text{pooled}}}$$"),
                  HTML("$$J = 1 - \\frac{3}{4(n_1 + n_2) - 9}, \\quad g = J \\cdot d, \\quad v_g = J^2 \\left[ \\frac{n_1 + n_2}{n_1 n_2} + \\frac{d^2}{2(n_1 + n_2)} \\right]$$")
                )
              )
            )
          ),
          
          # 5.2 Pre-Post Mean Gain
          div(
            class = "p-3 mb-3 rounded border",
            style = "background-color: #FAF8F5; border-color: rgba(197, 160, 89, 0.3) !important;",
            layout_columns(
              col_widths = c(6, 6),
              div(
                h6(class = "fw-bold mb-1", style = "color: #650915;", "5.2 Skor Perubahan Prates-Pascates (Repeated Measures PPCR / Gain Score)"),
                p(class = "small mb-1", "Diterapkan ketika studi melaporkan perubahan skor pre-post (Gain) antara kelompok intervensi dan kontrol (Becker, 1988; Morris, 2008):"),
                tags$ul(
                  class = "small text-muted mb-0",
                  tags$li("Deviasi baku perubahan mengintegrasikan korelasi berpasangan (r)."),
                  tags$li("Asumsi Rosenthal r = 0.50 digunakan bila r tidak dilaporkan dalam artikel primer."),
                  tags$li("Varians memperhitungkan pengurangan sampling error akibat desain pengukuran berulang.")
                )
              ),
              div(
                div(class = "kawe-eq-box my-0",
                  HTML("$$s_{\\text{change}, i} = \\sqrt{s_{pre, i}^2 + s_{post, i}^2 - 2 r s_{pre, i} s_{post, i}}$$"),
                  HTML("$$d_{\\text{gain}} = \\frac{\\Delta\\text{Gain}}{s_{\\text{pooled, change}}}, \\quad v_d = \\frac{2(1 - r_1)}{n_1} + \\frac{2(1 - r_2)}{n_2} + \\frac{d^2}{2(n_1 + n_2)}$$")
                )
              )
            )
          ),
          
          # 5.3 Parametrik t, F, p
          div(
            class = "p-3 mb-3 rounded border",
            style = "background-color: #FAF8F5; border-color: rgba(197, 160, 89, 0.3) !important;",
            layout_columns(
              col_widths = c(6, 6),
              div(
                h6(class = "fw-bold mb-1", style = "color: #650915;", "5.3 Rekonstruksi dari Nilai Statistik Inferensial (Parametrik t, F, p)"),
                p(class = "small mb-1", "Digunakan ketika artikel primer tidak melaporkan mean dan SD secara lengkap, melainkan hanya nilai uji statistik (Dunlap et al., 1996; Lipsey & Wilson, 2001):"),
                tags$ul(
                  class = "small text-muted mb-0",
                  tags$li("Uji-t independen: Konversi langsung berbasis nilai t dan ukuran sampel."),
                  tags$li("Uji-F satu arah (df1 = 1): Memanfaatkan relasi matematis t = sqrt(F)."),
                  tags$li("Nilai-p dua arah: Rekonstruksi nilai t via inversi kuantil Student's t distribution.")
                )
              ),
              div(
                div(class = "kawe-eq-box my-0",
                  HTML("$$\\text{Uji-t: } d = t \\times \\sqrt{\\frac{n_1 + n_2}{n_1 n_2}}, \\quad \\text{Uji-F: } d = \\pm \\sqrt{F \\left(\\frac{n_1 + n_2}{n_1 n_2}\\right)}$$"),
                  HTML("$$\\text{Nilai-p: } t = F_{t, df}^{-1}(1 - p/2), \\quad g = J \\times d$$")
                )
              )
            )
          ),
          
          # 5.4 Biner & Tabel 2x2
          div(
            class = "p-3 mb-3 rounded border",
            style = "background-color: #FAF8F5; border-color: rgba(197, 160, 89, 0.3) !important;",
            layout_columns(
              col_widths = c(6, 6),
              div(
                h6(class = "fw-bold mb-1", style = "color: #650915;", "5.4 Transformasi Luaran Kategorikal Biner ke Skala d (Tabel Kontingensi 2×2 & Odds Ratio)"),
                p(class = "small mb-1", "Mengonversi rasio odds (Odds Ratio) dari data dikotomi kejadian klinis (event / non-event) ke skala continuous standardized mean difference (Chinn, 2000; Hasselblad & Hedges, 1995):"),
                tags$ul(
                  class = "small text-muted mb-0",
                  tags$li("Berdasarkan asumsi variabel laten berdistribusi logistik kontinu."),
                  tags$li("Faktor pengali sqrt(3)/pi ~ 0.5513 menyelaraskan varians logistik ke normal baku."),
                  tags$li("Varians sampling d ditransformasikan secara proporsional dari varians Woolf.")
                )
              ),
              div(
                div(class = "kawe-eq-box my-0",
                  HTML("$$OR = \\frac{a \\cdot d}{b \\cdot c}, \\quad v_{\\ln(OR)} = \\frac{1}{a} + \\frac{1}{b} + \\frac{1}{c} + \\frac{1}{d}$$"),
                  HTML("$$d = \\ln(OR) \\times \\frac{\\sqrt{3}}{\\pi} \\approx 0.5513 \\ln(OR), \\quad v_d = v_{\\ln(OR)} \\times \\frac{3}{\\pi^2}$$")
                )
              )
            )
          ),
          
          # 5.5 Korelasi r & Fisher z
          div(
            class = "p-3 mb-0 rounded border",
            style = "background-color: #FAF8F5; border-color: rgba(197, 160, 89, 0.3) !important;",
            layout_columns(
              col_widths = c(6, 6),
              div(
                h6(class = "fw-bold mb-1", style = "color: #650915;", "5.5 Koefisien Asosiasi & Korelasi (Pearson r & Fisher's z)"),
                p(class = "small mb-1", "Mengintegrasikan korelasi point-biserial atau Pearson r ke dalam skala standardized mean difference (Borenstein et al., 2009; Jacobs & Viechtbauer, 2017):"),
                tags$ul(
                  class = "small text-muted mb-0",
                  tags$li("Korelasi point-biserial r dikonversi ke d dengan formula Jacobs."),
                  tags$li("Transformasi Fisher's z menstabilkan varians sampling independen dari nilai r populasi."),
                  tags$li("Varians sampling Fisher z: vz = 1 / (N - 3).")
                )
              ),
              div(
                div(class = "kawe-eq-box my-0",
                  HTML("$$d = \\frac{2r}{\\sqrt{1 - r^2}}, \\quad J = 1 - \\frac{3}{4N - 9}, \\quad g = J \\times d$$"),
                  HTML("$$z = \\frac{1}{2}\\ln\\left(\\frac{1 + r}{1 - r}\\right) = \\text{arctanh}(r), \\quad v_z = \\frac{1}{N - 3}$$")
                )
              )
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
  benchmark_storage <- reactiveVal(create_benchmark_data())
  calc_compiled_data <- reactiveVal(data.frame()) # starts clean, users can load sample or compute
  active_data <- reactiveVal(create_benchmark_data())
  active_source_label <- reactiveVal("Benchmark RCT (k = 14)")
  calc_result <- reactiveVal(NULL)
  
  # Filtered Active Dataset (supports Quick Sensitivity Filter in Tab 4)
  filtered_active_data <- reactive({
    df <- active_data()
    if (nrow(df) == 0) return(df)
    flt <- input$quick_sensitivity_filter
    if (!is.null(flt) && flt != "all") {
      if (flt == "low_rob" && "Risk_of_Bias" %in% colnames(df)) {
        df <- df[df$Risk_of_Bias == "Low Risk", , drop = FALSE]
      } else if (flt == "no_waitlist" && "Control_Type" %in% colnames(df)) {
        df <- df[df$Control_Type != "Waitlist", , drop = FALSE]
      } else if (flt == "clinical_only" && "Population" %in% colnames(df)) {
        df <- df[grepl("^Clinical", df$Population, ignore.case = TRUE), , drop = FALSE]
      }
    }
    df
  })
  
  # Hero & Workflow Navigation Buttons
  observeEvent(input$hero_btn_calc, {
    nav_select("main_navbar", "tab_calc")
  })
  observeEvent(input$hero_btn_data, {
    nav_select("main_navbar", "tab_model")
  })
  observeEvent(input$hero_btn_subgroup, {
    nav_select("main_navbar", "tab_forest")
  })
  observeEvent(input$btn_goto_compiled, {
    nav_select("main_navbar", "tab_compiled")
  })
  observeEvent(input$btn_data_to_model, {
    nav_select("main_navbar", "tab_model")
  })
  observeEvent(input$btn_model_to_subgroup, {
    nav_select("main_navbar", "tab_subgroup")
  })
  observeEvent(input$btn_model_to_forest, {
    nav_select("main_navbar", "tab_forest")
  })
  observeEvent(input$btn_model_to_bias, {
    nav_select("main_navbar", "tab_bias")
  })
  observeEvent(input$btn_calc_direct_model, {
    df_c <- calc_compiled_data()
    if (nrow(df_c) >= 2 && !isTRUE(input$calc_auto_push_active)) {
      active_data(df_c)
      active_source_label(paste0("Kompilasi Kalkulator (k = ", nrow(df_c), ")"))
      updateRadioButtons(session, "dataset_source_choice", selected = "calculator")
    }
    nav_select("main_navbar", "tab_model")
  })
  observeEvent(input$btn_calc_to_model, {
    df <- calc_compiled_data()
    if (nrow(df) < 2) {
      showNotification("Minimal dibutuhkan 2 studi dalam matriks kalkulator untuk menjalankan meta-analisis.", type = "warning")
      return()
    }
    active_data(df)
    active_source_label(paste0("Kompilasi Kalkulator (k = ", nrow(df), ")"))
    updateRadioButtons(session, "dataset_source_choice", selected = "calculator")
    nav_select("main_navbar", "tab_model")
    showNotification("Matriks kalkulator diaktifkan. Menampilkan hasil pemodelan meta-analisis.", type = "message")
  })
  
  # Tab 1: Normal Distribution Engine Plot
  output$kawe_normal_demo_plot <- renderPlot({
    mu_val <- input$demo_mu
    sigma_val <- input$demo_sigma
    show_zones <- input$demo_show_zones
    show_scales <- input$demo_show_scales
    
    x <- seq(mu_val - 4 * sigma_val, mu_val + 4 * sigma_val, length.out = 500)
    y <- dnorm(x, mean = mu_val, sd = sigma_val)
    
    par(mar = c(4.8, 4.5, 2.5, 1.5), bg = "#FFFFFF")
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
      idx2 <- x >= (mu_val - 2 * sigma_val) & x <= (mu_val + 2 * sigma_val)
      polygon(c(x[idx2], rev(x[idx2])), c(y[idx2], rep(0, sum(idx2))), col = rgb(197/255, 160/255, 89/255, 0.20), border = NA)
      idx1 <- x >= (mu_val - 1 * sigma_val) & x <= (mu_val + 1 * sigma_val)
      polygon(c(x[idx1], rev(x[idx1])), c(y[idx1], rep(0, sum(idx1))), col = rgb(197/255, 160/255, 89/255, 0.45), border = NA)
    }
    
    lines(x, y, col = "#650915", lwd = 3.2)
    abline(v = mu_val, col = "#C5A059", lty = 2, lwd = 1.8)
    
    usr <- par("usr")
    text(mu_val, usr[4] * 0.92, sprintf("mu = %.2f, sigma = %.2f", mu_val, sigma_val), col = "#650915", font = 2, pos = 4)
    
    if (isTRUE(show_scales)) {
      text(usr[1] + 0.2, usr[4] * 0.85, "Standard Psychometric Equivalences:\n• z-score = (x - mu) / sigma\n• T-score = 50 + 10z\n• Wechsler IQ = 100 + 15z", adj = c(0, 1), cex = 0.82, col = "#5A4B4C", font = 3)
    }
  })
  
  # Core Function to Compute Effect Size from Inputs
  compute_current_es <- function() {
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
          g1_m <- p1_m - po1_m
          g2_m <- p2_m - po2_m
        } else {
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
        
        if (gdir == "decrease_better" && g1_m < 0 && g2_m <= 0) {
          g1_m <- -g1_m
          g2_m <- -g2_m
        }
        
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
        grp1m = input$grp1m_sd, grp1sd = input$grp1sd_sd, grp1n = input$grp1n_sd,
        grp2m = input$grp2m_sd, grp2sd = input$grp2sd_sd, grp2n = input$grp2n_sd,
        es.type = metric
      )
      if (!is.null(input$sd_outcome_dir) && input$sd_outcome_dir == "decrease_better") {
        res$es <- -res$es
        temp_lo <- -res$ci.hi
        res$ci.hi <- -res$ci.lo
        res$ci.lo <- temp_lo
      }
    } else if (itype == "mean_se") {
      res <- esc_mean_se(
        grp1m = input$grp1m_se, grp1se = input$grp1se_se, grp1n = input$grp1n_se,
        grp2m = input$grp2m_se, grp2se = input$grp2se_se, grp2n = input$grp2n_se,
        es.type = metric
      )
      if (!is.null(input$se_outcome_dir) && input$se_outcome_dir == "decrease_better") {
        res$es <- -res$es
        temp_lo <- -res$ci.hi
        res$ci.hi <- -res$ci.lo
        res$ci.lo <- temp_lo
      }
    } else if (itype == "t_test") {
      res <- esc_t(t = input$t_val, grp1n = input$t_n1, grp2n = input$t_n2, es.type = metric)
    } else if (itype == "p_val") {
      df_deg <- input$p_n1 + input$p_n2 - 2
      t_equiv <- qt(1 - (input$p_value / 2), df_deg)
      if (input$p_direction == "neg") t_equiv <- -t_equiv
      res <- esc_t(t = t_equiv, grp1n = input$p_n1, grp2n = input$p_n2, es.type = metric)
    } else if (itype == "f_test") {
      res <- esc_f(f = input$f_val, grp1n = input$f_n1, grp2n = input$f_n2, es.type = metric)
      if (input$f_direction == "neg") {
        res$es <- -abs(res$es)
        temp_lo <- -res$ci.hi
        res$ci.hi <- -res$ci.lo
        res$ci.lo <- temp_lo
      }
    } else if (itype == "table_2x2") {
      res <- esc_2x2(
        grp1yes = input$tab_e1, grp1no = input$tab_n1 - input$tab_e1,
        grp2yes = input$tab_e2, grp2no = input$tab_n2 - input$tab_e2,
        es.type = metric
      )
    } else if (itype == "correlation") {
      res <- esc_rpb(r = input$corr_r, totaln = input$corr_n, es.type = metric)
    }
    res
  }
  
  # Live Reactive Auto-Calculation for Calculator Inputs
  observe({
    res <- tryCatch(compute_current_es(), error = function(e) NULL)
    if (!is.null(res) && length(res$es) == 1 && is.finite(res$es) && is.finite(res$var) && res$var > 0) {
      calc_result(res)
    }
  })
  
  # Observe Calculate Button
  observeEvent(input$btn_calculate, {
    tryCatch({
      res <- compute_current_es()
      calc_result(res)
      showNotification("Estimasi besaran efek berhasil dikalkulasi ulang.", type = "message")
    }, error = function(e) {
      showNotification(paste("Kesalahan perhitungan:", e$message), type = "error")
    })
  })
  
  # Observe Append Study Button (Smart auto-compute + add)
  observeEvent(input$btn_append_study, {
    res <- calc_result()
    if (is.null(res)) {
      res <- tryCatch(compute_current_es(), error = function(e) NULL)
      if (is.null(res)) {
        showNotification("Gagal menghitung besaran efek. Periksa kembali input numerik.", type = "error")
        return()
      }
      calc_result(res)
    }
    
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
    
    # Append to compiled calculator batch
    comp_df <- calc_compiled_data()
    calc_compiled_data(rbind(comp_df, new_row))
    
    # If auto-push is checked, also append to active analysis dataset
    if (isTRUE(input$calc_auto_push_active)) {
      curr_act <- active_data()
      active_data(rbind(curr_act, new_row))
    }
    
    showNotification(paste0("Studi '", input$study_name, "' berhasil disimpan ke matriks gabungan kalkulator."), type = "message")
  })
  
  # Reset Calculator Form Button
  observeEvent(input$btn_reset_calc_form, {
    updateTextInput(session, "study_name", value = paste("Studi Baru", nrow(calc_compiled_data()) + 1))
    calc_result(NULL)
    showNotification("Formulir telah disiapkan untuk entri studi berikutnya.", type = "default")
  })
  
  # Calculator Outputs
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
  
  output$calc_visual_preview_plot <- renderPlot({
    res <- calc_result()
    req(!is.null(res), is.finite(res$es), is.finite(res$ci.lo), is.finite(res$ci.hi))
    par(mar = c(3.2, 1.5, 1.2, 1.5), bg = "#FFFFFF")
    xlim_range <- c(min(-0.6, res$ci.lo - 0.25), max(1.4, res$ci.hi + 0.25))
    plot(
      0, 0, type = "n",
      xlim = xlim_range, ylim = c(0.4, 1.6),
      yaxt = "n", ylab = "",
      xlab = "Besaran Efek Terstandarisasi (g > 0 = Memihak Intervensi)",
      bty = "n", cex.lab = 0.85, col.lab = "#650915"
    )
    # Shaded magnitude zones
    rect(0.2, 0.45, 0.5, 1.55, col = rgb(197/255, 160/255, 89/255, 0.10), border = NA)
    rect(0.5, 0.45, 0.8, 1.55, col = rgb(197/255, 160/255, 89/255, 0.20), border = NA)
    rect(0.8, 0.45, xlim_range[2], 1.55, col = rgb(101/255, 9/255, 21/255, 0.08), border = NA)
    abline(v = 0, lty = 2, lwd = 1.5, col = "#5A4B4C")
    abline(v = c(0.2, 0.5, 0.8), lty = 3, col = "#C5A059")
    segments(res$ci.lo, 1.0, res$ci.hi, 1.0, col = "#650915", lwd = 3.0)
    arrows(res$ci.lo, 1.0, res$ci.hi, 1.0, code = 3, angle = 90, length = 0.06, col = "#650915", lwd = 2.2)
    points(res$es, 1.0, pch = 18, cex = 2.2, col = "#650915")
    text(
      res$es, 1.32,
      sprintf("g = %.3f [%.3f, %.3f]", res$es, res$ci.lo, res$ci.hi),
      col = "#650915", font = 2, cex = 0.88
    )
  })
  
  output$calc_summary_table <- renderTable({
    res <- calc_result()
    if (is.null(res)) {
      return(data.frame(Status = "Masukkan parameter numerik dan klik 'Hitung Parameter Efek'."))
    }
    w_val <- ifelse(is.finite(res$var) && res$var > 0, sprintf("%.2f (1/vi)", 1 / res$var), "-")
    
    data.frame(
      Parameter = c(
        "Metrik & Arah Besaran Efek",
        "Titik Estimasi Efek (Point Estimate)",
        "Varians Sampling (vi)",
        "Standar Error (sei)",
        "Bobot Studi (Weight wi = 1/vi)",
        "Interval Kepercayaan 95% CI",
        "Kategori Cohen (1988)"
      ),
      Nilai = c(
        paste0(ifelse(input$es_metric == "g", "Hedges' g (Unbiased)", "Cohen's d"), " (", ifelse(res$es >= 0, "Memihak Intervensi", "Memihak Kontrol"), ")"),
        sprintf("%.4f", res$es),
        sprintf("%.5f", res$var),
        sprintf("%.4f", res$se),
        w_val,
        sprintf("[%.4f, %.4f]", res$ci.lo, res$ci.hi),
        ifelse(abs(res$es) < 0.2, "Dapat Diabaikan (< 0.20)",
               ifelse(abs(res$es) < 0.5, "Kecil (0.20 - 0.49)",
                      ifelse(abs(res$es) < 0.8, "Sedang (0.50 - 0.79)", "Besar (>= 0.80)")))
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Active Method Badge in Card Header
  output$calc_active_formula_badge <- renderUI({
    itype <- input$input_type
    metric_lbl <- ifelse(input$es_metric == "g", "Hedges' g (Unbiased)", "Cohen's d")
    type_lbl <- switch(itype,
      "mean_gain" = "Pre-Post Mean Gain",
      "mean_sd" = "Post-Test Means & SDs",
      "mean_se" = "Means & SE",
      "t_test" = "Independent t-test",
      "p_val" = "p-value & Sample Sizes",
      "f_test" = "One-Way ANOVA F-test",
      "table_2x2" = "2x2 Contingency Table (OR)",
      "correlation" = "Correlation Coefficient (r)",
      itype
    )
    span(class = "badge bg-primary", paste("Metode:", type_lbl, "|", metric_lbl))
  })
  
  # Quick Formula Callout in Output Card
  output$calc_quick_formula_callout <- renderUI({
    itype <- input$input_type
    metric_lbl <- ifelse(input$es_metric == "g", "Hedges' g", "Cohen's d")
    eq_str <- switch(itype,
      "mean_sd" = "$$d = \\frac{\\bar{X}_1 - \\bar{X}_2}{s_{\\text{pooled}}}, \\quad s_{\\text{pooled}} = \\sqrt{\\frac{(n_1-1)s_1^2 + (n_2-1)s_2^2}{n_1+n_2-2}}, \\quad g = J \\cdot d$$",
      "mean_se" = "$$s_i = SE_i \\sqrt{n_i}, \\quad d = \\frac{\\bar{X}_1 - \\bar{X}_2}{s_{\\text{pooled}}}, \\quad g = J \\cdot d$$",
      "mean_gain" = "$$\\text{Gain}_i = \\Delta\\bar{X}_i, \\quad s_{\\text{change}} = \\sqrt{s_{\\text{pre}}^2 + s_{\\text{post}}^2 - 2 r s_{\\text{pre}}s_{\\text{post}}}, \\quad d = \\frac{\\Delta\\text{Gain}}{s_{\\text{pooled}}}, \\quad g = J \\cdot d$$",
      "t_test" = "$$d = t \\sqrt{\\frac{n_1 + n_2}{n_1 n_2}}, \\quad J = 1 - \\frac{3}{4(n_1+n_2)-9}, \\quad g = J \\cdot d$$",
      "f_test" = "$$|t| = \\sqrt{F}, \\quad d = \\pm \\sqrt{F\\left(\\frac{n_1 + n_2}{n_1 n_2}\\right)}, \\quad g = J \\cdot d$$",
      "p_val" = "$$t = F_{t, df}^{-1}(1 - p/2), \\quad d = \\pm t \\sqrt{\\frac{n_1 + n_2}{n_1 n_2}}, \\quad g = J \\cdot d$$",
      "table_2x2" = "$$OR = \\frac{a \\cdot d}{b \\cdot c}, \\quad d = \\ln(OR) \\times \\frac{\\sqrt{3}}{\\pi} \\approx 0.5513 \\ln(OR), \\quad g = J \\cdot d$$",
      "correlation" = "$$d = \\frac{2r}{\\sqrt{1 - r^2}}, \\quad z = \\frac{1}{2}\\ln\\left(\\frac{1+r}{1-r}\\right), \\quad g = J \\cdot d$$",
      "$$d = \\dots, \\quad g = J \\cdot d$$"
    )
    div(
      class = "kawe-formula-step mt-3",
      div(
        class = "d-flex justify-content-between align-items-center mb-1",
        strong(class = "text-primary", icon("square-root-alt"), " Rumus:"),
        span(class = "badge bg-secondary", metric_lbl)
      ),
      div(class = "kawe-eq-box my-1", HTML(eq_str)),
      div(class = "small text-muted", "Formulasi lengkap untuk seluruh variasi metodologis luaran klinis dapat dipelajari pada tab Kumpulan Rumus dan Panduan Perhitungan.")
    )
  })

  # Navigation Observers inside calc_results_navset
  observeEvent(input$btn_view_calc_compendium, {
    nav_select("calc_results_navset", "panel_formula_catalog")
  })
  observeEvent(input$btn_back_to_calc_results2, {
    nav_select("calc_results_navset", "panel_calc_results")
  })

  # Dynamic Active Formula Display
  output$calc_active_formula_display <- renderUI({
    res <- calc_result()
    build_active_formula_ui(input$input_type, input$es_metric, input, res)
  })
  
  output$calc_saved_count_badge <- renderUI({
    df <- calc_compiled_data()
    span(class = "badge bg-primary", paste(nrow(df), "Studi Tersimpan"))
  })
  
  output$table_calc_recent_preview <- renderTable({
    df <- calc_compiled_data()
    if (nrow(df) == 0) {
      return(data.frame(Status = "Belum ada studi yang disimpan pada sesi kalkulator ini."))
    }
    tail(df[, c("Study", "Year", "Intervention", "Control_Type", "yi", "sei")], 5)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Tab 3: Gabungan Hasil Kalkulator Server Handlers
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
  
  output$calc_compiled_status_alert <- renderUI({
    df <- calc_compiled_data()
    act <- active_data()
    is_active <- nrow(df) > 0 && identical(df$Study, act$Study)
    if (is_active) {
      div(
        class = "alert alert-success border my-2 py-2 d-flex align-items-center",
        icon("check-circle", class = "me-2 fs-5"),
        span("Matriks kalkulator ini saat ini ", strong("SEDANG AKTIF DIGUNAKAN"), " sebagai data analisis utama pada modul Meta-Analisis, Subgroup, dan Forest Plot.")
      )
    } else {
      div(
        class = "alert alert-light border my-2 py-2 d-flex align-items-center justify-content-between",
        div(
          icon("info-circle", class = "text-primary me-2 fs-5"),
          span("Studi hasil kalkulator terkumpul di sini. Klik ", strong("Jadikan Dataset Analisis Utama"), " untuk menganalisis batch ini secara langsung.")
        )
      )
    }
  })
  
  observeEvent(input$btn_use_calc_as_active, {
    df <- calc_compiled_data()
    if (nrow(df) == 0) {
      showNotification("Matriks kalkulator masih kosong. Silakan hitung studi terlebih dahulu.", type = "warning")
      return()
    }
    active_data(df)
    active_source_label(paste0("Kompilasi Kalkulator (k = ", nrow(df), ")"))
    updateRadioButtons(session, "dataset_source_choice", selected = "calculator")
    showNotification("Matriks kalkulator berhasil ditetapkan sebagai dataset analisis utama.", type = "message")
  })
  
  observeEvent(input$btn_append_calc_to_active, {
    df <- calc_compiled_data()
    if (nrow(df) == 0) {
      showNotification("Matriks kalkulator masih kosong.", type = "warning")
      return()
    }
    curr <- active_data()
    # avoid exact study name duplicates
    new_studies <- df[!(df$Study %in% curr$Study), ]
    if (nrow(new_studies) == 0) {
      showNotification("Semua studi dalam matriks kalkulator sudah ada dalam dataset aktif.", type = "warning")
      return()
    }
    combined <- rbind(curr, new_studies)
    active_data(combined)
    active_source_label(paste0("Dataset Gabungan (k = ", nrow(combined), ")"))
    showNotification(paste("Berhasil menambahkan", nrow(new_studies), "studi baru ke dataset aktif."), type = "message")
  })
  
  observeEvent(input$btn_load_calc_sample, {
    calc_compiled_data(create_calc_sample_batch())
    showNotification("Contoh batch studi kalkulator (k = 5) berhasil dimuat.", type = "message")
  })
  
  observeEvent(input$btn_delete_calc_row, {
    sel <- input$table_calc_compiled_rows_selected
    df <- calc_compiled_data()
    if (is.null(sel) || length(sel) == 0 || nrow(df) == 0) {
      showNotification("Pilih satu atau beberapa baris pada tabel terlebih dahulu untuk dihapus.", type = "warning")
      return()
    }
    calc_compiled_data(df[-sel, , drop = FALSE])
    showNotification(paste("Berhasil menghapus", length(sel), "studi dari matriks kompilasi."), type = "message")
  })
  
  observeEvent(input$btn_clear_calc_compiled, {
    calc_compiled_data(data.frame())
    showNotification("Matriks kompilasi kalkulator telah dikosongkan.", type = "warning")
  })
  
  output$btn_download_calc_csv <- downloadHandler(
    filename = function() paste0("Compiled_Calculator_Effects_", Sys.Date(), ".csv"),
    content = function(file) write.csv(calc_compiled_data(), file, row.names = FALSE)
  )
  
  output$table_calc_compiled <- renderDT({
    df <- calc_compiled_data()
    if (nrow(df) == 0) return(NULL)
    inv_var <- 1 / df$vi
    df$Bobot_Persen <- round((inv_var / sum(inv_var)) * 100, 2)
    datatable(
      df,
      selection = "multiple",
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE),
      rownames = FALSE,
      class = "display compact cell-border"
    ) %>%
      formatRound(columns = c("yi", "vi", "sei"), digits = 4) %>%
      formatRound(columns = "Bobot_Persen", digits = 2)
  })
  
  # Tab 4: Dataset Sintesis Bukti Server Handlers
  observeEvent(input$dataset_source_choice, {
    src <- input$dataset_source_choice
    if (src == "benchmark") {
      active_data(benchmark_storage())
      active_source_label("Benchmark RCT (k = 14)")
    } else if (src == "calculator") {
      df_calc <- calc_compiled_data()
      if (nrow(df_calc) > 0) {
        active_data(df_calc)
        active_source_label(paste0("Kompilasi Kalkulator (k = ", nrow(df_calc), ")"))
      } else {
        showNotification("Matriks kompilasi kalkulator masih kosong. Menggunakan data aktif sebelumnya.", type = "warning")
      }
    }
  })
  
  observeEvent(input$btn_load_benchmark, {
    active_data(create_benchmark_data())
    benchmark_storage(create_benchmark_data())
    active_source_label("Benchmark RCT (k = 14)")
    updateRadioButtons(session, "dataset_source_choice", selected = "benchmark")
    updateSelectInput(session, "quick_sensitivity_filter", selected = "all")
    showNotification("Data benchmark intervensi klinis (k = 14) berhasil dimuat.", type = "message")
  })
  
  observeEvent(input$btn_delete_active_row, {
    sel <- input$table_active_dataset_rows_selected
    df_flt <- filtered_active_data()
    df_all <- active_data()
    if (is.null(sel) || length(sel) == 0 || nrow(df_flt) == 0) {
      showNotification("Pilih satu atau beberapa baris studi pada tabel untuk dihapus.", type = "warning")
      return()
    }
    studies_to_drop <- df_flt$Study[sel]
    df_new <- df_all[!(df_all$Study %in% studies_to_drop), , drop = FALSE]
    active_data(df_new)
    active_source_label(paste0("Dataset Kustom (k = ", nrow(df_new), ")"))
    showNotification(paste("Berhasil menghapus", length(studies_to_drop), "studi dari dataset aktif."), type = "message")
  })
  
  observeEvent(input$table_active_dataset_cell_edit, {
    info <- input$table_active_dataset_cell_edit
    df <- active_data()
    row_idx <- info$row
    col_idx <- info$col + 1 # rownames = FALSE in DT
    if (row_idx >= 1 && row_idx <= nrow(df) && col_idx >= 1 && col_idx <= ncol(df)) {
      val <- info$value
      if (is.numeric(df[[col_idx]])) {
        num_val <- suppressWarnings(as.numeric(val))
        if (!is.na(num_val)) {
          df[row_idx, col_idx] <- num_val
          if (colnames(df)[col_idx] == "vi" && num_val > 0) {
            df$sei[row_idx] <- round(sqrt(num_val), 4)
          } else if (colnames(df)[col_idx] == "sei" && num_val > 0) {
            df$vi[row_idx] <- round(num_val^2, 5)
          }
          active_data(df)
        }
      } else {
        df[row_idx, col_idx] <- as.character(val)
        active_data(df)
      }
    }
  })
  
  observeEvent(input$btn_clear_data, {
    empty_df <- data.frame(
      Study = character(0), Year = numeric(0), Intervention = character(0), Format = character(0),
      Control_Type = character(0), Active_Comparator = character(0), Population = character(0),
      Age_Group = character(0), Therapist_Level = character(0), Risk_of_Bias = character(0),
      Duration_Weeks = numeric(0), Total_Sessions = numeric(0), N_Intervention = numeric(0),
      N_Control = numeric(0), yi = numeric(0), vi = numeric(0), sei = numeric(0), stringsAsFactors = FALSE
    )
    active_data(empty_df)
    active_source_label("Kosong (k = 0)")
    showNotification("Dataset analisis aktif telah direset.", type = "warning")
  })
  
  observeEvent(input$upload_file, {
    req(input$upload_file)
    file_path <- input$upload_file$datapath
    ext <- tools::file_ext(file_path)
    
    tryCatch({
      if (ext == "csv") {
        df_uploaded <- read.csv(file_path, stringsAsFactors = FALSE)
      } else if (ext %in% c("xlsx", "xls")) {
        if (!requireNamespace("readxl", quietly = TRUE)) stop("Paket 'readxl' diperlukan untuk membaca Excel.")
        df_uploaded <- as.data.frame(readxl::read_excel(file_path))
      } else {
        stop("Format berkas tidak didukung. Harap unggah berkas .csv atau .xlsx.")
      }
      
      colnames_lower <- tolower(colnames(df_uploaded))
      yi_col <- which(colnames_lower %in% c("yi", "es", "hedges_g", "cohen_d", "effectsize", "g", "d"))[1]
      if (is.na(yi_col)) stop("Kolom besaran efek (yi / es / g) tidak ditemukan.")
      df_uploaded$yi <- as.numeric(df_uploaded[[yi_col]])
      
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
      
      study_col <- which(colnames_lower %in% c("study", "author", "author_year", "peneliti", "label"))[1]
      if (!is.na(study_col)) {
        df_uploaded$Study <- as.character(df_uploaded[[study_col]])
      } else {
        df_uploaded$Study <- paste("Study", seq_len(nrow(df_uploaded)))
      }
      
      df_valid <- df_uploaded[!is.na(df_uploaded$yi) & !is.na(df_uploaded$vi) & df_uploaded$vi > 0, ]
      if (nrow(df_valid) == 0) stop("Tidak ditemukan baris data yang valid (yi numerik dan vi > 0).")
      
      active_data(df_valid)
      active_source_label(paste0("Berkas Unggahan (k = ", nrow(df_valid), ")"))
      updateSelectInput(session, "quick_sensitivity_filter", selected = "all")
      showNotification(paste("Berhasil memuat", nrow(df_valid), "studi dari berkas kustom."), type = "message")
    }, error = function(e) {
      showNotification(paste("Gagal memuat berkas:", e$message), type = "error")
    })
  })
  
  output$table_active_dataset <- renderDT({
    df <- filtered_active_data()
    if (nrow(df) == 0) return(NULL)
    inv_var <- 1 / df$vi
    df$Bobot_Persen <- round((inv_var / sum(inv_var)) * 100, 2)
    datatable(
      df,
      editable = TRUE,
      selection = "multiple",
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE),
      rownames = FALSE,
      class = "display compact cell-border"
    ) %>%
      formatRound(columns = c("yi", "vi", "sei"), digits = 4) %>%
      formatRound(columns = "Bobot_Persen", digits = 2)
  })
  
  output$btn_download_csv <- downloadHandler(
    filename = function() paste0("Intervention_Meta_Dataset_", Sys.Date(), ".csv"),
    content = function(file) write.csv(filtered_active_data(), file, row.names = FALSE)
  )
  
  output$btn_download_template_csv <- downloadHandler(
    filename = function() "Template_Meta_Analysis_KAWE.csv",
    content = function(file) write.csv(head(create_benchmark_data(), 4), file, row.names = FALSE)
  )
  
  # Dynamic Covariates & Subgroup Dropdowns
  observe({
    df <- filtered_active_data()
    if (nrow(df) > 0) {
      num_cols <- names(df)[sapply(df, is.numeric)]
      valid_covars <- setdiff(num_cols, c("yi", "vi", "sei", "Year", "N_Intervention", "N_Control"))
      if (length(valid_covars) > 0) {
        choices <- c("Tanpa Kovariat Kontinu" = "none", valid_covars)
      } else {
        choices <- c("Tanpa Kovariat Kontinu" = "none")
      }
      updateSelectInput(session, "meta_continuous_covar", choices = choices)
      
      # Categorical columns for Subgroup & Multilevel Cluster
      cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
      cat_cols_sub <- setdiff(cat_cols, c("Study", "Active_Comparator"))
      if (length(cat_cols_sub) > 0) {
        label_map <- c(
          "Control_Type" = "Tipe Kelompok Kontrol (Control_Type)",
          "Format" = "Format Penghantaran (Format)",
          "Intervention" = "Pendekatan Terapi (Intervention)",
          "Risk_of_Bias" = "Risiko Bias (Risk_of_Bias)",
          "Population" = "Populasi Sasaran (Population)",
          "Therapist_Level" = "Kualifikasi Terapis (Therapist_Level)",
          "Age_Group" = "Kelompok Usia (Age_Group)"
        )
        sub_choices <- setNames(cat_cols_sub, ifelse(cat_cols_sub %in% names(label_map), label_map[cat_cols_sub], cat_cols_sub))
        curr_sub <- isolate(input$subgroup_var_select)
        sel_sub <- if (!is.null(curr_sub) && curr_sub %in% cat_cols_sub) curr_sub else cat_cols_sub[1]
        updateSelectInput(session, "subgroup_var_select", choices = sub_choices, selected = sel_sub)
      }
    }
  })
  
  # Reactive Meta-Analysis Model (Supports Standard z and Hartung-Knapp knha)
  meta_model <- reactive({
    df <- filtered_active_data()
    req(nrow(df) >= 2)
    method <- input$meta_estimator
    test_type <- ifelse(is.null(input$meta_test_type) || method == "FE", "z", input$meta_test_type)
    conf_level <- as.numeric(input$conf_level)
    covar <- input$meta_continuous_covar
    
    if (!is.null(covar) && covar != "none" && covar %in% colnames(df)) {
      form <- as.formula(paste("~", covar))
      res <- rma(yi = yi, vi = vi, mods = form, data = df, method = method, test = test_type, level = conf_level * 100)
    } else {
      res <- rma(yi = yi, vi = vi, data = df, method = method, test = test_type, level = conf_level * 100)
    }
    res
  })
  
  # Unconditional Pooled Model (for Clinical Translation, Sensitivity, and Forest/Funnel)
  pooled_only_model <- reactive({
    df <- filtered_active_data()
    req(nrow(df) >= 2)
    method <- input$meta_estimator
    test_type <- ifelse(is.null(input$meta_test_type) || method == "FE", "z", input$meta_test_type)
    conf_level <- as.numeric(input$conf_level)
    rma(yi = yi, vi = vi, data = df, method = method, test = test_type, level = conf_level * 100)
  })
  
  # Tab 5: Value Boxes
  output$vb_pooled_es <- renderText({
    res <- tryCatch(meta_model(), error = function(e) NULL)
    if (is.null(res)) return("-")
    sprintf("%.3f [%.3f, %.3f]", res$b[1], res$ci.lb[1], res$ci.ub[1])
  })
  output$vb_hetero_i2 <- renderText({
    res <- tryCatch(meta_model(), error = function(e) NULL)
    if (is.null(res)) return("-")
    if (input$meta_estimator == "FE") return("N/A (FE)")
    sprintf("%.1f%% (Q = %.2f, p = %.4f)", res$I2, res$QE, res$QEp)
  })
  output$vb_hetero_tau2 <- renderText({
    res <- tryCatch(meta_model(), error = function(e) NULL)
    if (is.null(res)) return("-")
    if (input$meta_estimator == "FE") return("0 (FE)")
    sprintf("%.4f (tau = %.3f)", res$tau2, sqrt(res$tau2))
  })
  output$vb_pred_interval <- renderText({
    res <- tryCatch(meta_model(), error = function(e) NULL)
    if (is.null(res) || input$meta_estimator == "FE") return("N/A (FE Model)")
    pred <- predict(res)
    sprintf("[%.3f, %.3f]", pred$pi.lb, pred$pi.ub)
  })
  
  output$meta_model_summary <- renderPrint({
    res <- tryCatch(meta_model(), error = function(e) e)
    if (inherits(res, "error")) {
      cat("Gagal mengestimasi model meta-analisis:\n", res$message)
    } else {
      summary(res)
    }
  })
  
  output$meta_continuous_covar_ui <- renderUI({
    covar <- input$meta_continuous_covar
    df <- filtered_active_data()
    if (is.null(covar) || covar == "none" || !covar %in% colnames(df)) {
      return(
        div(
          class = "alert alert-light border my-2",
          h6(class = "fw-bold text-primary", icon("info-circle"), " Meta-Regresi Kontinu"),
          p("Pilih variabel kontinu (misal: ", tags$strong("Duration_Weeks"), " atau ", tags$strong("Total_Sessions"), ") pada bilah samping untuk memodelkan hubungan linear kovariat dengan besaran efek intervensi.")
        )
      )
    }
    
    tagList(
      plotOutput("meta_regplot_display", height = "420px"),
      hr(),
      verbatimTextOutput("meta_covar_print")
    )
  })
  
  output$meta_regplot_display <- renderPlot({
    df <- filtered_active_data()
    covar <- input$meta_continuous_covar
    req(covar != "none", covar %in% colnames(df))
    res <- tryCatch(meta_model(), error = function(e) NULL)
    req(!is.null(res))
    
    x_var <- df[[covar]]
    weights <- 1 / res$vi
    cex_scaled <- 1.0 + 3.0 * (weights - min(weights)) / (max(weights) - min(weights) + 1e-5)
    x_seq <- seq(min(x_var, na.rm = TRUE), max(x_var, na.rm = TRUE), length.out = 100)
    preds <- predict(res, newmods = x_seq)
    
    par(mar = c(5, 5, 3, 2), bg = "#FFFFFF")
    plot(
      x_var, res$yi,
      pch = 21,
      bg = rgb(101/255, 9/255, 21/255, 0.65),
      col = "#650915",
      cex = cex_scaled,
      xlab = paste("Kovariat Kontinu:", covar),
      ylab = "Besaran Efek Intervensi (Hedges' g)",
      main = paste("Meta-Regression Bubble Plot (Ukuran Titik = Presisi Studi 1/vi)"),
      las = 1,
      bty = "l",
      ylim = c(min(c(res$yi, preds$ci.lb)) - 0.1, max(c(res$yi, preds$ci.ub)) + 0.1)
    )
    polygon(c(x_seq, rev(x_seq)), c(preds$ci.lb, rev(preds$ci.ub)), col = rgb(197/255, 160/255, 89/255, 0.25), border = NA)
    lines(x_seq, preds$pred, col = "#650915", lwd = 2.5)
    grid(nx = NULL, ny = NULL, lty = 2, col = "#E5D3B3")
    points(x_var, res$yi, pch = 21, bg = rgb(101/255, 9/255, 21/255, 0.65), col = "#650915", cex = cex_scaled)
    abline(h = 0, lty = 3, col = "gray40")
  })
  
  output$meta_covar_print <- renderPrint({
    res <- tryCatch(meta_model(), error = function(e) e)
    if (inherits(res, "error")) cat("Gagal menjalankan meta-regresi:\n", res$message) else print(res)
  })
  
  # Baujat Plot & Leave-One-Out Sensitivity Analysis
  output$meta_baujat_plot <- renderPlot({
    df <- filtered_active_data()
    req(nrow(df) >= 3)
    res <- tryCatch(rma(yi = yi, vi = vi, slab = df$Study, data = df, method = input$meta_estimator), error = function(e) NULL)
    req(!is.null(res))
    par(mar = c(4.5, 4.5, 2.5, 1.5), bg = "#FFFFFF")
    baujat(
      res,
      xlab = "Kontribusi terhadap Heterogenitas Cochran Q",
      ylab = "Pengaruh terhadap Estimasi Gabungan (Squared Standardized Diff)",
      col = "#650915",
      bg = rgb(197/255, 160/255, 89/255, 0.45),
      bty = "l"
    )
    title(main = "Baujat Diagnostic Plot", col.main = "#650915", cex.main = 0.95)
  })
  
  output$table_leave1out <- renderTable({
    df <- filtered_active_data()
    req(nrow(df) >= 3)
    res <- tryCatch(rma(yi = yi, vi = vi, slab = df$Study, data = df, method = input$meta_estimator), error = function(e) NULL)
    req(!is.null(res))
    l1o <- tryCatch(leave1out(res), error = function(e) NULL)
    req(!is.null(l1o))
    data.frame(
      `Studi Dikecualikan` = l1o$slab,
      `Pooled g` = sprintf("%.3f", l1o$estimate),
      `95% CI` = sprintf("[%.3f, %.3f]", l1o$ci.lb, l1o$ci.ub),
      `p-value` = sprintf("%.4f", l1o$pval),
      `I^2 (%)` = sprintf("%.1f%%", l1o$I2),
      `tau^2` = sprintf("%.4f", l1o$tau2),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Multilevel Three-Level Meta-Analysis (rma.mv)
  multilevel_fit <- reactive({
    df <- filtered_active_data()
    req(nrow(df) >= 3)
    clust_col <- input$ml_cluster_var
    if (is.null(clust_col) || !(clust_col %in% colnames(df))) clust_col <- "Study"
    df$Cluster_ID <- as.factor(df[[clust_col]])
    df$Effect_ID <- seq_len(nrow(df))
    method_mv <- ifelse(input$meta_estimator %in% c("REML", "ML"), input$meta_estimator, "REML")
    test_mv <- ifelse(!is.null(input$meta_test_type) && input$meta_test_type == "knha", "t", "z")
    res_mv <- tryCatch(
      rma.mv(yi = yi, V = vi, random = ~ 1 | Cluster_ID / Effect_ID, data = df, method = method_mv, test = test_mv),
      error = function(e) NULL
    )
    if (is.null(res_mv)) return(NULL)
    # Cheung (2014) typical sampling variance
    wi <- 1 / df$vi
    k <- nrow(df)
    v_typ <- ((k - 1) * sum(wi)) / (sum(wi)^2 - sum(wi^2))
    s2_between <- res_mv$sigma2[1] # Level 3 (Between-Cluster)
    s2_within <- res_mv$sigma2[2]  # Level 2 (Within-Cluster)
    total_var <- v_typ + s2_within + s2_between
    i2_level1 <- (v_typ / total_var) * 100
    i2_level2 <- (s2_within / total_var) * 100
    i2_level3 <- (s2_between / total_var) * 100
    list(
      model = res_mv,
      clust_col = clust_col,
      n_clusters = length(unique(df$Cluster_ID)),
      v_typ = v_typ,
      s2_within = s2_within,
      s2_between = s2_between,
      i2_level1 = i2_level1,
      i2_level2 = i2_level2,
      i2_level3 = i2_level3
    )
  })
  
  output$table_multilevel_decomp <- renderTable({
    ml <- multilevel_fit()
    if (is.null(ml)) return(data.frame(Status = "Model multilevel tiga aras belum dapat diestimasi."))
    data.frame(
      `Aras Varians (Multilevel Level)` = c(
        "Level 1: Kesalahan Sampling (Sampling Error)",
        paste0("Level 2: Varians Dalam-Klaster (Within-", ml$clust_col, ")"),
        paste0("Level 3: Varians Antar-Klaster (Between-", ml$clust_col, ")"),
        "Total Heterogenitas Sejati (I^2 Level 2 + Level 3)"
      ),
      `Komponen Varians (sigma^2)` = c(
        sprintf("%.4f (Typical v)", ml$v_typ),
        sprintf("%.4f", ml$s2_within),
        sprintf("%.4f", ml$s2_between),
        sprintf("%.4f", ml$s2_within + ml$s2_between)
      ),
      `Proporsi Varians (%)` = c(
        sprintf("%.1f%%", ml$i2_level1),
        sprintf("%.1f%%", ml$i2_level2),
        sprintf("%.1f%%", ml$i2_level3),
        sprintf("%.1f%%", ml$i2_level2 + ml$i2_level3)
      ),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$multilevel_model_summary <- renderPrint({
    ml <- multilevel_fit()
    if (is.null(ml)) cat("Gagal mengestimasi rma.mv.") else summary(ml$model)
  })
  
  output$multilevel_variance_plot <- renderPlot({
    ml <- multilevel_fit()
    req(!is.null(ml))
    vals <- c(ml$i2_level1, ml$i2_level2, ml$i2_level3)
    labs <- c(
      sprintf("Level 1 (Sampling)\n%.1f%%", ml$i2_level1),
      sprintf("Level 2 (Within)\n%.1f%%", ml$i2_level2),
      sprintf("Level 3 (Between)\n%.1f%%", ml$i2_level3)
    )
    par(mar = c(4.5, 4.5, 2.5, 1.5), bg = "#FFFFFF")
    bp <- barplot(
      vals,
      names.arg = labs,
      ylim = c(0, 105),
      col = c("#E5D3B3", "#C5A059", "#650915"),
      border = "#650915",
      ylab = "Proporsi Varians Total (%)",
      main = paste0("Dekomposisi Varians 3-Level (Klaster: ", ml$clust_col, ")"),
      col.main = "#650915",
      las = 1
    )
    grid(nx = NA, ny = NULL, lty = 2, col = "#E5D3B3")
    text(bp, vals + 4, sprintf("%.1f%%", vals), font = 2, col = "#231618", cex = 0.9)
  })
  
  # Tab 6: Analisis Subkelompok (Subgroup Analysis)
  subgroup_models <- reactive({
    df <- filtered_active_data()
    req(nrow(df) >= 2)
    grp_var <- input$subgroup_var_select
    req(grp_var %in% colnames(df))
    
    df_clean <- df[!is.na(df[[grp_var]]) & df[[grp_var]] != "", ]
    req(nrow(df_clean) >= 2)
    
    method <- input$meta_estimator
    test_type <- ifelse(is.null(input$meta_test_type) || method == "FE", "z", input$meta_test_type)
    res_all <- tryCatch(rma(yi = yi, vi = vi, data = df_clean, method = method, test = test_type), error = function(e) NULL)
    
    form <- as.formula(paste("~ factor(", grp_var, ")"))
    res_mod <- tryCatch(rma(yi = yi, vi = vi, mods = form, data = df_clean, method = method, test = test_type), error = function(e) NULL)
    
    list(data = df_clean, grp_var = grp_var, res_all = res_all, res_mod = res_mod, method = method, test_type = test_type)
  })
  
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
  
  # Helper: Publication-Grade meta::forest Style Renderer (Author, Year, g, SE | Standardised Mean Difference | SMD, 95%-CI, Weight)
  draw_meta_publication_forest <- function(df, res, show_pi = TRUE, show_weights = TRUE, cex_val = 0.95, method = "REML", test_type = "z") {
    k <- nrow(df)
    sei_vec <- if ("sei" %in% colnames(df)) df$sei else sqrt(df$vi)
    yr_vec <- if ("Year" %in% colnames(df)) as.character(df$Year) else rep("-", k)
    
    # In metafor, res$level is alpha (e.g. 0.05 for 95% CI)
    alpha_ci <- ifelse(!is.null(res$level) && res$level < 0.5, res$level, 0.05)
    ci_pct <- round((1 - alpha_ci) * 100)
    z_crit <- qnorm(1 - alpha_ci / 2)
    ci_lb <- df$yi - z_crit * sei_vec
    ci_ub <- df$yi + z_crit * sei_vec
    w_raw <- tryCatch(weights(res), error = function(e) (1 / df$vi) / sum(1 / df$vi) * 100)
    
    # Prediction Interval matching meta::metagen
    if (k >= 3 && method != "FE") {
      t_pi <- qt(1 - alpha_ci / 2, df = k - 2)
      se_pi <- sqrt(res$tau2)
      pi_lb <- res$b[1] - t_pi * se_pi
      pi_ub <- res$b[1] + t_pi * se_pi
    } else {
      pred <- tryCatch(predict(res), error = function(e) list(pi.lb = NA_real_, pi.ub = NA_real_))
      pi_lb <- pred$pi.lb
      pi_ub <- pred$pi.ub
    }
    
    has_pi <- isTRUE(show_pi) && method != "FE" && !is.na(pi_lb) && !is.na(pi_ub)
    
    core_x <- c(df$yi, res$ci.lb[1], res$ci.ub[1], 0)
    if (has_pi) core_x <- c(core_x, pi_lb, pi_ub)
    max_core <- max(1.0, max(abs(core_x), na.rm = TRUE))
    ticks <- pretty(c(-max_core, max_core), n = 6)
    x_lim <- range(ticks)
    
    map_x <- function(val) {
      0.460 + (val - x_lim[1]) / (x_lim[2] - x_lim[1]) * (0.695 - 0.460)
    }
    
    fmt_ci <- function(lb, ub) {
      s_lb <- ifelse(lb >= 0, sprintf(" %.2f", lb), sprintf("%.2f", lb))
      s_ub <- ifelse(ub >= 0, sprintf(" %.2f", ub), sprintf("%.2f", ub))
      paste0("[", s_lb, "; ", s_ub, "]")
    }
    
    # Row y-coordinates (from top to bottom)
    y_header_top <- k + 5.25
    y_header <- k + 4.45
    y_studies <- rev(seq_len(k)) + 3.0
    y_re <- 2.75
    y_pi <- if (has_pi) 1.85 else NA_real_
    y_het <- if (has_pi) 0.95 else 1.85
    y_axis <- if (has_pi) 0.95 else 1.65
    y_max <- k + 6.1
    
    par(mar = c(1.2, 0.4, 0.6, 0.4), bg = "#FFFFFF", family = "sans")
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(-0.4, y_max), xaxs = "i", yaxs = "i")
    
    # Column X positions
    x_auth <- 0.015
    x_year <- 0.255
    x_g    <- 0.340
    x_se   <- 0.420
    x_plot_mid <- (0.455 + 0.705) / 2
    x_smd  <- 0.775
    x_ci   <- if (isTRUE(show_weights)) 0.905 else 0.975
    x_wt   <- 0.985
    
    # Headers
    text(x_auth, y_header, "Author", adj = c(0, 0.5), font = 2, cex = cex_val)
    text(x_year, y_header, "Year", adj = c(1, 0.5), font = 2, cex = cex_val)
    text(x_g, y_header, "g", adj = c(1, 0.5), font = 2, cex = cex_val)
    text(x_se, y_header, "SE", adj = c(1, 0.5), font = 2, cex = cex_val)
    text(x_plot_mid, y_header_top, "Standardised Mean", adj = c(0.5, 0.5), font = 2, cex = cex_val)
    text(x_plot_mid, y_header, "Difference", adj = c(0.5, 0.5), font = 2, cex = cex_val)
    text(x_smd, y_header, "SMD", adj = c(1, 0.5), font = 2, cex = cex_val)
    text(x_ci, y_header, paste0(ci_pct, "%-CI"), adj = c(1, 0.5), font = 2, cex = cex_val)
    if (isTRUE(show_weights)) {
      text(x_wt, y_header, "Weight", adj = c(1, 0.5), font = 2, cex = cex_val)
    }
    
    # Reference vertical line at 0 and dotted pooled line
    x0_px <- map_x(0)
    xp_px <- map_x(res$b[1])
    segments(x0_px, y_axis, x0_px, max(y_studies) + 0.65, col = "black", lwd = 1.2)
    segments(xp_px, y_re + 0.32, xp_px, max(y_studies) + 0.65, col = "black", lty = 3, lwd = 1.1)
    
    # Study Rows
    w_sqrt <- sqrt(pmax(w_raw, 1))
    w_norm <- 0.14 + 0.22 * (w_sqrt - min(w_sqrt)) / (max(w_sqrt) - min(w_sqrt) + 1e-6)
    
    for (i in seq_len(k)) {
      yi_r <- y_studies[i]
      text(x_auth, yi_r, df$Study[i], adj = c(0, 0.5), cex = cex_val)
      text(x_year, yi_r, yr_vec[i], adj = c(1, 0.5), cex = cex_val)
      text(x_g, yi_r, sprintf("%.4f", df$yi[i]), adj = c(1, 0.5), cex = cex_val)
      text(x_se, yi_r, sprintf("%.4f", sei_vec[i]), adj = c(1, 0.5), cex = cex_val)
      
      # Square & CI in center panel (square first, then CI line & center tick)
      xc <- map_x(df$yi[i])
      xl <- map_x(ci_lb[i])
      xr <- map_x(ci_ub[i])
      sq_half_y <- w_norm[i]
      sq_half_x <- sq_half_y * 0.032
      
      rect(xc - sq_half_x, yi_r - sq_half_y, xc + sq_half_x, yi_r + sq_half_y, col = "#C4C4C4", border = "#B0B0B0")
      segments(xl, yi_r, xr, yi_r, col = "black", lwd = 1.25)
      segments(xc, yi_r - 0.09, xc, yi_r + 0.09, col = "black", lwd = 1.4)
      
      # Right columns
      text(x_smd, yi_r, sprintf("%.2f", df$yi[i]), adj = c(1, 0.5), cex = cex_val)
      text(x_ci, yi_r, fmt_ci(ci_lb[i], ci_ub[i]), adj = c(1, 0.5), cex = cex_val)
      if (isTRUE(show_weights)) {
        text(x_wt, yi_r, sprintf("%.1f%%", w_raw[i]), adj = c(1, 0.5), cex = cex_val)
      }
    }
    
    # Random / Fixed effects summary row
    re_label <- if (method == "FE") {
      "Common effect model"
    } else if (test_type == "knha") {
      "Random effects model (HK)"
    } else {
      paste0("Random effects model (", method, ")")
    }
    text(x_auth, y_re, re_label, adj = c(0, 0.5), font = 2, cex = cex_val)
    
    # Summary Diamond
    d_xc <- map_x(res$b[1])
    d_xl <- map_x(res$ci.lb[1])
    d_xr <- map_x(res$ci.ub[1])
    polygon(
      x = c(d_xl, d_xc, d_xr, d_xc),
      y = c(y_re, y_re + 0.30, y_re, y_re - 0.30),
      col = "#BEBEBE",
      border = "black",
      lwd = 1.3
    )
    
    text(x_smd, y_re, sprintf("%.2f", res$b[1]), adj = c(1, 0.5), font = 2, cex = cex_val)
    text(x_ci, y_re, fmt_ci(res$ci.lb[1], res$ci.ub[1]), adj = c(1, 0.5), font = 2, cex = cex_val)
    if (isTRUE(show_weights)) {
      text(x_wt, y_re, "100.0%", adj = c(1, 0.5), font = 2, cex = cex_val)
    }
    
    # Prediction Interval row (Red bar)
    if (has_pi) {
      text(x_auth, y_pi, "Prediction interval", adj = c(0, 0.5), font = 2, cex = cex_val)
      pi_xl <- map_x(pi_lb)
      pi_xr <- map_x(pi_ub)
      rect(pi_xl, y_pi - 0.11, pi_xr, y_pi + 0.11, col = "#EE0000", border = "black", lwd = 1.15)
      text(x_ci, y_pi, fmt_ci(pi_lb, pi_ub), adj = c(1, 0.5), font = 2, cex = cex_val)
    }
    
    # Heterogeneity footer (Cochran Q-based I^2 matching meta::metagen)
    q_val <- ifelse(is.null(res$QE) || is.na(res$QE), 0, res$QE)
    i2_q <- if (q_val > (k - 1) && q_val > 0) ((q_val - (k - 1)) / q_val) * 100 else 0
    tau2_val <- ifelse(is.null(res$tau2) || is.na(res$tau2), 0, res$tau2)
    p_het <- ifelse(is.null(res$QEp) || is.na(res$QEp), 1, res$QEp)
    het_expr <- bquote(
      paste(
        "Heterogeneity: ",
        italic(I)^2, " = ", .(sprintf("%.1f%%", i2_q)), ", ",
        tau^2, " = ", .(sprintf("%.4f", tau2_val)), ", ",
        italic(p), " = ", .(sprintf("%.4f", p_het))
      )
    )
    text(x_auth, y_het, het_expr, adj = c(0, 0.5), cex = cex_val * 0.92)
    
    # Bottom Axis under Center Plot
    ax_left <- map_x(x_lim[1])
    ax_right <- map_x(x_lim[2])
    segments(ax_left, y_axis, ax_right, y_axis, col = "black", lwd = 1.4)
    for (tk in ticks) {
      xtk <- map_x(tk)
      segments(xtk, y_axis, xtk, y_axis - 0.26, col = "black", lwd = 1.3)
      text(xtk, y_axis - 0.65, as.character(tk), adj = c(0.5, 0.5), cex = cex_val * 0.95)
    }
  }
  
  # Helper: Subgroup Forest Plot in meta::forest Publication Style
  draw_meta_subgroup_forest <- function(m, cex_val = 0.90) {
    df <- m$data
    grp_var <- m$grp_var
    method <- m$method
    test_type <- m$test_type
    res_all <- m$res_all
    res_mod <- m$res_mod
    
    ord_idx <- order(df[[grp_var]], df$yi, decreasing = FALSE)
    df <- df[ord_idx, ]
    cats <- unique(df[[grp_var]])
    
    sei_vec <- if ("sei" %in% colnames(df)) df$sei else sqrt(df$vi)
    yr_vec <- if ("Year" %in% colnames(df)) as.character(df$Year) else rep("-", nrow(df))
    ci_lb <- df$yi - 1.96 * sei_vec
    ci_ub <- df$yi + 1.96 * sei_vec
    w_raw <- tryCatch(weights(res_all), error = function(e) (1 / df$vi) / sum(1 / df$vi) * 100)
    k_all <- nrow(df)
    if (k_all >= 3 && method != "FE") {
      t_pi_all <- qt(0.975, df = k_all - 2)
      pi_lb_all <- res_all$b[1] - t_pi_all * sqrt(res_all$tau2)
      pi_ub_all <- res_all$b[1] + t_pi_all * sqrt(res_all$tau2)
    } else {
      pred_all <- tryCatch(predict(res_all), error = function(e) list(pi.lb = res_all$ci.lb[1], pi.ub = res_all$ci.ub[1]))
      pi_lb_all <- pred_all$pi.lb
      pi_ub_all <- pred_all$pi.ub
    }
    
    core_x <- c(df$yi, res_all$ci.lb[1], res_all$ci.ub[1], pi_lb_all, pi_ub_all, 0)
    max_abs <- max(1.0, min(4.0, max(abs(core_x), na.rm = TRUE)))
    ticks <- pretty(c(-max_abs, max_abs), n = 6)
    x_lim <- range(ticks)
    
    map_x <- function(val) {
      0.460 + (val - x_lim[1]) / (x_lim[2] - x_lim[1]) * (0.695 - 0.460)
    }
    fmt_ci <- function(lb, ub) {
      s_lb <- ifelse(lb >= 0, sprintf(" %.2f", lb), sprintf("%.2f", lb))
      s_ub <- ifelse(ub >= 0, sprintf(" %.2f", ub), sprintf("%.2f", ub))
      paste0("[", s_lb, "; ", s_ub, "]")
    }
    
    # Build row positions bottom-up
    row_cursor <- 4.2
    cat_het_rows <- list()
    cat_poly_rows <- list()
    cat_header_rows <- list()
    study_rows <- numeric(nrow(df))
    
    for (g_name in rev(cats)) {
      idx <- which(df[[grp_var]] == g_name)
      cat_het_rows[[g_name]] <- row_cursor
      row_cursor <- row_cursor + 0.85
      cat_poly_rows[[g_name]] <- row_cursor
      row_cursor <- row_cursor + 0.95
      for (i in rev(seq_along(idx))) {
        study_rows[idx[i]] <- row_cursor
        row_cursor <- row_cursor + 0.90
      }
      cat_header_rows[[g_name]] <- row_cursor
      row_cursor <- row_cursor + 1.35
    }
    
    y_header <- row_cursor + 0.4
    y_header_top <- y_header + 0.8
    y_re <- 2.85
    y_pi <- 2.00
    y_het <- 1.15
    y_qb <- 0.35
    y_axis <- 1.15
    
    par(mar = c(1.2, 0.4, 0.6, 0.4), bg = "#FFFFFF", family = "sans")
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(-0.8, y_header_top + 0.8), xaxs = "i", yaxs = "i")
    
    x_auth <- 0.015
    x_year <- 0.255
    x_g    <- 0.340
    x_se   <- 0.420
    x_plot_mid <- (0.455 + 0.705) / 2
    x_smd  <- 0.775
    x_ci   <- 0.905
    x_wt   <- 0.985
    
    text(x_auth, y_header, "Author", adj = c(0, 0.5), font = 2, cex = cex_val)
    text(x_year, y_header, "Year", adj = c(1, 0.5), font = 2, cex = cex_val)
    text(x_g, y_header, "g", adj = c(1, 0.5), font = 2, cex = cex_val)
    text(x_se, y_header, "SE", adj = c(1, 0.5), font = 2, cex = cex_val)
    text(x_plot_mid, y_header_top, "Standardised Mean", adj = c(0.5, 0.5), font = 2, cex = cex_val)
    text(x_plot_mid, y_header, "Difference", adj = c(0.5, 0.5), font = 2, cex = cex_val)
    text(x_smd, y_header, "SMD", adj = c(1, 0.5), font = 2, cex = cex_val)
    text(x_ci, y_header, "95%-CI", adj = c(1, 0.5), font = 2, cex = cex_val)
    text(x_wt, y_header, "Weight", adj = c(1, 0.5), font = 2, cex = cex_val)
    
    x0_px <- map_x(0)
    xp_px <- map_x(res_all$b[1])
    segments(x0_px, y_axis, x0_px, y_header - 0.5, col = "black", lwd = 1.2)
    segments(xp_px, y_re + 0.28, xp_px, y_header - 0.5, col = "black", lty = 3, lwd = 1.1)
    
    w_sqrt <- sqrt(pmax(w_raw, 1))
    w_norm <- 0.12 + 0.18 * (w_sqrt - min(w_sqrt)) / (max(w_sqrt) - min(w_sqrt) + 1e-6)
    
    for (i in seq_len(nrow(df))) {
      yi_r <- study_rows[i]
      text(x_auth, yi_r, df$Study[i], adj = c(0, 0.5), cex = cex_val)
      text(x_year, yi_r, yr_vec[i], adj = c(1, 0.5), cex = cex_val)
      text(x_g, yi_r, sprintf("%.4f", df$yi[i]), adj = c(1, 0.5), cex = cex_val)
      text(x_se, yi_r, sprintf("%.4f", sei_vec[i]), adj = c(1, 0.5), cex = cex_val)
      
      xc <- map_x(df$yi[i])
      xl <- map_x(ci_lb[i])
      xr <- map_x(ci_ub[i])
      sq_half_y <- w_norm[i]
      sq_half_x <- sq_half_y * 0.030
      rect(xc - sq_half_x, yi_r - sq_half_y, xc + sq_half_x, yi_r + sq_half_y, col = "#C4C4C4", border = "#B0B0B0")
      segments(xl, yi_r, xr, yi_r, col = "black", lwd = 1.2)
      segments(xc, yi_r - 0.08, xc, yi_r + 0.08, col = "black", lwd = 1.3)
      
      text(x_smd, yi_r, sprintf("%.2f", df$yi[i]), adj = c(1, 0.5), cex = cex_val)
      text(x_ci, yi_r, fmt_ci(ci_lb[i], ci_ub[i]), adj = c(1, 0.5), cex = cex_val)
      text(x_wt, yi_r, sprintf("%.1f%%", w_raw[i]), adj = c(1, 0.5), cex = cex_val)
    }
    
    for (g_name in cats) {
      idx <- which(df[[grp_var]] == g_name)
      text(x_auth, cat_header_rows[[g_name]], paste0(grp_var, " = ", g_name), adj = c(0, 0.5), font = 2, cex = cex_val)
      sub_res <- tryCatch(rma(df$yi[idx], df$vi[idx], method = method, test = test_type), error = function(e) NULL)
      if (!is.null(sub_res)) {
        yp <- cat_poly_rows[[g_name]]
        yh <- cat_het_rows[[g_name]]
        text(x_auth, yp, "Random effects model", adj = c(0, 0.5), font = 2, cex = cex_val)
        d_xc <- map_x(sub_res$b[1])
        d_xl <- map_x(sub_res$ci.lb[1])
        d_xr <- map_x(sub_res$ci.ub[1])
        polygon(c(d_xl, d_xc, d_xr, d_xc), c(yp, yp + 0.25, yp, yp - 0.25), col = "#BEBEBE", border = "black", lwd = 1.2)
        text(x_smd, yp, sprintf("%.2f", sub_res$b[1]), adj = c(1, 0.5), font = 2, cex = cex_val)
        text(x_ci, yp, fmt_ci(sub_res$ci.lb[1], sub_res$ci.ub[1]), adj = c(1, 0.5), font = 2, cex = cex_val)
        text(x_wt, yp, sprintf("%.1f%%", sum(w_raw[idx])), adj = c(1, 0.5), font = 2, cex = cex_val)
        
        sub_het <- bquote(
          paste(
            "Heterogeneity: ",
            italic(I)^2, " = ", .(sprintf("%.1f%%", ifelse(is.na(sub_res$I2), 0, sub_res$I2))), ", ",
            tau^2, " = ", .(sprintf("%.4f", ifelse(is.na(sub_res$tau2), 0, sub_res$tau2))), ", ",
            italic(p), " = ", .(sprintf("%.4f", ifelse(is.na(sub_res$QEp), 1, sub_res$QEp)))
          )
        )
        text(x_auth, yh, sub_het, adj = c(0, 0.5), cex = cex_val * 0.88)
      }
    }
    
    re_label <- ifelse(test_type == "knha", "Random effects model (HK)", paste0("Random effects model (", method, ")"))
    text(x_auth, y_re, re_label, adj = c(0, 0.5), font = 2, cex = cex_val)
    d_xc <- map_x(res_all$b[1])
    d_xl <- map_x(res_all$ci.lb[1])
    d_xr <- map_x(res_all$ci.ub[1])
    polygon(c(d_xl, d_xc, d_xr, d_xc), c(y_re, y_re + 0.28, y_re, y_re - 0.28), col = "#BEBEBE", border = "black", lwd = 1.3)
    text(x_smd, y_re, sprintf("%.2f", res_all$b[1]), adj = c(1, 0.5), font = 2, cex = cex_val)
    text(x_ci, y_re, fmt_ci(res_all$ci.lb[1], res_all$ci.ub[1]), adj = c(1, 0.5), font = 2, cex = cex_val)
    text(x_wt, y_re, "100.0%", adj = c(1, 0.5), font = 2, cex = cex_val)
    
    if (method != "FE" && !is.na(pi_lb_all)) {
      text(x_auth, y_pi, "Prediction interval", adj = c(0, 0.5), font = 2, cex = cex_val)
      rect(map_x(pi_lb_all), y_pi - 0.10, map_x(pi_ub_all), y_pi + 0.10, col = "#EE0000", border = "black", lwd = 1.1)
      text(x_ci, y_pi, fmt_ci(pi_lb_all, pi_ub_all), adj = c(1, 0.5), font = 2, cex = cex_val)
    }
    
    q_all <- ifelse(is.null(res_all$QE) || is.na(res_all$QE), 0, res_all$QE)
    i2_q_all <- if (q_all > (k_all - 1) && q_all > 0) ((q_all - (k_all - 1)) / q_all) * 100 else 0
    het_all <- bquote(
      paste(
        "Heterogeneity: ",
        italic(I)^2, " = ", .(sprintf("%.1f%%", i2_q_all)), ", ",
        tau^2, " = ", .(sprintf("%.4f", ifelse(is.na(res_all$tau2), 0, res_all$tau2))), ", ",
        italic(p), " = ", .(sprintf("%.4f", ifelse(is.na(res_all$QEp), 1, res_all$QEp)))
      )
    )
    text(x_auth, y_het, het_all, adj = c(0, 0.5), cex = cex_val * 0.88)
    
    if (!is.null(res_mod)) {
      qb_expr <- bquote(
        paste(
          "Test for subgroup differences: ",
          chi[.(res_mod$QMdf[1])]^2, " = ", .(sprintf("%.2f", res_mod$QM)), ", ",
          "df = ", .(res_mod$QMdf[1]), " (", italic(p), " = ", .(sprintf("%.4f", res_mod$QMp)), ")"
        )
      )
      text(x_auth, y_qb, qb_expr, adj = c(0, 0.5), cex = cex_val * 0.88)
    }
    
    ax_left <- map_x(x_lim[1])
    ax_right <- map_x(x_lim[2])
    segments(ax_left, y_axis, ax_right, y_axis, col = "black", lwd = 1.4)
    for (tk in ticks) {
      xtk <- map_x(tk)
      segments(xtk, y_axis, xtk, y_axis - 0.24, col = "black", lwd = 1.2)
      text(xtk, y_axis - 0.58, as.character(tk), adj = c(0.5, 0.5), cex = cex_val * 0.92)
    }
  }
  
  # Subgroup Forest Plot Internal Renderer
  render_subgroup_forest_internal <- function() {
    m <- subgroup_models()
    req(!is.null(m), !is.null(m$res_all))
    
    if (is.null(input$subgroup_forest_style) || input$subgroup_forest_style == "meta_style") {
      draw_meta_subgroup_forest(m, cex_val = input$subgroup_forest_cex)
      return(invisible(NULL))
    }
    
    df <- m$data
    grp_var <- m$grp_var
    method <- m$method
    
    ord_idx <- order(df[[grp_var]], df$yi, decreasing = FALSE)
    df <- df[ord_idx, ]
    cats <- unique(df[[grp_var]])
    
    row_cursor <- 3
    cat_poly_rows <- list()
    cat_header_rows <- list()
    study_rows <- numeric(nrow(df))
    
    for (g_name in rev(cats)) {
      idx <- which(df[[grp_var]] == g_name)
      cat_poly_rows[[g_name]] <- row_cursor
      row_cursor <- row_cursor + 1
      
      for (i in seq_along(idx)) {
        study_rows[idx[i]] <- row_cursor
        row_cursor <- row_cursor + 1
      }
      
      cat_header_rows[[g_name]] <- row_cursor
      row_cursor <- row_cursor + 2
    }
    
    top_row <- row_cursor
    
    if (grp_var == "Control_Type" && "Active_Comparator" %in% colnames(df)) {
      slabs <- paste0(df$Study, " (", df$Year, ") [", ifelse(df$Control_Type == "Active Control" & !is.na(df$Active_Comparator), df$Active_Comparator, df$Control_Type), "]")
    } else if ("Year" %in% colnames(df)) {
      slabs <- paste0(df$Study, " (", df$Year, ")")
    } else {
      slabs <- df$Study
    }
    
    cex_val <- input$subgroup_forest_cex
    
    par(mar = c(5, 5, 3, 2), bg = "#FFFFFF")
    forest(
      df$yi,
      df$vi,
      slab = slabs,
      rows = study_rows,
      ylim = c(-3.0, top_row + 1.5),
      xlab = "Besaran Efek Intervensi (Hedges' g)",
      header = c("Studi Primer & Subkelompok", "Hedges' g [95% CI]"),
      cex = cex_val,
      col = "#650915",
      refline = 0,
      alim = c(min(-0.5, min(df$yi - 1.96 * sqrt(df$vi)) - 0.2), max(1.5, max(df$yi + 1.96 * sqrt(df$vi)) + 0.2))
    )
    
    addpoly(
      m$res_all,
      row = 1,
      mlab = paste0("Total Pooled (", method, ")"),
      cex = cex_val,
      col = "#650915",
      border = "#650915"
    )
    
    for (g_name in cats) {
      idx <- which(df[[grp_var]] == g_name)
      k_cat <- length(idx)
      sub_res <- tryCatch(rma(df$yi[idx], df$vi[idx], method = method), error = function(e) NULL)
      if (!is.null(sub_res)) {
        addpoly(
          sub_res,
          row = cat_poly_rows[[g_name]],
          mlab = paste0("Subtotal: ", g_name, " (k = ", k_cat, ")"),
          cex = cex_val,
          col = "#C5A059",
          border = "#650915"
        )
      }
      
      text(
        par("usr")[1],
        cat_header_rows[[g_name]],
        paste0("Subkelompok: ", g_name, " (k = ", k_cat, ")"),
        font = 2,
        pos = 4,
        cex = cex_val,
        col = "#650915"
      )
    }
    
    if (!is.null(m$res_mod)) {
      text(
        par("usr")[1],
        -1.8,
        sprintf("Uji Beda Subkelompok (QB): QM = %.2f, df = %d, p = %.4f  |  Heterogenitas Residual: QE = %.2f, p = %.4f",
                m$res_mod$QM, m$res_mod$QMdf[1], m$res_mod$QMp, m$res_mod$QE, m$res_mod$QEp),
        pos = 4,
        cex = cex_val * 0.85,
        font = 3,
        col = "#5A4B4C"
      )
    }
  }
  
  output$subgroup_forest_plot_display <- renderPlot({
    render_subgroup_forest_internal()
  })
  
  output$btn_dl_subgroup_png <- downloadHandler(
    filename = function() paste0("Subgroup_Forest_Plot_", input$subgroup_var_select, "_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 3000, height = 2400, res = 300)
      render_subgroup_forest_internal()
      dev.off()
    }
  )
  
  output$btn_dl_subgroup_pdf <- downloadHandler(
    filename = function() paste0("Subgroup_Forest_Plot_", input$subgroup_var_select, "_", Sys.Date(), ".pdf"),
    content = function(file) {
      pdf(file, width = 11, height = 8.5)
      render_subgroup_forest_internal()
      dev.off()
    }
  )
  
  subgroup_summary_df <- reactive({
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
        `p-value` = sprintf("%.4f", sub_res$pval[1]),
        `I^2 (%)` = sprintf("%.1f%%", sub_res$I2),
        `tau^2` = sprintf("%.4f", sub_res$tau2),
        `Heterogenitas Qw` = sprintf("%.2f (df = %d, p = %.4f)", sub_res$QE, sub_res$QEdf, sub_res$QEp),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, res_list)
  })
  
  output$table_subgroup_comprehensive <- renderTable({
    subgroup_summary_df()
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$btn_dl_subgroup_csv <- downloadHandler(
    filename = function() paste0("Subgroup_Comparison_", input$subgroup_var_select, "_", Sys.Date(), ".csv"),
    content = function(file) write.csv(subgroup_summary_df(), file, row.names = FALSE)
  )
  
  observeEvent(input$btn_copy_sub_apa, {
    m <- tryCatch(subgroup_models(), error = function(e) NULL)
    if (is.null(m) || is.null(m$res_mod)) return()
    res_mod <- m$res_mod
    txt <- sprintf(
      "Analisis moderator subkelompok pada variabel %s menunjukkan statistik uji beda antar-subkelompok QB(df = %d) = %.2f, p = %.4f (R^2 = %.1f%%), dengan heterogenitas residual QE(df = %d) = %.2f, p = %.4f.",
      m$grp_var, res_mod$QMdf[1], res_mod$QM, res_mod$QMp,
      ifelse(!is.null(res_mod$R2) && !is.na(res_mod$R2), res_mod$R2, 0),
      res_mod$QEdf, res_mod$QE, res_mod$QEp
    )
    session$sendCustomMessage("copy_to_clipboard", list(text = txt))
    showNotification("Naratif subkelompok APA 7th berhasil disalin ke Clipboard.", type = "message")
  })
  
  output$subgroup_interpretation_narrative <- renderUI({
    m <- tryCatch(subgroup_models(), error = function(e) NULL)
    if (is.null(m) || is.null(m$res_mod)) return(p("Model subkelompok belum tersedia."))
    
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
      mag <- ifelse(abs(sub_res$b[1]) < 0.2, "dapat diabaikan", ifelse(abs(sub_res$b[1]) < 0.5, "kecil", ifelse(abs(sub_res$b[1]) < 0.8, "sedang", "besar")))
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
  
  # Tab 7: Forest Plot Internal Renderer
  render_forest_internal <- function() {
    df <- filtered_active_data()
    req(nrow(df) >= 2)
    
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
    
    test_type <- ifelse(is.null(input$meta_test_type) || input$meta_estimator == "FE", "z", input$meta_test_type)
    res <- rma(yi = yi, vi = vi, data = df, method = input$meta_estimator, test = test_type, level = as.numeric(input$conf_level) * 100)
    
    if (is.null(input$forest_style) || input$forest_style == "meta_style") {
      draw_meta_publication_forest(
        df = df,
        res = res,
        show_pi = input$forest_show_pi,
        show_weights = input$forest_show_weights,
        cex_val = input$forest_cex,
        method = input$meta_estimator,
        test_type = test_type
      )
      return(invisible(NULL))
    }
    
    if (input$forest_sort %in% c("format", "control", "interv", "rob")) {
      mod_tag <- switch(
        input$forest_sort,
        "format" = df$Format,
        "control" = ifelse(df$Control_Type == "Active Control" & "Active_Comparator" %in% colnames(df) & !is.na(df$Active_Comparator),
                           paste0("Active: ", df$Active_Comparator), df$Control_Type),
        "interv" = df$Intervention,
        "rob" = df$Risk_of_Bias
      )
      slabs <- paste0(df$Study, " (", df$Year, ") [", mod_tag, "]")
    } else if ("Year" %in% colnames(df)) {
      slabs <- paste0(df$Study, " (", df$Year, ")")
    } else {
      slabs <- df$Study
    }
    
    par(mar = c(5, 5, 3, 2), bg = "#FFFFFF")
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
      ylim = c(-3.0, nrow(df) + 3.5),
      alim = c(min(-0.5, min(df$yi - 1.96 * sqrt(df$vi)) - 0.2), max(1.5, max(df$yi + 1.96 * sqrt(df$vi)) + 0.2)),
      refline = 0
    )
    
    # Safe Axis labels positioned inside plot bounds
    usr <- par("usr")
    text(usr[1] + 0.1, -2.0, input$forest_label_left, pos = 4, cex = input$forest_cex * 0.85, font = 3, col = "#650915")
    text(usr[2] - 0.1, -2.0, input$forest_label_right, pos = 2, cex = input$forest_cex * 0.85, font = 3, col = "#650915")
  }
  
  output$forest_plot_display <- renderPlot({
    render_forest_internal()
  })
  
  output$btn_dl_forest_png <- downloadHandler(
    filename = function() paste0("Forest_Plot_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 2800, height = 2000, res = 300)
      render_forest_internal()
      dev.off()
    }
  )
  
  output$btn_dl_forest_pdf <- downloadHandler(
    filename = function() paste0("Forest_Plot_", Sys.Date(), ".pdf"),
    content = function(file) {
      pdf(file, width = 10, height = 7.5)
      render_forest_internal()
      dev.off()
    }
  )
  
  # Tab 8: Funnel Plot Internal Renderer
  render_funnel_internal <- function() {
    df <- filtered_active_data()
    req(nrow(df) >= 2)
    res <- rma(yi = yi, vi = vi, data = df, method = input$meta_estimator)
    
    par(mar = c(5, 5, 3, 2), bg = "#FFFFFF")
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
    } else if (input$funnel_type == "trimfill") {
      tf <- tryCatch(trimfill(res), error = function(e) res)
      funnel(
        tf,
        legend = TRUE,
        xlab = "Besaran Efek Intervensi (Hedges' g) — Imputasi Trim & Fill",
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
  
  output$funnel_plot_display <- renderPlot({
    render_funnel_internal()
  })
  
  output$btn_dl_funnel_png <- downloadHandler(
    filename = function() paste0("Funnel_Plot_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 2400, height = 1800, res = 300)
      render_funnel_internal()
      dev.off()
    }
  )
  
  output$btn_dl_funnel_pdf <- downloadHandler(
    filename = function() paste0("Funnel_Plot_", Sys.Date(), ".pdf"),
    content = function(file) {
      pdf(file, width = 8, height = 6)
      render_funnel_internal()
      dev.off()
    }
  )
  
  # Publication Bias Tables (Forced Computation at k.min = 5)
  output$table_egger_test <- renderTable({
    df <- filtered_active_data()
    req(nrow(df) >= 2)
    res <- tryCatch(rma(yi = yi, vi = vi, data = df, method = input$meta_estimator), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    eg_lm <- tryCatch(regtest(res, model = "lm", predictor = "sei"), error = function(e) NULL)
    eg_rma <- tryCatch(regtest(res, model = "rma", predictor = "sei"), error = function(e) NULL)
    eg <- if (!is.null(eg_lm)) eg_lm else eg_rma
    if (is.null(eg)) return(data.frame(Status = "Uji Egger tidak dapat dikomputasi (minimal dibutuhkan 3 studi)."))
    
    stat_label <- if (!is.null(eg$dfs) && !is.na(eg$dfs)) {
      sprintf("t = %.4f (df = %d)", eg$zval, eg$dfs)
    } else {
      sprintf("z = %.4f", eg$zval)
    }
    
    data.frame(
      Metrik = c(
        "Statistik Regresi Linier Egger (k.min = 5)",
        "Nilai p (p-value)",
        "Limit Estimate (Intersep b0 saat SE -> 0)",
        "Batas 95% CI Limit Estimate",
        "Kesimpulan Bias Publikasi"
      ),
      Nilai = c(
        stat_label,
        sprintf("%.4f", eg$pval),
        sprintf("%.4f", eg$est),
        sprintf("[%.4f, %.4f]", eg$ci.lb, eg$ci.ub),
        ifelse(eg$pval < 0.05, "Terindikasi Asimetri Bermakna (Potensi Bias Publikasi)", "Tidak Terindikasi Asimetri Bermakna (Simetris)")
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$table_begg_test <- renderTable({
    df <- filtered_active_data()
    req(nrow(df) >= 2)
    res <- tryCatch(rma(yi = yi, vi = vi, data = df, method = input$meta_estimator), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    bg <- tryCatch(ranktest(res), error = function(e) NULL)
    if (is.null(bg)) return(data.frame(Status = "Uji Begg tidak dapat dikomputasi."))
    
    data.frame(
      Metrik = c("Statistik Kendall's Tau", "Nilai p (p-value)", "Kesimpulan"),
      Nilai = c(
        sprintf("%.4f", bg$tau),
        sprintf("%.4f", bg$pval),
        ifelse(bg$pval < 0.05, "Terindikasi Korelasi Asimetri Signifikan", "Korelasi Peringkat Tidak Signifikan")
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$table_trimfill_summary <- renderTable({
    df <- filtered_active_data()
    req(nrow(df) >= 2)
    res <- tryCatch(rma(yi = yi, vi = vi, data = df, method = input$meta_estimator), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    tf <- tryCatch(trimfill(res), error = function(e) NULL)
    if (is.null(tf)) return(data.frame(Status = "Analisis Trim-and-Fill tidak dapat dijalankan."))
    
    data.frame(
      Parameter = c(
        "Estimasi Studi Hilang (k0)",
        "Sisi Asimetri Imputasi",
        "Pooled Effect Asli (Sebelum Imputasi)",
        "Adjusted Pooled Effect (Setelah Imputasi)",
        "Pergeseran Besaran Efek"
      ),
      Nilai = c(
        as.character(tf$k0),
        tf$side,
        sprintf("%.4f [%.4f, %.4f]", res$b[1], res$ci.lb[1], res$ci.ub[1]),
        sprintf("%.4f [%.4f, %.4f]", tf$b[1], tf$ci.lb[1], tf$ci.ub[1]),
        sprintf("%.4f (Atenuasi Efek)", tf$b[1] - res$b[1])
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$table_failsafe_n <- renderTable({
    df <- filtered_active_data()
    req(nrow(df) >= 2)
    k_min_val <- ifelse(is.null(input$bias_k_min), 5, as.numeric(input$bias_k_min))
    target_orw <- ifelse(is.null(input$fsn_orwin_target), 0.20, as.numeric(input$fsn_orwin_target))
    
    # Compatible with both metafor >= 5.0 (x = ...) and older metafor (yi = ...)
    run_fsn <- function(type_name, target_val = NULL) {
      tryCatch({
        if (is.null(target_val)) {
          fsn(x = df$yi, vi = df$vi, type = type_name, alpha = 0.05)
        } else {
          fsn(x = df$yi, vi = df$vi, type = type_name, alpha = 0.05, target = target_val)
        }
      }, error = function(e) {
        tryCatch({
          if (is.null(target_val)) {
            fsn(yi = df$yi, vi = df$vi, type = type_name, alpha = 0.05)
          } else {
            fsn(yi = df$yi, vi = df$vi, type = type_name, alpha = 0.05, target = target_val)
          }
        }, error = function(e2) NULL)
      })
    }
    
    fs_ros <- run_fsn("Rosenthal")
    fs_orw <- run_fsn("Orwin", target_val = target_orw)
    fs_rsb <- run_fsn("Rosenberg")
    
    if (is.null(fs_ros)) return(data.frame(Status = "Fail-Safe N tidak dapat dikomputasi."))
    threshold_ros <- 5 * nrow(df) + 10
    
    data.frame(
      `Metode & Parameter (k.min = 5)` = c(
        sprintf("Status Komputasi Paksa (k = %d, k.min = %d)", nrow(df), k_min_val),
        "Rosenthal Fail-Safe N (Target alpha = 0.05)",
        "Signifikansi Observasi Rosenthal (Observed p)",
        "Ambang Ketahanan Rosenthal (5k + 10)",
        sprintf("Orwin Fail-Safe N (Target g = %.2f)", target_orw),
        "Rata-Rata Efek Observasi Orwin (Average ES)",
        "Rosenberg Fail-Safe N (Weighted, alpha = 0.05)",
        "Signifikansi Observasi Rosenberg (Observed p)",
        "Kesimpulan Ketahanan Bukti (File-Drawer)"
      ),
      `Nilai Estimasi` = c(
        ifelse(nrow(df) >= k_min_val, sprintf("Aktif & Terpenuhi (k = %d >= %d)", nrow(df), k_min_val), sprintf("Dipaksa Aktif (k = %d < %d)", nrow(df), k_min_val)),
        paste(fs_ros$fsnum, "Studi"),
        sprintf("p = %.4f (Target alpha = %.2f)", fs_ros$pval, fs_ros$alpha),
        paste(threshold_ros, "Studi"),
        if (!is.null(fs_orw)) paste(fs_orw$fsnum, "Studi") else "-",
        if (!is.null(fs_orw)) sprintf("g = %.4f (Target = %.4f)", fs_orw$meanes, fs_orw$target) else "-",
        if (!is.null(fs_rsb)) paste(fs_rsb$fsnum, "Studi") else "-",
        if (!is.null(fs_rsb)) sprintf("p = %.4f (Average g = %.4f)", fs_rsb$pval, fs_rsb$meanes) else "-",
        ifelse(
          fs_ros$fsnum > threshold_ros,
          "Kuat / Tahan Bias File-Drawer (N_fs > 5k + 10)",
          ifelse(fs_ros$fsnum == 0, "N_fs = 0 (Efek gabungan belum melampaui ambang signifikansi target)", "Rentan terhadap Studi Tidak Terpublikasi (N_fs <= 5k + 10)")
        )
      ),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

# Run Application
shinyApp(ui = ui, server = server)
