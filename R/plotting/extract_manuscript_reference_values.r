# Extract ALL numerical values referenced in Manuscript.tex and reply.tex
# Purpose: Single truth-table for manuscript audit
# Output: data/output/models/manuscript_reference_values.txt
#
# Uses emdi::summary() for diagnostics (consistent with extract_model_values.r)
# Uses emdi::estimators() for FH estimates, MSE, CV
#
# ============================================================================
# LINE REFERENCE INDEX: body_main.tex numbers → R code sections below
# ============================================================================
# L8:   "316,000 respondents"                        → Section 1 (nrow df_persons)
# L12:  "1,590 estimation domains (400 x 4 - 10)"   → Section 3 (fh_in), Section 13
# L12:  "16--44%"                                    → Section 6 (cv_reduction_pct)
# L19:  "316,361 individuals", "156,420 households", "960,619 trips" → Section 1
# L21:  "1,604 domain cells across 401 districts"    → Section 2 (nrow df_direct), Section 13
# L21:  "10 domains...out-of-sample"                 → Section 2 (n_zero), Section 13
# L21:  "1,590 domains across 400 districts"         → Section 3 (fh_in), Section 13
# L30:  "38 statistical tables"                      → Section 14 (census_tables count)
# L37:  "562 indicators", "540 unique", "424 (79%)", "480 (89%)", "526 (97%)" → Section 15
# L37:  "400 districts", "41 thematic categories"    → Section 15 (INKAR data)
# L73:  "S = 160 strata"                             → Section 10 (16 states x 10 types)
# L77:  "~1% states, 0.7% sex, 2.1% age, 3.7% quarter, 1.4% weekday" → Section 8
# L87:  "16 states x 10 regional types"              → Section 10
# L97:  "slope ~0.89, R2 = 0.96" (bootstrap)         → Section 9
# L186: "1,095 potential covariates for 1,590 domains"→ Section 5 + Section 16
# L186: "p=1,092...binom(p,2) ~ 596,000"             → Section 16
# L221: median estimates by mode (26.4, 4.5, 0.9, 0.9) → Section 2 (direct_by_mode$median_est)
# L221: "62.8%, 48.8%, 21.9%, 11.4%" CV below 20%    → Section 2 (100 - pct_cv_above_20)
# L221: "63.7% of all domain estimates exceed 20%"    → Section 2 (overall CV > 20%)
# L221: "246 persons...43...34" (median n_with_trips)  → Section 2
# L263: "26.4, 4.2, 0.9, 0.9" (FH medians)           → Section 3 (fh_by_mode$median_FH)
# L263: "2.4 times longer, 2.3 times higher (27% vs 12%)" → Section 7
# L268: "beta1 = 0.37, 0.40, 0.50, 0.48"              → Section 3 (regression slopes)
# L270: "mean gamma_d 0.54 (car), 0.48 (walking)"     → Section 3 (gamma_by_mode)
# L288: "63.7%...37.0%"                               → Section 2 + Section 6
# L288: "62.8→88.2, 48.8→78.4, 21.9→51.3, 11.4→33.4" → Section 6 (CV<20% by mode)
# L297: "0.16 (car) to 0.44 (public transit)"         → Section 6 (rv_by_mode$median_RV)
# L297: "158 of 1,590 domains (9.9%)"                 → Section 6 (overall negative RV)
# L315: "17.40 to 3.25" (kurtosis log vs nolog)       → Section 12
# L315: "2.17e-36 to 2.04e-3" (Shapiro-Wilk p)       → Section 4 + Section 12
# L315: "49 for log, 37 for non-log"                  → Section 4 (n_coefficients excl. intercept)
# L326: "3.83 vs target 3.0" (RE kurtosis)            → Section 4
# L326: "4.43" (residual kurtosis)                     → Section 4
# L326: "p < 0.01"                                     → Section 4 (Shapiro-Wilk)
# L349: "11%...22%...49%...63%" (CV<20% direct)        → Section 6
# L349: "33%...51%...78%...88%" (CV<20% FH)            → Section 6
# L349: "93%, 89%, 3.83" (FH R2, AdjR2, RE kurtosis)  → Section 4
# ============================================================================
# Numbers in body_appendix.tex:
# L48:  "~1% states, 0.7% sex, 2.1% age, 3.7% quarter, 1.4% weekday" → Section 8
# L69:  "beta_1 ~ 0.89, R2 = 0.96" (panel a)         → Section 9
# L69:  "beta_1 ~ 1.00, R2 > 0.99" (panels b,c)      → Section 9
# ============================================================================
# Numbers in abstract.tex:
# L1:   "1,590 estimation domains...400 districts"     → Section 3, Section 13
# L1:   "16--44%"                                      → Section 6
# ============================================================================

library(dplyr)
library(tidyr)
library(emdi)

data_path <- "./data"

sink(file.path(data_path, "output/models/manuscript_reference_values.txt"))

cat("================================================================\n")
cat("MANUSCRIPT REFERENCE VALUES\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat("================================================================\n\n")

# ==============================================================================
# 1. DATA DESCRIPTION (Section 2)
# ==============================================================================

cat("================================================================\n")
cat("1. DATA DESCRIPTION\n")
cat("================================================================\n\n")

df_persons <- readRDS(file.path(data_path, "data_cleaning/df_persons.rds"))
df_trips <- readRDS(file.path(data_path, "data_cleaning/df_trips_with_persons.rds"))

cat("Total persons in cleaned data:", nrow(df_persons), "\n")  # body_main.tex L8: "316,000 respondents"; L19: "316,361 individuals"
cat("Total trips in cleaned data:", nrow(df_trips), "\n")      # body_main.tex L19: "960,619 trips"
cat("Unique households:", length(unique(df_persons$H_ID_Reg)), "\n")  # body_main.tex L19: "156,420 households"
cat("Unique districts (ags5):", length(unique(df_persons$ags5)), "\n")  # body_main.tex L21: "401 districts"
cat("Unique Bundeslaender:", length(unique(df_persons$BLAND)), "\n")    # body_main.tex L73: "16 states"; L87: "16 states x 10 regional types"

# ==============================================================================
# 2. DIRECT ESTIMATES (Section 4.1)
# ==============================================================================

cat("\n================================================================\n")
cat("2. DIRECT ESTIMATES\n")
cat("================================================================\n\n")

df_direct <- readRDS(file.path(data_path, "output/direct/df_HTH_perskm_analytical.rds"))

cat("Total domains:", nrow(df_direct), "\n")              # body_main.tex L21: "1,604 domain cells"
cat("Unique districts:", length(unique(df_direct$ags5)), "\n")  # body_main.tex L21: "401 districts"
cat("Modes:", paste(levels(df_direct$hvm_label), collapse = ", "), "\n\n")  # body_main.tex L21: "walking...cycling...MIV...public transport"

# Median estimates by mode
# body_main.tex L221: "car travel shows the highest MPK with a median of 26.4 km/day,
#   followed by public transit (4.5 km/day), cycling (0.9 km/day), and walking (0.9 km/day)"
#   → check median_est for each mode
# body_main.tex L221: "car travel achieves the best coverage with 62.8% of domains meeting
#   this criterion, followed by walking (48.8%), cycling (21.9%), and public transit (11.4%)"
#   → check (100 - pct_cv_above_20) for each mode; NOTE: manuscript reports % BELOW 20%,
#     code computes % ABOVE 20%, so: 100 - pct_cv_above_20 = % below threshold
# body_main.tex L221: "car travel has the largest median (246 persons per domain),
#   while cycling (43) and public transit (34)"
#   → check median_n_with_trips for each mode (NOT n_obs, which counts ALL persons including zero-trip)
cat("--- Median direct estimates (km/day) by mode ---\n")
direct_by_mode <- df_direct %>%
  group_by(hvm_label) %>%
  summarise(
    n = n(),
    mean_est = mean(perskm_hth, na.rm = TRUE),
    median_est = median(perskm_hth, na.rm = TRUE),
    mean_se = mean(se_hth, na.rm = TRUE),
    median_se = median(se_hth, na.rm = TRUE),
    mean_cv = mean(cv_hth, na.rm = TRUE),
    median_cv = median(cv_hth, na.rm = TRUE),
    pct_cv_above_20 = mean(cv_hth > 0.20, na.rm = TRUE) * 100,
    median_n_obs = median(n_obs, na.rm = TRUE),
    mean_n_obs = mean(n_obs, na.rm = TRUE),
    median_n_with_trips = median(n_with_trips, na.rm = TRUE),
    mean_n_with_trips = mean(n_with_trips, na.rm = TRUE),
    .groups = "drop"
  )
print(as.data.frame(direct_by_mode), digits = 4)

cat("\n--- Overall CV distribution ---\n")
cat("% domains with CV > 20%:", round(mean(df_direct$cv_hth > 0.20, na.rm = TRUE) * 100, 1), "%\n")  # body_main.tex L221: "63.7% of all domain estimates exceed the 20% threshold"; also L288, L349
cat("% domains with CV > 30%:", round(mean(df_direct$cv_hth > 0.30, na.rm = TRUE) * 100, 1), "%\n")
cat("% domains with CV > 50%:", round(mean(df_direct$cv_hth > 0.50, na.rm = TRUE) * 100, 1), "%\n")

cat("\n--- Sample size summary ---\n")
cat("Total sample across all domains:", sum(df_direct$n_obs, na.rm = TRUE), "\n")
cat("Median sample size:", median(df_direct$n_obs, na.rm = TRUE), "\n")
cat("Mean sample size:", round(mean(df_direct$n_obs, na.rm = TRUE), 1), "\n")
cat("Min sample size:", min(df_direct$n_obs, na.rm = TRUE), "\n")
cat("Max sample size:", max(df_direct$n_obs, na.rm = TRUE), "\n")

# Out-of-sample domains: domains where no person reported a trip by that mode
# NOTE: n_obs counts ALL persons (always > 0), n_with_trips counts persons WITH trips
n_zero <- sum(df_direct$n_with_trips == 0, na.rm = TRUE)
cat("\nDomains with zero trips (n_with_trips == 0):", n_zero, "\n")  # body_main.tex L21: "10 domains are excluded where no sampled respondent reported any trip"

# ==============================================================================
# 3. FH MODEL ESTIMATES (Section 4.3) - using emdi functions
# ==============================================================================

cat("\n================================================================\n")
cat("3. FH MODEL ESTIMATES (Final/CV-selected model)\n")
cat("================================================================\n\n")

fh_model <- readRDS(file.path(data_path, "output/models/fh_model_final.rds"))

# Use emdi::estimators for proper CV calculation
fh_est_raw <- estimators(fh_model, indicator = "FH", MSE = TRUE, CV = TRUE)$ind
cat("Total FH domains (from estimators):", nrow(fh_est_raw), "\n")

# Get gamma from model internals (as in extract_model_values.r)
gamma_df <- fh_model$model$gamma

# Get Out indicator from m$ind
out_df <- fh_model$ind %>% select(Domain, Out)

# Merge estimates with gamma and Out indicator
fh_est <- fh_est_raw %>%
  left_join(gamma_df, by = "Domain") %>%
  left_join(out_df, by = "Domain") %>%
  mutate(
    Out = ifelse(is.na(Out), 0, Out),
    hvm_label = sub(".*_", "", Domain),
    ags5 = sub("_.*", "", Domain)
  )

# RV = 1 - CV_FH / CV_HJ (eq. 3.18 in manuscript)
# Merge with direct CV for RV calculation
direct_cv_df <- df_direct %>%
  mutate(Domain = paste(ags5, hvm_label, sep = "_")) %>%
  select(Domain, CV_HTH = cv_hth)
fh_est <- fh_est %>%
  left_join(direct_cv_df, by = "Domain") %>%
  mutate(RV = ifelse(!is.na(FH_CV) & !is.na(CV_HTH) & CV_HTH > 0,
                     1 - FH_CV / CV_HTH, NA))

# In-sample only
fh_in <- fh_est %>% filter(Out == 0)

cat("In-sample FH domains:", nrow(fh_in), "\n")              # body_main.tex L12: "1,590 estimation domains"; L21: "1,590 domains across 400 districts"
cat("emdi Out-of-sample indicator (Out != 0):", sum(fh_est$Out != 0), "\n")
cat("NOTE: The 10 zero-sample domains are excluded BEFORE FH fitting (in phase1 inner_join),\n")
cat("      so they do not appear in fh_est at all. The emdi 'Out' indicator is unrelated.\n")
cat("      The '10 excluded domains' number is verified in Section 2 (n_zero from df_direct).\n\n")

# body_main.tex L263: "car travel dominates...median of 26.4 km/day, followed by
#   public transit (4.3 km/day), cycling (0.9 km/day), and walking (0.9 km/day)"
#   → check median_FH for each mode
cat("--- Median FH estimates by mode ---\n")
fh_by_mode <- fh_in %>%
  group_by(hvm_label) %>%
  summarise(
    n = n(),
    mean_FH = mean(FH, na.rm = TRUE),
    median_FH = median(FH, na.rm = TRUE),
    mean_FH_CV_pct = mean(FH_CV * 100, na.rm = TRUE),
    median_FH_CV_pct = median(FH_CV * 100, na.rm = TRUE),
    pct_cv_above_20 = mean(FH_CV > 0.20, na.rm = TRUE) * 100,
    .groups = "drop"
  )
print(as.data.frame(fh_by_mode), digits = 4)

cat("\nOverall FH CV > 20% (in-sample):", round(mean(fh_in$FH_CV > 0.20, na.rm = TRUE) * 100, 1), "%\n")  # body_main.tex L288: "37.0% of FH estimates do so"

# Direct vs FH: % of domains with CV below 20%, by mode
# body_main.tex L288/L349: comparison of direct vs FH precision by mode
cat("\n--- % domains with CV < 20% by mode: Direct vs FH ---\n")
cv_comparison <- direct_by_mode %>%
  select(hvm_label, direct_below_20 = pct_cv_above_20) %>%
  mutate(direct_below_20 = round(100 - direct_below_20, 1)) %>%
  left_join(
    fh_by_mode %>% select(hvm_label, fh_below_20 = pct_cv_above_20) %>%
      mutate(fh_below_20 = round(100 - fh_below_20, 1)),
    by = "hvm_label"
  ) %>%
  mutate(improvement_pp = round(fh_below_20 - direct_below_20, 1))
print(as.data.frame(cv_comparison))

# Gamma (shrinkage factor) by mode
# body_main.tex L270: "Car travel has the highest mean gamma_d (0.56)"
# body_main.tex L270: "Walking, with similarly high gamma_d (0.50)"
cat("\n--- Gamma (shrinkage factor) by mode ---\n")
gamma_by_mode <- fh_in %>%
  group_by(hvm_label) %>%
  summarise(
    mean_gamma = mean(Gamma, na.rm = TRUE),
    median_gamma = median(Gamma, na.rm = TRUE),
    min_gamma = min(Gamma, na.rm = TRUE),
    max_gamma = max(Gamma, na.rm = TRUE),
    .groups = "drop"
  )
print(as.data.frame(gamma_by_mode), digits = 4)

# Overall gamma
cat("\nOverall gamma mean:", round(mean(fh_in$Gamma, na.rm = TRUE), 4), "\n")
cat("Overall gamma median:", round(median(fh_in$Gamma, na.rm = TRUE), 4), "\n")

# Regression: FH vs HJ (HTH)
# body_main.tex L268: "slopes beta_1 < 1...strongest for walking (beta_1 = 0.37),
#   followed by public transit (beta_1 = 0.40), while cycling and car travel
#   show the least compression (both beta_1 ~ 0.50)"
#   → check coef(fit)[2] for each mode
cat("\n--- Regression slopes: FH vs HJ by mode ---\n")
merged <- fh_in %>%
  left_join(
    df_direct %>% mutate(Domain = paste(ags5, hvm_label, sep = "_")) %>%
      select(Domain, perskm_hth),
    by = "Domain"
  ) %>%
  filter(!is.na(perskm_hth) & !is.na(FH))

for (mode in c("fuss", "fahrrad", "miv", "oepv")) {
  d <- merged %>% filter(hvm_label == mode)
  if (nrow(d) > 2) {
    fit <- lm(FH ~ perskm_hth, data = d)
    cat(sprintf("  %s: beta1 = %.3f, R2 = %.3f, n = %d\n",
                mode, coef(fit)[2], summary(fit)$r.squared, nrow(d)))
  }
}

# Overall regression
fit_all <- lm(FH ~ perskm_hth, data = merged)
cat(sprintf("  OVERALL: beta1 = %.3f, R2 = %.3f, n = %d\n",
            coef(fit_all)[2], summary(fit_all)$r.squared, nrow(merged)))

# RV (Relative Variability) = 1 - CV_FH / CV_HJ (eq. 3.18 in manuscript)
# body_main.tex L297: "median RV ranges from 0.16 (car) to 0.43 (public transit)"
#   → check median_RV for miv and oepv
# body_main.tex L297: "16--43%" (also abstract.tex L1, L12 intro)
# body_main.tex L297: "160 of 1,590 domains (10.1%) show negative RV values"
#   → check n_negative_RV summed, and overall count
cat("\n--- Relative Variability (RV = 1 - CV_FH/CV_HJ) by mode ---\n")
rv_by_mode <- fh_in %>%
  filter(!is.na(RV)) %>%
  group_by(hvm_label) %>%
  summarise(
    median_RV = median(RV, na.rm = TRUE),
    mean_RV = mean(RV, na.rm = TRUE),
    n_negative_RV = sum(RV < 0, na.rm = TRUE),
    pct_negative_RV = mean(RV < 0, na.rm = TRUE) * 100,
    n = n(),
    .groups = "drop"
  )
print(as.data.frame(rv_by_mode), digits = 4)

cat("\nOverall negative RV:", sum(fh_in$RV < 0, na.rm = TRUE), "of", sum(!is.na(fh_in$RV)), "\n")  # body_main.tex L297: "160 of 1,590 domains (10.1%)"
cat("Overall median RV:", round(median(fh_in$RV, na.rm = TRUE), 4), "\n")

# ==============================================================================
# 4. MODEL DIAGNOSTICS (Section 4.4) — from emdi::summary (via extract_model_values.r)
# ==============================================================================

cat("\n================================================================\n")
cat("4. MODEL DIAGNOSTICS (all 6 models, from extract_model_values.r)\n")
cat("================================================================\n\n")

diag <- readRDS(file.path(data_path, "output/models/model_diagnostics_summary.rds"))
print(as.data.frame(diag), digits = 4)

# Key values for manuscript text
cat("\n--- Key values for manuscript ---\n")
cat("Final model (baseline_min):\n")
cat("  n_coefficients:", diag$baseline_min[diag$diagnostic == "n_coefficients"], "\n")
cat("  sigma_u_sq:", diag$baseline_min[diag$diagnostic == "sigma_u_sq"], "\n")          # tbl_varsel_cv (not hardcoded in text)
cat("  RE kurtosis:", diag$baseline_min[diag$diagnostic == "re_kurtosis"], "\n")         # body_main.tex L326/L349: "3.83"
cat("  RE skewness:", diag$baseline_min[diag$diagnostic == "re_skewness"], "\n")
cat("  RE Shapiro-Wilk W:", diag$baseline_min[diag$diagnostic == "re_shapiro_W"], "\n")
cat("  RE Shapiro-Wilk p:", diag$baseline_min[diag$diagnostic == "re_shapiro_p"], "\n")  # body_main.tex L326: "p < 0.01"
cat("  Residual kurtosis:", diag$baseline_min[diag$diagnostic == "resid_kurtosis"], "\n")  # body_main.tex L326: "4.43"
cat("  Residual skewness:", diag$baseline_min[diag$diagnostic == "resid_skewness"], "\n")
cat("  AdjR2:", diag$baseline_min[diag$diagnostic == "AdjR2"], "\n")                    # body_main.tex L349: "89%"
cat("  FH_R2:", diag$baseline_min[diag$diagnostic == "FH_R2"], "\n")                    # body_main.tex L349: "93%"
cat("  AIC:", diag$baseline_min[diag$diagnostic == "AIC"], "\n")

cat("\nMaximal_min model (runner-up):\n")
if ("maximal_min" %in% names(diag)) {
  cat("  n_coefficients:", diag$maximal_min[diag$diagnostic == "n_coefficients"], "\n")
  cat("  sigma_u_sq:", diag$maximal_min[diag$diagnostic == "sigma_u_sq"], "\n")
  cat("  RE kurtosis:", diag$maximal_min[diag$diagnostic == "re_kurtosis"], "\n")         # tbl_varsel_cv (table comparison only)
  cat("  AdjR2:", diag$maximal_min[diag$diagnostic == "AdjR2"], "\n")                     # tbl_varsel_cv (table comparison only)
  cat("  FH_R2:", diag$maximal_min[diag$diagnostic == "FH_R2"], "\n")
  cat("  AIC:", diag$maximal_min[diag$diagnostic == "AIC"], "\n")
} else {
  cat("  [MISSING] maximal_min not in diagnostics — run extract_model_values.r\n")
}

cat("\nBase model:\n")
cat("  n_coefficients:", diag$base[diag$diagnostic == "n_coefficients"], "\n")            # body_main.tex L315: "49 for log, 37 for non-log" (excl. intercept)
cat("  sigma_u_sq:", diag$base[diag$diagnostic == "sigma_u_sq"], "\n")                   # body_main.tex L315: "0.37 to 0.10" → base log = 0.10
cat("  RE kurtosis:", diag$base[diag$diagnostic == "re_kurtosis"], "\n")                 # body_main.tex L315: "3.25" (log base)
cat("  RE Shapiro-Wilk p:", diag$base[diag$diagnostic == "re_shapiro_p"], "\n")          # body_main.tex L315: "2.04e-3" (log base)
cat("  AIC:", diag$base[diag$diagnostic == "AIC"], "\n")

cat("\nBase non-log model:\n")
cat("  n_coefficients:", diag$base_nolog[diag$diagnostic == "n_coefficients"], "\n")      # body_main.tex L315: "37 for non-log" (excl. intercept)
cat("  sigma_u_sq:", diag$base_nolog[diag$diagnostic == "sigma_u_sq"], "\n")             # body_main.tex L315: sigma_u_sq for non-log
cat("  RE kurtosis:", diag$base_nolog[diag$diagnostic == "re_kurtosis"], "\n")           # body_main.tex L315: "17.40"

# Cross-model diagnostic ranges (useful for table verification)
# These are across the 5 log-transformed models (base, baseline_min, baseline_1se, maximal_min, maximal_1se)
log_models <- intersect(c("base", "baseline_min", "baseline_1se", "maximal_min", "maximal_1se"), names(diag))
if (length(log_models) >= 2) {
  re_kurt_vals <- sapply(log_models, function(m) diag[[m]][diag$diagnostic == "re_kurtosis"])
  resid_kurt_vals <- sapply(log_models, function(m) diag[[m]][diag$diagnostic == "resid_kurtosis"])
  cat("\n--- Cross-model diagnostic ranges (5 log-transformed models) ---\n")
  cat("Models included:", paste(log_models, collapse = ", "), "\n")
  cat("RE kurtosis range:", round(min(re_kurt_vals), 2), "to", round(max(re_kurt_vals), 2), "\n")
  cat("Residual kurtosis range:", round(min(resid_kurt_vals), 2), "to", round(max(resid_kurt_vals), 2), "\n")
  cat("RE kurtosis by model:\n")
  for (m in log_models) cat("  ", m, ":", round(diag[[m]][diag$diagnostic == "re_kurtosis"], 4), "\n")
  cat("Resid kurtosis by model:\n")
  for (m in log_models) cat("  ", m, ":", round(diag[[m]][diag$diagnostic == "resid_kurtosis"], 4), "\n")
} else {
  cat("\n[WARNING] Not enough log models to compute cross-model ranges\n")
  cat("  Found:", paste(log_models, collapse = ", "), "\n")
}

# Also verify with emdi::summary directly on final model
cat("\n--- Normality diagnostics from emdi::summary(fh_model_final) ---\n")
s_final <- summary(fh_model)
print(s_final$normality)
cat("\nsigma_u_sq:", fh_model$model$variance, "\n")
cat("model_select:\n")
print(fh_model$model$model_select)

# ==============================================================================
# 5. VARIABLE SELECTION (Section 4.2)
# ==============================================================================

cat("\n================================================================\n")
cat("5. VARIABLE SELECTION PIPELINE\n")
cat("================================================================\n\n")

# Phase 1 LASSO
phase1 <- readRDS(file.path(data_path, "output/variables/phase1_lasso_results.rds"))
cat("Phase 1 LASSO:\n")
cat("  Selected variables:", length(phase1$varlist), "\n")  # body_main.tex L238: "reduces the 1,095 candidate covariates to a manageable set"
cat("  Performance:\n")
print(phase1$performance)

# Phase 2 sparseR
p2_base <- readRDS(file.path(data_path, "output/variables/phase2_sparseR_results_baseline.rds"))
p2_max <- readRDS(file.path(data_path, "output/variables/phase2_sparseR_results_maximal.rds"))

# body_main.tex L210: "specific variable counts are reported in Table tbl_varsel_pipeline"
#   → all Phase 2 counts feed into tbl_varsel_pipeline (generated table)
cat("\nPhase 2 sparseR Baseline (poly2, mode x covar):\n")
cat("  Base covariates:", p2_base$n_base_covariates, "\n")
cat("  Expanded terms:", p2_base$n_expanded_terms, "\n")
cat("  lambda.min selected:", length(p2_base$selected_lambda_min), "\n")   # body_main.tex L238: "baseline lambda_min" winner
cat("  lambda.1se selected:", length(p2_base$selected_lambda_1se), "\n")
cat("  lambda.min by category:", paste(names(lengths(p2_base$categories_min)), lengths(p2_base$categories_min), sep="=", collapse=", "), "\n")
cat("  lambda.1se by category:", paste(names(lengths(p2_base$categories_1se)), lengths(p2_base$categories_1se), sep="=", collapse=", "), "\n")

cat("\nPhase 2 sparseR Maximal (poly3, +covar x covar):\n")
cat("  Base covariates:", p2_max$n_base_covariates, "\n")
cat("  Expanded terms:", p2_max$n_expanded_terms, "\n")
cat("  lambda.min selected:", length(p2_max$selected_lambda_min), "\n")
cat("  lambda.1se selected:", length(p2_max$selected_lambda_1se), "\n")
cat("  lambda.min by category:", paste(names(lengths(p2_max$categories_min)), lengths(p2_max$categories_min), sep="=", collapse=", "), "\n")
cat("  lambda.1se by category:", paste(names(lengths(p2_max$categories_1se)), lengths(p2_max$categories_1se), sep="=", collapse=", "), "\n")

# Phase 3 CV
# body_main.tex L243: "baseline lambda_min model achieves the lowest SSE on reliable domains"
p3 <- readRDS(file.path(data_path, "output/models/phase3_cv_results.rds"))
cat("\nPhase 3 Cross-Validation:\n")
cat("  Winner:", p3$winner, "\n")                           # body_main.tex L238: Phase 3 selects "lambda_min model as winner"
cat("  CV results:\n")
print(as.data.frame(p3$cv_summary), digits = 4)             # body_main.tex L243: tbl_varsel_cv numbers

# ==============================================================================
# 6. FH CV REDUCTION (Abstract, Section 5, Conclusion)
# ==============================================================================

cat("\n================================================================\n")
cat("6. CV REDUCTION: Direct vs FH\n")
cat("================================================================\n\n")

merged2 <- fh_in %>%
  left_join(
    df_direct %>% mutate(Domain = paste(ags5, hvm_label, sep = "_")) %>%
      select(Domain, cv_hth, hvm_label_direct = hvm_label),
    by = "Domain"
  ) %>%
  filter(!is.na(cv_hth) & !is.na(FH_CV))

# Both CVs in percentage (0-100 scale)
merged2$cv_direct_pct <- merged2$cv_hth * 100
merged2$cv_fh_pct <- merged2$FH_CV * 100

# body_main.tex L297: "median RV ranges from 0.16 (car) to 0.43 (public transit),
#   indicating that the FH model typically reduces the CV by 16--43%"
#   → check cv_reduction_pct for miv and oepv
# abstract.tex L1: "reduces coefficients of variation by 16--43%"
# body_main.tex L12: "precision improvements of 16--43%"
# body_main.tex L349: "only 11% of public transit domains and 22% of cycling domains,
#   compared to 63% for car travel" → check (100 - direct_above_20) for each mode
cat("--- CV reduction by mode ---\n")
cv_red <- merged2 %>%
  group_by(hvm_label) %>%
  summarise(
    median_direct_cv_pct = median(cv_direct_pct, na.rm = TRUE),
    median_fh_cv_pct = median(cv_fh_pct, na.rm = TRUE),
    cv_reduction_pct = (1 - median(cv_fh_pct, na.rm = TRUE) / median(cv_direct_pct, na.rm = TRUE)) * 100,
    direct_above_20 = mean(cv_direct_pct > 20, na.rm = TRUE) * 100,
    fh_above_20 = mean(cv_fh_pct > 20, na.rm = TRUE) * 100,
    .groups = "drop"
  )
print(as.data.frame(cv_red), digits = 4)

cat("\n--- Overall CV reduction ---\n")
cat("Overall median direct CV:", round(median(merged2$cv_direct_pct, na.rm = TRUE), 1), "%\n")
cat("Overall median FH CV:", round(median(merged2$cv_fh_pct, na.rm = TRUE), 1), "%\n")
cat("CV reduction (median):", round((1 - median(merged2$cv_fh_pct, na.rm = TRUE) / median(merged2$cv_direct_pct, na.rm = TRUE)) * 100, 1), "%\n")
cat("Direct CV > 20%:", round(mean(merged2$cv_direct_pct > 20, na.rm = TRUE) * 100, 1), "%\n")   # body_main.tex L288: "63.7%" (same as Section 2 above)
cat("FH CV > 20%:", round(mean(merged2$cv_fh_pct > 20, na.rm = TRUE) * 100, 1), "%\n")           # body_main.tex L288: "37.0%"

# ==============================================================================
# 7. WALKING/CYCLING COMPARISON (reply.tex)
# body_main.tex L263: "cycling trips are 2.4 times longer on average, but walking
#   participation is 2.3 times higher (27% vs 12%)"
# ==============================================================================

cat("\n================================================================\n")
cat("7. WALKING vs CYCLING COMPARISON\n")
cat("================================================================\n\n")

wc_file <- file.path(data_path, "output/comparison_walking_cycling.rds")
if (file.exists(wc_file)) {
  wc <- readRDS(wc_file)
  # Print only data.frame elements (skip metadata list)
  for (nm in names(wc)) {
    obj <- wc[[nm]]
    if (is.data.frame(obj) && nrow(obj) > 0) {
      cat("--- ", nm, " ---\n")
      print(as.data.frame(obj), digits = 4)
      cat("\n")
    }
  }
} else {
  cat("comparison_walking_cycling.rds not found\n")
}

# ==============================================================================
# 8. CALIBRATION VALIDATION (Appendix B)
# body_main.tex L77: "mean deviations of approximately 1% for federal states,
#   0.7% for sex, 2.1% for age groups, 3.7% for survey quarter, and 1.4% for weekday"
# body_appendix.tex L48: same numbers repeated
# ==============================================================================

cat("\n================================================================\n")
cat("8. CALIBRATION VALIDATION\n")
cat("================================================================\n\n")

val_file <- file.path(data_path, "output/direct/calibration_validation.rds")
if (file.exists(val_file)) {
  val <- readRDS(val_file)
  cat("Validation summary:\n")
  print(as.data.frame(val$validation_summary), digits = 4)
  cat("\nHH size details (NOTE: may be INCORRECT - uses partial Kreise sums):\n")
  print(as.data.frame(val$hh_size_details), digits = 4)
  cat("\nDecision:", val$decision_reason, "\n")
  cat("Note:", val$note, "\n")
} else {
  cat("calibration_validation.rds not found\n")
}

cat("\n--- Correct national HH totals (DG row, 12211-Z-10.csv, in thousands) ---\n")
cat("1-person HH: 17,263\n")
cat("2-person HH: 13,850\n")
cat("3+ person HH: 10,191\n")
cat("Total HH: 41,304\n")

# ==============================================================================
# 9. BOOTSTRAP COMPARISON (Appendix B)
# body_main.tex L97: "slope beta_1 ~ 0.89, R2 = 0.96"
# body_main.tex L97: "R2 > 0.99, slopes ~ 1.0"
# body_main.tex L339: "regression slope ~ 0.89, R2 = 0.96"
# body_appendix.tex L69: "beta_1 ~ 0.89 and R2 = 0.96" (panel a)
# body_appendix.tex L69: "beta_1 ~ 1.00, R2 > 0.99" (panels b, c)
# ==============================================================================

cat("\n================================================================\n")
cat("9. BOOTSTRAP vs ANALYTICAL VARIANCE COMPARISON\n")
cat("================================================================\n\n")

# Load three separate emdi::direct() bootstrap output objects
# Each has $ind (Domain, Mean, ...) and $MSE (Mean, ...)
boot_mid_file <- file.path(data_path, "output/direct/df_HTH_perskm_emdi_mid_calib.rds")
boot_census_file <- file.path(data_path, "output/direct/df_HTH_perskm_emdi_census_calib.rds")
boot_naive_file <- file.path(data_path, "output/direct/df_HTH_perskm_emdi_naive.rds")

boot_files_exist <- file.exists(boot_mid_file) & file.exists(boot_census_file) & file.exists(boot_naive_file)

if (boot_files_exist) {
  emdi_mid <- readRDS(boot_mid_file)
  emdi_census <- readRDS(boot_census_file)
  emdi_naive <- readRDS(boot_naive_file)

  # Extract SE from each bootstrap method (SE = sqrt(MSE))
  df_boot_mid <- data.frame(
    Domain = emdi_mid$ind$Domain,
    se_mid_calib = sqrt(emdi_mid$MSE$Mean)
  )
  df_boot_census <- data.frame(
    Domain = emdi_census$ind$Domain,
    se_census_calib = sqrt(emdi_census$MSE$Mean)
  )
  df_boot_naive <- data.frame(
    Domain = emdi_naive$ind$Domain,
    se_naive = sqrt(emdi_naive$MSE$Mean)
  )

  # Merge analytical SE with all three bootstrap SEs
  comp <- df_direct %>%
    mutate(Domain = paste(ags5, hvm_label, sep = "_"),
           se_analytical = sqrt(var_hth)) %>%
    select(Domain, se_analytical) %>%
    left_join(df_boot_mid, by = "Domain") %>%
    left_join(df_boot_census, by = "Domain") %>%
    left_join(df_boot_naive, by = "Domain") %>%
    filter(!is.na(se_mid_calib) & !is.na(se_census_calib) & !is.na(se_naive))

  cat("Domains compared:", nrow(comp), "\n\n")

  # Panel (a): Taylor linearization vs MiD-calibrated bootstrap
  # body_appendix.tex L69: "beta_1 ~ 0.89 and R2 = 0.96"
  fit_a <- lm(se_mid_calib ~ se_analytical, data = comp)
  cat(sprintf("Panel (a) Taylor vs MiD-calib: beta1 = %.3f, R2 = %.4f\n",
              coef(fit_a)[2], summary(fit_a)$r.squared))

  # Panel (b): Census-calibrated vs MiD-calibrated bootstrap
  # body_appendix.tex L69: "beta_1 ~ 1.00, R2 > 0.99"
  fit_b <- lm(se_census_calib ~ se_mid_calib, data = comp)
  cat(sprintf("Panel (b) Census vs MiD-calib: beta1 = %.3f, R2 = %.4f\n",
              coef(fit_b)[2], summary(fit_b)$r.squared))

  # Panel (c): Naive vs Census-calibrated bootstrap
  # body_appendix.tex L69: "beta_1 ~ 1.00, R2 > 0.99"
  fit_c <- lm(se_naive ~ se_census_calib, data = comp)
  cat(sprintf("Panel (c) Naive vs Census-calib: beta1 = %.3f, R2 = %.4f\n",
              coef(fit_c)[2], summary(fit_c)$r.squared))
} else {
  cat("Bootstrap results not found (need all 3 files):\n")
  cat("  mid_calib:", ifelse(file.exists(boot_mid_file), "found", "MISSING"), "\n")
  cat("  census_calib:", ifelse(file.exists(boot_census_file), "found", "MISSING"), "\n")
  cat("  naive:", ifelse(file.exists(boot_naive_file), "found", "MISSING"), "\n")
}

# ==============================================================================
# 10. VARIANCE ESTIMATION DETAILS (Appendix B)
# ==============================================================================

cat("\n================================================================\n")
cat("10. VARIANCE ESTIMATION DETAILS\n")
cat("================================================================\n\n")

design_file <- file.path(data_path, "output/direct/mid_design_calibrated.rds")
if (file.exists(design_file)) {
  design <- readRDS(design_file)
  cat("Number of strata:", nrow(unique(design$strata[1]))* nrow(unique(design$strata[2])), "\n")

  if (!is.null(design$postStrata)) {
    cat("Calibration formula variables:\n")
    ps <- design$postStrata
    for (i in seq_along(ps)) {
      if (!is.null(attr(ps[[i]], "formula"))) {
        cat("  ", deparse(attr(ps[[i]], "formula")), "\n")
      }
    }
  }
}

cat("\nCalibration dimensions: BLAND (16) + HP_SEX (2) + age_group (5) + ST_QUARTAL (4) + ST_WOTAG (7) = 34\n")  # body_main.tex L73: "S = 160 strata"; L87: "16 states x 10 regional types"

# ==============================================================================
# 11. MODE-SPECIFIC GAMMA STATISTICS FOR REPLY
# ==============================================================================

cat("\n================================================================\n")
cat("11. MODE-SPECIFIC GAMMA FOR REPLY.TEX\n")
cat("================================================================\n\n")

for (mode in c("fuss", "fahrrad", "miv", "oepv")) {
  d <- fh_in %>% filter(hvm_label == mode)
  cat(sprintf("%s: mean=%.3f, median=%.3f, min=%.3f, max=%.3f, n=%d\n",
              mode, mean(d$Gamma, na.rm=TRUE), median(d$Gamma, na.rm=TRUE),
              min(d$Gamma, na.rm=TRUE), max(d$Gamma, na.rm=TRUE), nrow(d)))
}

# ==============================================================================
# 12. LOG vs NON-LOG COMPARISON
# body_main.tex L315: "kurtosis of the standardized random effects drops from 17.40 to 3.25"
# body_main.tex L315: "Shapiro-Wilk p-values increase...from 2.17e-36 to 2.04e-3"
# body_main.tex L315: sigma_u_sq comparison (log vs non-log)
# body_main.tex L315: "49 for log, 37 for non-log" (n_coefficients excl. intercept)
# NOTE: these numbers also appear in tbl_log_vs_nolog (generated table)
# ==============================================================================

cat("\n================================================================\n")
cat("12. LOG vs NON-LOG COMPARISON\n")
cat("================================================================\n\n")

cat("Non-log model:\n")
cat("  RE kurtosis:", diag$base_nolog[diag$diagnostic == "re_kurtosis"], "\n")     # body_main.tex L315: "17.40"
cat("  Resid kurtosis:", diag$base_nolog[diag$diagnostic == "resid_kurtosis"], "\n")
cat("  sigma_u_sq:", diag$base_nolog[diag$diagnostic == "sigma_u_sq"], "\n")       # body_main.tex L315: "0.37"
cat("  AIC:", diag$base_nolog[diag$diagnostic == "AIC"], "\n")

cat("\nLog model (base):\n")
cat("  RE kurtosis:", diag$base[diag$diagnostic == "re_kurtosis"], "\n")           # body_main.tex L315: "3.25"
cat("  Resid kurtosis:", diag$base[diag$diagnostic == "resid_kurtosis"], "\n")
cat("  sigma_u_sq:", diag$base[diag$diagnostic == "sigma_u_sq"], "\n")             # body_main.tex L315: "0.10"
cat("  AIC:", diag$base[diag$diagnostic == "AIC"], "\n")

# ==============================================================================
# 13. EXCLUDED DISTRICTS AND DOMAIN COUNTS
# ==============================================================================

cat("\n================================================================\n")
cat("13. EXCLUDED DISTRICTS AND DOMAIN COUNTS\n")
cat("================================================================\n\n")

n_districts_direct <- length(unique(df_direct$ags5))
n_districts_fh <- length(unique(fh_in$ags5))
n_domains_direct <- nrow(df_direct)
n_domains_fh <- nrow(fh_in)

cat("Direct estimation:", n_domains_direct, "domains across", n_districts_direct, "districts\n")  # body_main.tex L21: "1,604 domain cells across 401 districts"
cat("FH model (in-sample):", n_domains_fh, "domains across", n_districts_fh, "districts\n")       # body_main.tex L12: "1,590 estimation domains (400 districts x 4 modes)"
cat("emdi Out indicator (Out != 0):", sum(fh_est$Out != 0), "domains\n")
cat("Excluded before FH fitting:", n_domains_direct - nrow(fh_est), "domains\n")                  # body_main.tex L21: "domains excluded" = zero-sample + Eisenach
cat("  Of which zero-trip domains (n_with_trips == 0):", n_zero, "(verified in Section 2)\n")       # body_main.tex L21: "domains...out-of-sample"
cat("  Of which excluded districts: Eisenach (16056) = 1 district x 4 modes = 4\n")

# ==============================================================================
# 14. CENSUS TABLE COUNT
# body_main.tex L30: "38 statistical tables"
# ==============================================================================

cat("\n================================================================\n")
cat("14. CENSUS TABLE COUNT\n")
cat("================================================================\n\n")

census_tables_file <- file.path(data_path, "raw/census/selected_tables.csv")
if (file.exists(census_tables_file)) {
  census_tables <- read.csv(census_tables_file)
  cat("Census tables in selected_tables.csv:", nrow(census_tables), "\n")  # body_main.tex L30: "38 statistical tables"
} else {
  # Fallback: count individual table files
  table_files <- list.files(file.path(data_path, "raw/census"), pattern = "^table_.*_2017\\.rds$")
  cat("Census table .rds files found:", length(table_files), "\n")  # body_main.tex L30: "38 statistical tables"
}

# ==============================================================================
# 15. INKAR INDICATOR COUNTS
# body_main.tex L37: "562 indicators", "540 unique", "424 (79%)", "480 (89%)",
#   "526 (97%)", "400 districts", "41 thematic categories"
# ==============================================================================

cat("\n================================================================\n")
cat("15. INKAR INDICATOR COUNTS\n")
cat("================================================================\n\n")

inkar_quality_file <- file.path(data_path, "temp/inkar_variable_quality.rds")
inkar_metadata_file <- file.path(data_path, "temp/inkar_variable_metadata.rds")
inkar_data_file <- file.path(data_path, "temp/inkar_2017_districts.rds")

if (file.exists(inkar_metadata_file)) {
  inkar_meta <- readRDS(inkar_metadata_file)
  cat("Total variables collected (with duplicates):", nrow(inkar_meta), "\n")  # body_main.tex L37: "562 indicators"
  cat("Unique Gruppe IDs:", length(unique(inkar_meta$Gruppe)), "(= nrow, Gruppe is unique per row)\n")
  # NOTE: "540 unique" count comes from inkar_variable_quality.rds below (after dedup/quality filtering)
  if ("Bereich" %in% names(inkar_meta)) {
    n_themes <- length(unique(inkar_meta$Bereich))
    cat("Thematic categories (unique Bereich):", n_themes, "\n")  # body_main.tex L37: "41 thematic categories"
  }
} else {
  cat("inkar_variable_metadata.rds not found\n")
}

if (file.exists(inkar_quality_file)) {
  inkar_quality <- readRDS(inkar_quality_file)
  n_vars <- nrow(inkar_quality)
  cat("Variables in quality file:", n_vars, "\n")  # body_main.tex L37: "540 unique" (should match)

  n_target_year <- sum(inkar_quality$is_target_year, na.rm = TRUE)
  cat("Variables with 2017 data:", n_target_year,
      sprintf("(%.0f%%)", n_target_year / n_vars * 100), "\n")  # body_main.tex L37: "424 (79%)"
  cat("Variables not 2017 (nearest year):", n_vars - n_target_year, "\n")  # body_main.tex L37: "remaining 116"

  n_complete <- sum(inkar_quality$pct_missing == 0)
  cat("Complete (0% missing):", n_complete,
      sprintf("(%.0f%%)", n_complete / n_vars * 100), "\n")  # body_main.tex L37: "480 (89%)"

  n_low_missing <- sum(inkar_quality$pct_missing < 10)
  cat("< 10% missing:", n_low_missing,
      sprintf("(%.0f%%)", n_low_missing / n_vars * 100), "\n")  # body_main.tex L37: "526 (97%)"
} else {
  cat("inkar_variable_quality.rds not found\n")
}

if (file.exists(inkar_data_file)) {
  inkar_data <- readRDS(inkar_data_file)
  cat("INKAR districts:", nrow(inkar_data), "\n")  # body_main.tex L37: "400 districts"
} else {
  cat("inkar_2017_districts.rds not found\n")
}

# ==============================================================================
# 16. CANDIDATE COVARIATE COUNT (Phase 1 input)
# body_main.tex L186: "1,095 potential area-level covariates for 1,590 domain observations"
# body_main.tex L186: "p=1,092...binom(p,2) ~ 596,000"
# ==============================================================================

cat("\n================================================================\n")
cat("16. CANDIDATE COVARIATE COUNT (Phase 1 input)\n")
cat("================================================================\n\n")

phase1_data_file <- file.path(data_path, "output/models/phase1_prepared_data.rds")
if (file.exists(phase1_data_file)) {
  p1_data <- readRDS(phase1_data_file)

  # Count covariates in the prepared data (post-filtering)
  n_covar_post_filter <- length(p1_data$covar_cols)
  cat("Covariates after filtering:", n_covar_post_filter, "\n")

  # Reconstruct initial count from raw source data
  # NOTE: phase1_lasso_base_covariates.r uses INCLUDE_MID_COVARIATES = FALSE,
  #   so MiD covariates are EXCLUDED (body_main.tex L23: "we exclude MiD-derived covariates")
  #   Only Census + INKAR + 3 mode contrasts are used
  census_file <- file.path(data_path, "raw/census/census_merged_2017.rds")
  inkar_file <- file.path(data_path, "temp/inkar_2017_districts.rds")

  n_census <- n_inkar <- 0

  if (file.exists(census_file)) {
    df_census_raw <- readRDS(census_file)
    n_census <- ncol(df_census_raw) - 1  # minus ags5 column
    cat("Census covariate columns:", n_census, "\n")
  }
  if (file.exists(inkar_file)) {
    df_inkar_raw <- readRDS(inkar_file)
    n_inkar <- ncol(df_inkar_raw) - 1  # minus ags5 column
    cat("INKAR covariate columns:", n_inkar, "\n")
  }

  # The initial count before NA/NZV/correlation filtering
  # MiD covariates excluded (INCLUDE_MID_COVARIATES = FALSE)
  cat("Mode contrast columns: 3\n")
  cat("Total initial candidate covariates: Census + INKAR + mode_contrasts =",
      n_census, "+", n_inkar, "+ 3 =", n_census + n_inkar + 3, "\n")  # body_main.tex L186: "1,095"

  # The p in "binom(p,2)" excludes mode contrasts
  p_main <- n_census + n_inkar
  cat("p (main effects excl. mode contrasts):", p_main, "\n")  # body_main.tex L186: "p = 1,092"
  cat("binom(p,2) pairwise interactions:", choose(p_main, 2), "\n")  # body_main.tex L186: "~ 596,000"
} else {
  cat("phase1_prepared_data.rds not found\n")
}

sink()

cat("Reference values saved to:", file.path(data_path, "output/models/manuscript_reference_values.txt"), "\n")
