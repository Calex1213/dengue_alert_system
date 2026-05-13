# ============================================================
# BARANGAY ENVIRONMENTAL-ONLY PURE XGBOOST REGRESSOR H+0 TO H+12 SCRIPT
# Cebu City Dengue Early Warning System
#
# Purpose:
#   Train and evaluate ONLY the pure ENVIRONMENTAL / NON-AUTOCORRELATION XGBoost regressor
#   for barangay-level dengue prediction from H+0 to H+12.
#
# Key rule:
#   The model DOES NOT use any feature involving dengue cases, incidence,
#   target lag, outbreak label, citywide dengue activity, or neighbor dengue activity.
#
# Outputs saved in outputs/:
#   1. barangay_environmental_xgboost_H0_TO_H12_predictions_ENV_XGBOOST_H0_TO_H12.csv
#   2. barangay_environmental_xgboost_H0_TO_H12_metrics_ENV_XGBOOST_H0_TO_H12.csv
#   3. barangay_environmental_xgboost_H0_TO_H12_feature_list_ENV_XGBOOST_H0_TO_H12.csv
#   4. barangay_environmental_xgboost_H0_TO_H12_tuning_ENV_XGBOOST_PURE_REGRESSOR_WITH_CITY_NEIGHBOR_ENV_H0_TO_H12.csv
#   5. barangay_environmental_xgboost_H0_TO_H12_feature_importance_ENV_XGBOOST_H0_TO_H12.csv
#
# Notes for app integration:
#   - This creates ONLY:
#       environmental_xgboost_cases  = predicted dengue cases from the pure regressor
#   - No classifier, outbreak probability, or soft-gate correction is trained here.
#   - The selected week in the app can be treated as the ORIGIN week.
#     The script then gives predictions for that origin week and 1, 2, 3, 4, 8, and 12 weeks after.
# ============================================================


# ============================================================
# 0. USER SETTINGS
# ============================================================


# ============================================================
# PROJECT-RELATIVE PATHS FOR FINAL APP V2
# ============================================================
args0 <- commandArgs(trailingOnly = FALSE)
file_arg <- args0[grepl("^--file=", args0)]
SCRIPT_DIR <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE))
} else {
  getwd()
}
PROJECT_DIR <- if (basename(SCRIPT_DIR) == "r_scripts") {
  normalizePath(file.path(SCRIPT_DIR, ".."), mustWork = FALSE)
} else {
  normalizePath(SCRIPT_DIR, mustWork = FALSE)
}
setwd(PROJECT_DIR)

DATA_PATH <- file.path(PROJECT_DIR, "data", "FINAL_DATASET.xlsx")
SHAPE_ZIP_PATH <- file.path(PROJECT_DIR, "data", "cebu_city_barangays.zip")
OUTPUT_DIR <- file.path(PROJECT_DIR, "outputs")
MODEL_ROOT_DIR <- file.path(PROJECT_DIR, "models")
METADATA_DIR <- file.path(PROJECT_DIR, "model_metadata")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(MODEL_ROOT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(METADATA_DIR, showWarnings = FALSE, recursive = TRUE)


OUTPUT_TAG <- "ENV_XGBOOST_PURE_REGRESSOR_WITH_CITY_NEIGHBOR_ENV_H0_TO_H12"
MODEL_DIR <- file.path(MODEL_ROOT_DIR, "environmental_barangay")
dir.create(MODEL_DIR, showWarnings = FALSE, recursive = TRUE)

TRAIN_END_YEAR <- 2022
TEST_YEARS <- c(2023, 2024)

VALIDATION_YEARS <- c(2021, 2022)
TRAIN_CORE_END_YEAR <- min(VALIDATION_YEARS) - 1

OUTBREAK_COUNT_THRESHOLD <- 2
LARGE_OUTBREAK_THRESHOLD <- 5

MAX_LAG_TO_TEST <- 20
MAX_LAGS_KEPT_PER_ENV_VARIABLE <- 2
ENV_ROLL_WINDOWS <- c(4, 8)

# App-friendly display horizons.
# The model is still trained for all H+0 to H+12, but the app can show only these.
DISPLAY_HORIZONS <- c(0, 1, 2, 3, 4, 8, 12)
ALL_HORIZONS <- 0:12

POP_DENSITY_UNIT <- "per_km2"
USE_PEAK_WEIGHTS <- TRUE
SEED <- 123
set.seed(SEED)

# If TRUE, the model uses environmental lags/rolling weather + non-case static/spatial/seasonal fields.
# If FALSE, the model uses only lagged and rolling weather variables.
# This is TRUE because your instruction was: use everything EXCEPT autocorrelation/case features.
INCLUDE_STATIC_AND_SEASONAL_NON_CASE_FEATURES <- TRUE


# ============================================================
# 1. PACKAGES
# ============================================================

needed_packages <- c(
  "readxl", "dplyr", "tidyr", "stringr", "janitor", "purrr",
  "xgboost", "Matrix", "tibble", "zoo", "pROC", "sf", "spdep"
)

installed <- rownames(installed.packages())
for (p in needed_packages) {
  if (!(p %in% installed)) install.packages(p, dependencies = TRUE)
}

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(purrr)
library(xgboost)
library(Matrix)
library(tibble)
library(zoo)
library(pROC)
library(sf)
library(spdep)


# ============================================================
# 2. BARANGAY AREA TABLE
# ============================================================

area_text <- "
barangay,barangay_area_m2
Adlaon,10568289.49
Agsungot,3141145.937
Apas,1924622.783
Babag,9127305.301
Basak Pardo,2280016.225
Bacayan,1247466.647
Banilad,2361932.924
Basak San Nicolas,1482442.104
Binaliw,5160478.223
Bonbon,9335589.991
Budla-an (Pob.),5528415.047
Buhisan,7362349.884
Bulacao,4458308.598
Buot-Taup Pardo,6309902.571
Busay (Pob.),9073669.846
Calamba,461260.8167
Cambinocot,5404029.229
Capitol Site (Pob.),794266.9439
Carreta,944504.2462
Central (Pob.),295179.9903
Cogon Ramos (Pob.),295106.1074
Cogon Pardo,1534001.407
Day-as,106105.5372
Duljo (Pob.),403621.5959
Ermita (Pob.),379645.5962
Guadalupe,7364428.349
Guba,10821331.27
Hippodromo,430684.893
Inayawan,2753267.116
Kalubihan (Pob.),161379.8344
Kalunasan,1420118.552
Kamagayan (Pob.),117092.5693
Camputhaw (Pob.),1215505.455
Kasambagan,1832657.268
Kinasang-an Pardo,1509429.505
Labangon,1122041.116
Lahug (Pob.),4239264.538
Lorega (Lorega San Miguel),199630.1232
Lusaran,11205979.19
Luz,560240.7135
Mabini,5611102.651
Mabolo,1946308.241
Malubog,8555676.09
Mambaling,1249056.385
Pahina Central (Pob.),257598.4505
Pahina San Nicolas,75403.24046
Pamutan,11375185.98
Pardo (Pob.),2034673.842
Pari-an,97305.23716
Paril,3223325.36
Pasil,79951.38113
Pit-os,1653568.117
Pulangbato,5234284.92
Pung-ol-Sibugay,14616354.74
Punta Princesa,1276947.168
Quiot Pardo,761372.7629
Sambag I (Pob.),516257.1623
Sambag II (Pob.),447417.6939
San Antonio (Pob.),128340.3764
San Jose,2869390.311
San Nicolas Central,281010.9574
San Roque (Ciudad),455039.6854
Santa Cruz (Pob.),229062.3109
Sawang Calero (Pob.),242699.2115
Sinsin,8122060.059
Sirao,7704545.516
Suba Pob. (Suba San Nicolas),96693.99165
Sudlon I,3206182.493
Sapangdaku,8393868.225
T. Padilla,165106.4826
Tabunan,15861577.93
Tagbao,10534037.02
Talamban,4629709.911
Taptap,7570183.845
Tejero (Villa Gonzalo),538422.355
Tinago,229802.9203
Tisa,2436056.449
To-ong Pardo,6814618.803
Zapatera,335615.5369
Sudlon II,16116531.07
"

area_df <- read.csv(text = area_text, stringsAsFactors = FALSE) %>%
  mutate(barangay_clean = toupper(str_squish(barangay))) %>%
  select(barangay_clean, barangay_area_m2)


# ============================================================
# 3. BASIC HELPERS
# ============================================================

safe_rmse <- function(actual, pred) sqrt(mean((actual - pred)^2, na.rm = TRUE))
safe_mae <- function(actual, pred) mean(abs(actual - pred), na.rm = TRUE)

safe_r2 <- function(actual, pred) {
  ss_res <- sum((actual - pred)^2, na.rm = TRUE)
  ss_tot <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
  if (ss_tot == 0) return(NA_real_)
  1 - ss_res / ss_tot
}

safe_cor <- function(actual, pred) suppressWarnings(cor(actual, pred, use = "complete.obs"))

safe_auc <- function(actual_class, prob) {
  tryCatch(as.numeric(pROC::auc(actual_class, prob)), error = function(e) NA_real_)
}

cases_to_loginc <- function(cases, estimated_population) {
  incidence <- (cases / estimated_population) * 10000
  incidence <- ifelse(is.na(incidence) | incidence < 0, 0, incidence)
  log1p(incidence)
}

loginc_to_cases <- function(pred_log_incidence, estimated_population) {
  pred_incidence <- expm1(pred_log_incidence)
  pred_cases <- (pred_incidence / 10000) * estimated_population
  pred_cases <- ifelse(is.na(pred_cases), 0, pred_cases)
  pmax(pred_cases, 0)
}

classification_metrics_from_cases <- function(actual_cases, pred_cases, threshold) {
  actual_class <- actual_cases >= threshold
  pred_class <- pred_cases >= threshold
  
  tp <- sum(actual_class & pred_class, na.rm = TRUE)
  tn <- sum(!actual_class & !pred_class, na.rm = TRUE)
  fp <- sum(!actual_class & pred_class, na.rm = TRUE)
  fn <- sum(actual_class & !pred_class, na.rm = TRUE)
  
  precision <- ifelse((tp + fp) == 0, NA_real_, tp / (tp + fp))
  recall <- ifelse((tp + fn) == 0, NA_real_, tp / (tp + fn))
  f1 <- ifelse(is.na(precision) | is.na(recall) | (precision + recall) == 0, NA_real_, 2 * precision * recall / (precision + recall))
  false_alarm_rate <- ifelse((fp + tn) == 0, NA_real_, fp / (fp + tn))
  specificity <- ifelse((tn + fp) == 0, NA_real_, tn / (tn + fp))
  
  tibble(
    Precision = precision,
    Recall = recall,
    Outbreak_F1_count_threshold = f1,
    False_alarm_rate = false_alarm_rate,
    Specificity = specificity,
    TP = tp,
    TN = tn,
    FP = fp,
    FN = fn
  )
}

classification_metrics_from_prob <- function(actual_cases, prob, prob_threshold, outbreak_threshold) {
  actual_class <- actual_cases >= outbreak_threshold
  pred_class <- prob >= prob_threshold
  
  tp <- sum(actual_class & pred_class, na.rm = TRUE)
  tn <- sum(!actual_class & !pred_class, na.rm = TRUE)
  fp <- sum(!actual_class & pred_class, na.rm = TRUE)
  fn <- sum(actual_class & !pred_class, na.rm = TRUE)
  
  precision <- ifelse((tp + fp) == 0, NA_real_, tp / (tp + fp))
  recall <- ifelse((tp + fn) == 0, NA_real_, tp / (tp + fn))
  f1 <- ifelse(is.na(precision) | is.na(recall) | (precision + recall) == 0, NA_real_, 2 * precision * recall / (precision + recall))
  false_alarm_rate <- ifelse((fp + tn) == 0, NA_real_, fp / (fp + tn))
  specificity <- ifelse((tn + fp) == 0, NA_real_, tn / (tn + fp))
  
  tibble(
    Alert_threshold = prob_threshold,
    Alert_Precision = precision,
    Alert_Recall = recall,
    Alert_F1 = f1,
    Alert_False_alarm_rate = false_alarm_rate,
    Alert_Specificity = specificity,
    Alert_TP = tp,
    Alert_TN = tn,
    Alert_FP = fp,
    Alert_FN = fn,
    Alert_AUC = safe_auc(as.integer(actual_class), prob),
    Brier_score = mean((prob - as.integer(actual_class))^2, na.rm = TRUE)
  )
}

subset_error_metrics <- function(eval_df, threshold, prefix) {
  sub <- eval_df %>% filter(actual_cases >= threshold)
  
  if (nrow(sub) == 0) {
    return(tibble(
      !!paste0(prefix, "_RMSE_raw") := NA_real_,
      !!paste0(prefix, "_MAE_raw") := NA_real_,
      !!paste0(prefix, "_Bias_raw") := NA_real_,
      !!paste0(prefix, "_Underprediction_rate") := NA_real_
    ))
  }
  
  tibble(
    !!paste0(prefix, "_RMSE_raw") := safe_rmse(sub$actual_cases, sub$pred_cases),
    !!paste0(prefix, "_MAE_raw") := safe_mae(sub$actual_cases, sub$pred_cases),
    !!paste0(prefix, "_Bias_raw") := mean(sub$pred_cases - sub$actual_cases, na.rm = TRUE),
    !!paste0(prefix, "_Underprediction_rate") := mean(sub$pred_cases < sub$actual_cases, na.rm = TRUE)
  )
}

alert_level_from_probability <- function(probability) {
  case_when(
    is.na(probability) ~ "Unknown",
    probability < 0.30 ~ "Low",
    probability < 0.50 ~ "Watch",
    probability < 0.70 ~ "Moderate",
    probability < 0.85 ~ "High",
    TRUE ~ "Very High"
  )
}

make_reg_weights <- function(cases) {
  if (!USE_PEAK_WEIGHTS) return(rep(1, length(cases)))
  case_when(
    cases >= 10 ~ 8,
    cases >= LARGE_OUTBREAK_THRESHOLD ~ 5,
    cases >= OUTBREAK_COUNT_THRESHOLD ~ 3,
    cases > 0 ~ 1.5,
    TRUE ~ 1
  )
}

make_class_weights <- function(outbreak_actual) {
  if (!USE_PEAK_WEIGHTS) return(rep(1, length(outbreak_actual)))
  ifelse(outbreak_actual == 1, 3, 1)
}

make_lags <- function(data, cols, lags, group_col = "barangay") {
  out <- data
  for (cc in cols) {
    if (cc %in% names(out)) {
      for (ll in lags) {
        new_name <- paste0(cc, "_lag", ll)
        out <- out %>%
          group_by(.data[[group_col]]) %>%
          arrange(year, week, .by_group = TRUE) %>%
          mutate(!!new_name := dplyr::lag(.data[[cc]], ll)) %>%
          ungroup()
      }
    }
  }
  out
}

make_rolls <- function(data, cols, windows, group_col = "barangay") {
  out <- data
  for (cc in cols) {
    if (cc %in% names(out)) {
      for (ww in windows) {
        new_name <- paste0(cc, "_rollmean_lag1_w", ww)
        out <- out %>%
          group_by(.data[[group_col]]) %>%
          arrange(year, week, .by_group = TRUE) %>%
          mutate(
            !!new_name := zoo::rollapplyr(
              dplyr::lag(.data[[cc]], 1),
              width = ww,
              FUN = function(z) mean(z, na.rm = TRUE),
              fill = NA_real_,
              partial = FALSE
            )
          ) %>%
          ungroup()
      }
    }
  }
  out
}

make_future_targets <- function(data, horizon) {
  data %>%
    group_by(barangay) %>%
    arrange(year, week, .by_group = TRUE) %>%
    mutate(
      origin_year = year,
      origin_week = week,
      forecast_horizon = horizon,
      forecast_horizon_label = paste0("H+", horizon),
      target_year_h = dplyr::lead(year, horizon),
      target_week_h = dplyr::lead(week, horizon),
      target_cases_h = dplyr::lead(dengue_cases, horizon),
      target_population_h = dplyr::lead(estimated_population, horizon),
      target_log_incidence_h = cases_to_loginc(target_cases_h, target_population_h),
      target_outbreak_h = as.integer(target_cases_h >= OUTBREAK_COUNT_THRESHOLD),
      target_large_outbreak_h = as.integer(target_cases_h >= LARGE_OUTBREAK_THRESHOLD)
    ) %>%
    ungroup()
}

is_case_related_feature <- function(feature_names) {
  str_detect(
    feature_names,
    regex(
      paste(
        c(
          "dengue", "case", "cases", "incidence", "target", "outbreak",
          "lagged_case", "positive"
        ),
        collapse = "|"
      ),
      ignore_case = TRUE
    )
  )
}

make_matrix <- function(train_data, pred_data, features) {
  train_x_raw <- train_data %>% select(all_of(features))
  pred_x_raw <- pred_data %>% select(all_of(features))
  
  combined <- bind_rows(train_x_raw, pred_x_raw) %>%
    mutate(across(where(is.character), as.factor)) %>%
    mutate(across(where(is.logical), as.integer))
  
  mm <- model.matrix(~ . - 1, data = combined)
  
  train_mat <- mm[1:nrow(train_x_raw), , drop = FALSE]
  pred_mat <- mm[(nrow(train_x_raw) + 1):nrow(mm), , drop = FALSE]
  
  list(train = train_mat, pred = pred_mat, feature_names = colnames(mm))
}

select_best_env_lags_train_only <- function(data, env_vars, max_lag = 20, keep_per_var = 2) {
  train_only <- data %>% filter(year <= TRAIN_END_YEAR)
  selected <- c()
  reports <- list()
  
  for (v in env_vars) {
    lag_cols <- paste0(v, "_lag", 1:max_lag)
    lag_cols <- lag_cols[lag_cols %in% names(train_only)]
    if (length(lag_cols) == 0) next
    
    scores <- purrr::map_df(lag_cols, function(col) {
      ok <- !is.na(train_only[[col]]) &
        !is.na(train_only$target_log_incidence) &
        !is.na(train_only$dengue_cases) &
        !is.na(train_only$outbreak_actual) &
        !is.na(train_only$large_outbreak_actual)
      
      if (sum(ok) < 30 || sd(train_only[[col]][ok], na.rm = TRUE) == 0) {
        return(tibble(
          variable = v,
          feature = col,
          lag = as.integer(str_extract(col, "\\d+$")),
          cor_log_incidence = NA_real_,
          cor_cases = NA_real_,
          cor_outbreak = NA_real_,
          cor_large_outbreak = NA_real_,
          selection_score = NA_real_
        ))
      }
      
      x <- train_only[[col]][ok]
      y_log <- train_only$target_log_incidence[ok]
      y_cases <- train_only$dengue_cases[ok]
      y_out <- train_only$outbreak_actual[ok]
      y_large <- train_only$large_outbreak_actual[ok]
      
      cor_log <- suppressWarnings(abs(cor(x, y_log, use = "complete.obs")))
      cor_cases <- suppressWarnings(abs(cor(x, y_cases, use = "complete.obs")))
      cor_out <- suppressWarnings(abs(cor(x, y_out, use = "complete.obs")))
      cor_large <- suppressWarnings(abs(cor(x, y_large, use = "complete.obs")))
      
      score <- 0.40 * ifelse(is.na(cor_log), 0, cor_log) +
        0.35 * ifelse(is.na(cor_cases), 0, cor_cases) +
        0.15 * ifelse(is.na(cor_out), 0, cor_out) +
        0.10 * ifelse(is.na(cor_large), 0, cor_large)
      
      tibble(
        variable = v,
        feature = col,
        lag = as.integer(str_extract(col, "\\d+$")),
        cor_log_incidence = cor_log,
        cor_cases = cor_cases,
        cor_outbreak = cor_out,
        cor_large_outbreak = cor_large,
        selection_score = score
      )
    }) %>%
      arrange(desc(selection_score), lag)
    
    chosen <- scores %>%
      filter(!is.na(selection_score)) %>%
      slice_head(n = keep_per_var) %>%
      pull(feature)
    
    selected <- c(selected, chosen)
    reports[[v]] <- scores %>% mutate(selected = feature %in% chosen)
  }
  
  list(
    selected_features = unique(selected),
    lag_report = bind_rows(reports)
  )
}

classification_threshold_tuning <- function(actual_cases, prob) {
  thresholds <- seq(0.20, 0.85, by = 0.05)
  
  out <- purrr::map_df(thresholds, function(th) {
    m <- classification_metrics_from_prob(
      actual_cases = actual_cases,
      prob = prob,
      prob_threshold = th,
      outbreak_threshold = OUTBREAK_COUNT_THRESHOLD
    )
    
    score <- (1 - ifelse(is.na(m$Alert_Recall), 0, m$Alert_Recall)) +
      2.5 * ifelse(is.na(m$Alert_False_alarm_rate), 0, m$Alert_False_alarm_rate) +
      0.25 * (1 - ifelse(is.na(m$Alert_Precision), 0, m$Alert_Precision))
    
    bind_cols(tibble(threshold = th, threshold_score = score), m)
  })
  
  out %>% arrange(threshold_score)
}


# ============================================================
# 4. READ DATA AND CREATE BASE VARIABLES
# ============================================================

if (!file.exists(DATA_PATH)) {
  stop(paste0("DATA_PATH not found: ", DATA_PATH, "\nEdit DATA_PATH at the top of this script."))
}
if (!file.exists(SHAPE_ZIP_PATH)) {
  stop(paste0("SHAPE_ZIP_PATH not found: ", SHAPE_ZIP_PATH, "\nEdit SHAPE_ZIP_PATH at the top of this script."))
}

raw_df <- read_excel(DATA_PATH) %>% clean_names()

cat("\nColumns found in dataset:\n")
print(names(raw_df))

required_cols <- c("barangay", "year", "week", "pop_density", "dengue_cases")
missing_cols <- setdiff(required_cols, names(raw_df))
if (length(missing_cols) > 0) {
  stop(paste0("Missing required columns: ", paste(missing_cols, collapse = ", ")))
}

df <- raw_df %>%
  mutate(
    barangay = toupper(str_squish(as.character(barangay))),
    year = as.integer(year),
    week = as.integer(week),
    dengue_cases = as.numeric(dengue_cases),
    pop_density = as.numeric(pop_density)
  ) %>%
  left_join(area_df, by = c("barangay" = "barangay_clean"))

if (any(is.na(df$barangay_area_m2))) {
  cat("\nBarangays without matched area:\n")
  print(unique(df$barangay[is.na(df$barangay_area_m2)]))
  stop("Some barangays did not match the area table.")
}

df <- df %>%
  mutate(barangay_area_km2 = barangay_area_m2 / 1e6)

if (POP_DENSITY_UNIT == "per_km2") {
  df <- df %>% mutate(estimated_population = pop_density * barangay_area_km2)
} else if (POP_DENSITY_UNIT == "per_m2") {
  df <- df %>% mutate(estimated_population = pop_density * barangay_area_m2)
} else {
  stop("POP_DENSITY_UNIT must be either 'per_km2' or 'per_m2'.")
}

df <- df %>%
  mutate(
    estimated_population = ifelse(estimated_population <= 0 | is.na(estimated_population), NA_real_, estimated_population),
    dengue_incidence_10000 = (dengue_cases / estimated_population) * 10000,
    dengue_incidence_10000 = ifelse(is.na(dengue_incidence_10000) | dengue_incidence_10000 < 0, 0, dengue_incidence_10000),
    target_log_incidence = log1p(dengue_incidence_10000),
    outbreak_actual = as.integer(dengue_cases >= OUTBREAK_COUNT_THRESHOLD),
    large_outbreak_actual = as.integer(dengue_cases >= LARGE_OUTBREAK_THRESHOLD),
    week_sin = sin(2 * pi * week / 52),
    week_cos = cos(2 * pi * week / 52)
  ) %>%
  arrange(barangay, year, week)


# ============================================================
# 5. ENVIRONMENTAL FEATURE ENGINEERING ONLY
#    Barangay weather + neighboring weather + citywide weather
# ============================================================

env_candidates <- c(
  "rainfall", "rh", "humidity", "relative_humidity",
  "temp_c", "temperature", "t_mean", "tmin", "tmax", "t_min", "t_max",
  "u_component_of_wind_10m", "v_component_of_wind_10m", "wind_speed_10m", "wind_speed",
  "flood_depth", "flood_duration", "flood_extent", "water_level"
)

env_vars <- env_candidates[env_candidates %in% names(df)]
cat("\nBarangay environmental variables found and used for lag engineering:\n")
print(env_vars)

if (length(env_vars) == 0) {
  stop("No environmental variables were found in the dataset. Check column names after clean_names().")
}

# ------------------------------------------------------------
# 5A. Barangay adjacency for neighboring environmental summaries
# ------------------------------------------------------------

shape_dir <- file.path(tempdir(), "cebu_city_barangays_shape_env_xgb")
dir.create(shape_dir, showWarnings = FALSE, recursive = TRUE)
unzip(SHAPE_ZIP_PATH, exdir = shape_dir)

shp_file <- list.files(shape_dir, pattern = "\\.shp$", full.names = TRUE)[1]
if (is.na(shp_file)) stop("No .shp file found inside the shapefile zip.")

barangay_sf <- st_read(shp_file, quiet = TRUE) %>% clean_names()

possible_name_cols <- c("barangay", "brgy", "name", "adm4_en", "adm4_name", "bgy_name", "barangay_n", "brgy_name")
name_col <- possible_name_cols[possible_name_cols %in% names(barangay_sf)][1]
if (is.na(name_col)) stop("Could not detect barangay name column in shapefile.")

barangay_sf <- barangay_sf %>%
  mutate(barangay = toupper(str_squish(as.character(.data[[name_col]])))) %>%
  filter(barangay %in% unique(df$barangay)) %>%
  arrange(barangay)

barangay_sf <- st_make_valid(barangay_sf)

nb <- poly2nb(barangay_sf, queen = TRUE)
names(nb) <- barangay_sf$barangay

neighbor_pairs <- tibble(
  barangay = rep(names(nb), lengths(nb)),
  neighbor = names(nb)[unlist(nb)]
) %>%
  filter(!is.na(neighbor))

cat("\nNeighbor pairs created for environmental summaries:", nrow(neighbor_pairs), "\n")

# ------------------------------------------------------------
# 5B. Neighboring environmental values
#     For each barangay-week, get mean weather of adjacent barangays.
#     These are environmental only, not dengue/case variables.
# ------------------------------------------------------------

neighbor_env <- df %>%
  select(year, week, neighbor = barangay, all_of(env_vars)) %>%
  inner_join(neighbor_pairs, by = "neighbor") %>%
  group_by(barangay, year, week) %>%
  summarise(
    across(
      all_of(env_vars),
      ~ mean(.x, na.rm = TRUE),
      .names = "neighbor_env_{.col}_mean"
    ),
    .groups = "drop"
  )

neighbor_env_vars <- names(neighbor_env)[str_detect(names(neighbor_env), "^neighbor_env_")]

# ------------------------------------------------------------
# 5C. Citywide environmental values
#     For each week, get mean weather across Cebu City barangays.
#     These are environmental only, not citywide dengue variables.
# ------------------------------------------------------------

city_env <- df %>%
  group_by(year, week) %>%
  summarise(
    across(
      all_of(env_vars),
      ~ mean(.x, na.rm = TRUE),
      .names = "city_env_{.col}_mean"
    ),
    .groups = "drop"
  )

city_env_vars <- names(city_env)[str_detect(names(city_env), "^city_env_")]

# ------------------------------------------------------------
# 5D. Combine barangay, neighbor, and city environmental fields
# ------------------------------------------------------------

df_env <- df %>%
  left_join(neighbor_env, by = c("barangay", "year", "week")) %>%
  left_join(city_env, by = c("year", "week"))

all_env_base_vars <- unique(c(env_vars, neighbor_env_vars, city_env_vars))
all_env_base_vars <- all_env_base_vars[all_env_base_vars %in% names(df_env)]
all_env_base_vars <- all_env_base_vars[!is_case_related_feature(all_env_base_vars)]

cat("\nAll environmental base variables, including barangay, neighbor, and citywide weather:\n")
print(all_env_base_vars)
cat("\nNumber of environmental base variables:", length(all_env_base_vars), "\n")

static_candidates <- c(
  "barangay", "week_sin", "week_cos",
  "pop_density", "barangay_area_m2", "barangay_area_km2",
  "flood_risk_index", "barangay_classification",
  names(df)[str_detect(names(df), "^percent_|^x_percent_|annual_crop|brush|built|forest|crop|barren|fishpond|grassland|mangrove|water|landcover")]
)

static_non_case_vars <- unique(static_candidates[static_candidates %in% names(df_env)])
static_non_case_vars <- static_non_case_vars[!is_case_related_feature(static_non_case_vars)]

df_env <- df_env %>%
  make_lags(all_env_base_vars, 1:MAX_LAG_TO_TEST) %>%
  make_rolls(all_env_base_vars, ENV_ROLL_WINDOWS)

env_lag_selection <- select_best_env_lags_train_only(
  data = df_env,
  env_vars = all_env_base_vars,
  max_lag = MAX_LAG_TO_TEST,
  keep_per_var = MAX_LAGS_KEPT_PER_ENV_VARIABLE
)

selected_env_lag_features <- env_lag_selection$selected_features
roll_feature_cols <- names(df_env)[str_detect(names(df_env), "_rollmean_lag1_w")]
roll_feature_cols <- roll_feature_cols[!is_case_related_feature(roll_feature_cols)]

if (INCLUDE_STATIC_AND_SEASONAL_NON_CASE_FEATURES) {
  feature_cols_base <- unique(c(static_non_case_vars, selected_env_lag_features, roll_feature_cols))
} else {
  feature_cols_base <- unique(c(selected_env_lag_features, roll_feature_cols))
}

# Extra safety filter: remove any case/autocorrelation feature.
feature_cols_base <- feature_cols_base[feature_cols_base %in% names(df_env)]
feature_cols_base <- feature_cols_base[!is_case_related_feature(feature_cols_base)]
feature_cols_base <- setdiff(
  feature_cols_base,
  c(
    "dengue_cases", "dengue_incidence_10000", "target_log_incidence",
    "outbreak_actual", "large_outbreak_actual", "estimated_population"
  )
)

if (length(feature_cols_base) == 0) {
  stop("No environmental / non-case features available after filtering. Check feature engineering section.")
}

cat("\nFinal environmental/non-autocorrelation feature columns:\n")
print(feature_cols_base)
cat("\nNumber of final environmental/non-autocorrelation features:", length(feature_cols_base), "\n")

write.csv(
  tibble(feature = feature_cols_base),
  file.path(OUTPUT_DIR, paste0("barangay_environmental_xgboost_H0_TO_H12_feature_list_", OUTPUT_TAG, ".csv")),
  row.names = FALSE
)

write.csv(
  env_lag_selection$lag_report,
  file.path(OUTPUT_DIR, paste0("barangay_environmental_xgboost_lag_selection_", OUTPUT_TAG, ".csv")),
  row.names = FALSE
)

# ============================================================


# ============================================================
# 6. XGBOOST TUNING AND TRAINING HELPERS
# ============================================================

xgb_regression_grid <- tibble::tribble(
  ~eta, ~max_depth, ~min_child_weight, ~subsample, ~colsample_bytree, ~lambda, ~alpha,
  0.03, 3, 3, 0.85, 0.85, 1.0, 0.05,
  0.03, 4, 5, 0.85, 0.85, 1.0, 0.05,
  0.02, 4, 5, 0.90, 0.80, 1.5, 0.10,
  0.05, 3, 5, 0.85, 0.85, 1.0, 0.05,
  0.05, 4, 8, 0.80, 0.80, 2.0, 0.10,
  0.03, 5, 8, 0.80, 0.80, 2.0, 0.20
)

get_best_nrounds <- function(fit, fallback_nrounds = 500) {
  best_iter <- fit$best_iteration
  
  if (is.null(best_iter) || length(best_iter) == 0 || is.na(best_iter)) {
    best_iter <- fit$best_ntreelimit
  }
  
  if (is.null(best_iter) || length(best_iter) == 0 || is.na(best_iter) || best_iter < 50) {
    best_iter <- fallback_nrounds
  }
  
  as.integer(best_iter)
}

tune_xgb_regressor <- function(train_core, validation, features, horizon_label) {
  mats <- make_matrix(train_core, validation, features)
  x_train <- mats$train
  x_val <- mats$pred
  
  dtrain <- xgb.DMatrix(
    data = x_train,
    label = train_core$target_log_incidence_h,
    weight = make_reg_weights(train_core$target_cases_h)
  )
  
  dval <- xgb.DMatrix(data = x_val, label = validation$target_log_incidence_h)
  
  tuning_results <- purrr::map_df(seq_len(nrow(xgb_regression_grid)), function(i) {
    g <- xgb_regression_grid[i, ]
    
    fit <- xgb.train(
      params = list(
        objective = "reg:squarederror",
        eval_metric = "rmse",
        eta = g$eta,
        max_depth = g$max_depth,
        min_child_weight = g$min_child_weight,
        subsample = g$subsample,
        colsample_bytree = g$colsample_bytree,
        lambda = g$lambda,
        alpha = g$alpha
      ),
      data = dtrain,
      nrounds = 1000,
      evals = list(train = dtrain, validation = dval),
      early_stopping_rounds = 40,
      verbose = 0
    )
    
    pred_log <- predict(fit, dval)
    pred_cases <- loginc_to_cases(pred_log, validation$target_population_h)
    
    tibble(
      forecast_horizon_label = horizon_label,
      model_part = "regressor",
      grid_id = i,
      eta = g$eta,
      max_depth = g$max_depth,
      min_child_weight = g$min_child_weight,
      subsample = g$subsample,
      colsample_bytree = g$colsample_bytree,
      lambda = g$lambda,
      alpha = g$alpha,
      best_nrounds = get_best_nrounds(fit),
      validation_RMSE_raw = safe_rmse(validation$target_cases_h, pred_cases),
      validation_MAE_raw = safe_mae(validation$target_cases_h, pred_cases),
      validation_R2_raw = safe_r2(validation$target_cases_h, pred_cases),
      validation_Correlation_raw = safe_cor(validation$target_cases_h, pred_cases)
    )
  })
  
  tuning_results %>% arrange(validation_RMSE_raw)
}

fit_final_xgb_regressor <- function(train_data, pred_data, features, best_params) {
  mats <- make_matrix(train_data, pred_data, features)
  
  dtrain <- xgb.DMatrix(
    data = mats$train,
    label = train_data$target_log_incidence_h,
    weight = make_reg_weights(train_data$target_cases_h)
  )
  dpred <- xgb.DMatrix(data = mats$pred)
  
  nrounds <- if ("best_nrounds" %in% names(best_params)) best_params$best_nrounds[1] else 500
  if (is.null(nrounds) || is.na(nrounds) || nrounds < 50) nrounds <- 500
  
  fit <- xgb.train(
    params = list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      eta = best_params$eta[1],
      max_depth = best_params$max_depth[1],
      min_child_weight = best_params$min_child_weight[1],
      subsample = best_params$subsample[1],
      colsample_bytree = best_params$colsample_bytree[1],
      lambda = best_params$lambda[1],
      alpha = best_params$alpha[1]
    ),
    data = dtrain,
    nrounds = nrounds,
    verbose = 0
  )
  
  pred_log <- predict(fit, dpred)
  pred_cases <- loginc_to_cases(pred_log, pred_data$target_population_h)
  
  list(
    fit = fit,
    pred_log = pred_log,
    pred_cases = pmax(pred_cases, 0),
    matrix_feature_names = mats$feature_names
  )
}

evaluate_pure_regressor_horizon <- function(horizon_label, base_df, pred_cases) {
  pred_cases <- pmax(pred_cases, 0)
  
  eval_df <- base_df %>%
    mutate(
      actual_cases = target_cases_h,
      pred_cases = pred_cases,
      actual_log_incidence = target_log_incidence_h,
      pred_log_incidence = cases_to_loginc(pred_cases, target_population_h)
    )
  
  reg_metrics <- tibble(
    Model = "Pure environmental XGBoost regressor",
    Horizon = horizon_label,
    forecast_horizon = unique(base_df$forecast_horizon)[1],
    forecast_horizon_label = horizon_label,
    RMSE_raw = safe_rmse(eval_df$actual_cases, eval_df$pred_cases),
    MAE_raw = safe_mae(eval_df$actual_cases, eval_df$pred_cases),
    R2_raw = safe_r2(eval_df$actual_cases, eval_df$pred_cases),
    Correlation_raw = safe_cor(eval_df$actual_cases, eval_df$pred_cases),
    Bias_raw = mean(eval_df$pred_cases - eval_df$actual_cases, na.rm = TRUE),
    Mean_pred_raw = mean(eval_df$pred_cases, na.rm = TRUE),
    Mean_actual_raw = mean(eval_df$actual_cases, na.rm = TRUE),
    RMSE_log_incidence = safe_rmse(eval_df$actual_log_incidence, eval_df$pred_log_incidence),
    MAE_log_incidence = safe_mae(eval_df$actual_log_incidence, eval_df$pred_log_incidence),
    R2_log_incidence = safe_r2(eval_df$actual_log_incidence, eval_df$pred_log_incidence),
    Correlation_log_incidence = safe_cor(eval_df$actual_log_incidence, eval_df$pred_log_incidence)
  )
  
  case_cls <- classification_metrics_from_cases(
    actual_cases = eval_df$actual_cases,
    pred_cases = eval_df$pred_cases,
    threshold = OUTBREAK_COUNT_THRESHOLD
  )
  
  outbreak_errors <- subset_error_metrics(eval_df, OUTBREAK_COUNT_THRESHOLD, "Outbreak_week")
  large_errors <- subset_error_metrics(eval_df, LARGE_OUTBREAK_THRESHOLD, "Large_outbreak")
  
  bind_cols(reg_metrics, case_cls, outbreak_errors, large_errors)
}



# ============================================================
# 7. LOOP THROUGH H+0 TO H+12
# ============================================================

all_predictions <- list()
all_metrics <- list()
all_tuning <- list()
all_importance <- list()

for (h in ALL_HORIZONS) {
  horizon_label <- paste0("H+", h)
  
  cat("
==============================
")
  cat("TRAINING PURE ENVIRONMENTAL XGBOOST REGRESSOR FOR ", horizon_label, "
", sep = "")
  cat("==============================
")
  
  horizon_df <- make_future_targets(df_env, h) %>%
    filter(
      !is.na(target_year_h),
      !is.na(target_week_h),
      !is.na(target_cases_h),
      !is.na(target_population_h),
      !is.na(target_log_incidence_h),
      !is.na(target_outbreak_h),
      if_all(all_of(feature_cols_base), ~ !is.na(.x))
    ) %>%
    mutate(
      barangay = as.factor(barangay),
      barangay_classification = if ("barangay_classification" %in% names(.)) as.factor(barangay_classification) else factor("unknown")
    )
  
  train_core_df <- horizon_df %>% filter(origin_year <= TRAIN_CORE_END_YEAR)
  validation_df <- horizon_df %>% filter(origin_year %in% VALIDATION_YEARS)
  train_df <- horizon_df %>% filter(origin_year <= TRAIN_END_YEAR)
  test_df <- horizon_df %>% filter(origin_year %in% TEST_YEARS)
  
  cat("Rows - train_core:", nrow(train_core_df), "validation:", nrow(validation_df), "train:", nrow(train_df), "test:", nrow(test_df), "
")
  
  if (nrow(train_core_df) == 0 || nrow(validation_df) == 0 || nrow(train_df) == 0 || nrow(test_df) == 0) {
    warning(paste0("Skipping ", horizon_label, " because one split is empty."))
    next
  }
  
  reg_tuning <- tune_xgb_regressor(train_core_df, validation_df, feature_cols_base, horizon_label)
  best_reg <- reg_tuning %>% slice_head(n = 1)
  all_tuning[[horizon_label]] <- reg_tuning
  
  cat("Best pure regressor validation RMSE:", best_reg$validation_RMSE_raw[1], "
")
  
  final_reg <- fit_final_xgb_regressor(train_df, test_df, feature_cols_base, best_reg)
  
  metrics_h <- evaluate_pure_regressor_horizon(
    horizon_label = horizon_label,
    base_df = test_df,
    pred_cases = final_reg$pred_cases
  )
  
  all_metrics[[horizon_label]] <- metrics_h
  
  predictions_h <- test_df %>%
    select(
      barangay,
      origin_year,
      origin_week,
      target_year_h,
      target_week_h,
      forecast_horizon,
      forecast_horizon_label,
      actual_cases = target_cases_h,
      actual_log_incidence = target_log_incidence_h,
      target_population_h
    ) %>%
    mutate(
      Horizon = forecast_horizon_label,
      Horizon_number = forecast_horizon,
      display_in_app_default = forecast_horizon %in% DISPLAY_HORIZONS,
      environmental_xgboost_cases = final_reg$pred_cases,
      predicted_cases = final_reg$pred_cases,
      predicted_log_incidence = final_reg$pred_log,
      outbreak_probability = NA_real_,
      alert_threshold = NA_real_,
      alert_level = NA_character_,
      model_type = "Pure environmental-only XGBoost regressor",
      uses_autocorrelation_features = FALSE,
      includes_neighboring_environmental_features = TRUE,
      includes_citywide_environmental_features = TRUE
    )
  
  all_predictions[[horizon_label]] <- predictions_h
  
  imp_reg <- xgb.importance(
    feature_names = final_reg$matrix_feature_names,
    model = final_reg$fit
  ) %>%
    as_tibble() %>%
    mutate(
      forecast_horizon_label = horizon_label,
      model_part = "regressor"
    )
  
  all_importance[[horizon_label]] <- imp_reg

  reg_bundle <- list(
    model = final_reg$fit,
    model_family = "environmental_barangay_xgboost",
    model_part = "regressor",
    horizon = h,
    horizon_label = horizon_label,
    feature_cols = feature_cols_base,
    matrix_feature_names = final_reg$matrix_feature_names,
    best_params = as.data.frame(best_reg),
    target = "target_log_incidence_h",
    uses_autocorrelation_features = FALSE,
    trained_on_years = paste0("<=", TRAIN_END_YEAR)
  )

  saveRDS(reg_bundle, file.path(MODEL_DIR, paste0("h", h, "_regressor.rds")))
  saveRDS(
    list(
      feature_cols = feature_cols_base,
      matrix_feature_names = final_reg$matrix_feature_names,
      horizon = h,
      horizon_label = horizon_label,
      uses_autocorrelation_features = FALSE
    ),
    file.path(MODEL_DIR, paste0("h", h, "_feature_spec.rds"))
  )
}


# ============================================================
# 8. SAVE OUTPUTS
# ============================================================

final_predictions <- bind_rows(all_predictions) %>%
  arrange(origin_year, origin_week, forecast_horizon, barangay)

final_metrics <- bind_rows(all_metrics) %>%
  arrange(forecast_horizon)

final_tuning <- bind_rows(all_tuning) %>%
  arrange(forecast_horizon_label, model_part)

final_importance <- bind_rows(all_importance) %>%
  arrange(forecast_horizon_label, model_part, desc(Gain))

pred_file <- file.path(
  OUTPUT_DIR,
  paste0("barangay_environmental_xgboost_H0_TO_H12_predictions_", OUTPUT_TAG, ".csv")
)

metrics_file <- file.path(
  OUTPUT_DIR,
  paste0("barangay_environmental_xgboost_H0_TO_H12_metrics_", OUTPUT_TAG, ".csv")
)

tuning_file <- file.path(
  OUTPUT_DIR,
  paste0("barangay_environmental_xgboost_H0_TO_H12_tuning_", OUTPUT_TAG, ".csv")
)

importance_file <- file.path(
  OUTPUT_DIR,
  paste0("barangay_environmental_xgboost_H0_TO_H12_feature_importance_", OUTPUT_TAG, ".csv")
)

write.csv(final_predictions, pred_file, row.names = FALSE)
write.csv(final_metrics, metrics_file, row.names = FALSE)
write.csv(final_tuning, tuning_file, row.names = FALSE)
write.csv(final_importance, importance_file, row.names = FALSE)

write.csv(final_metrics, file.path(METADATA_DIR, "environmental_barangay_metrics.csv"), row.names = FALSE)
write.csv(final_tuning, file.path(METADATA_DIR, "environmental_barangay_tuning.csv"), row.names = FALSE)
write.csv(final_importance, file.path(METADATA_DIR, "environmental_barangay_feature_importance.csv"), row.names = FALSE)

cat("
==============================
")
cat("DONE: PURE ENVIRONMENTAL XGBOOST REGRESSOR H+0 TO H+12 COMPLETED
")
cat("==============================
")

cat("
Saved files:
")
cat("- ", pred_file, "
", sep = "")
cat("- ", metrics_file, "
", sep = "")
cat("- ", tuning_file, "
", sep = "")
cat("- ", importance_file, "
", sep = "")
cat("- ", file.path(OUTPUT_DIR, paste0("barangay_environmental_xgboost_H0_TO_H12_feature_list_", OUTPUT_TAG, ".csv")), "
", sep = "")

cat("
Reminder for app UX:
")
cat("Use origin_year + origin_week as the selected week.
")
cat("Then display forecast_horizon values: ", paste(DISPLAY_HORIZONS, collapse = ", "), "
", sep = "")
cat("This corresponds to: selected week, +1, +2, +3, +4, +8, and +12 weeks.
")
cat("This script contains ONLY the pure regressor branch from the winning environmental-only code.
")
