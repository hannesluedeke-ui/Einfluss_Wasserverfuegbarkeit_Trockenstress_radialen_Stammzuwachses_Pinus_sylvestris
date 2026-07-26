# =========================================================
# 05_linear_models.R
# Lineare Modelle für GI
# =========================================================

source("./Skripte/00_setup.R")

analysis_data    <- readRDS("./data_out/analysis_data.rds")
analysis_positive <- readRDS("./data_out/analysis_positive.rds")

# Einfaches lineares Modell
model_linear_0 <- lm(
  GI_daily_inc_mean ~ temp_mean + vpd_mean + precip_sum,
  data = analysis_data
)

summary(model_linear_0)

# Lags 1 bis 3
analysis_data$temp_max_lag1   <- dplyr::lag(analysis_data$temp_max, 1)
analysis_data$vpd_max_lag1    <- dplyr::lag(analysis_data$vpd_max, 1)
analysis_data$precip_sum_lag1 <- dplyr::lag(analysis_data$precip_sum, 1)

analysis_data$temp_max_lag2   <- dplyr::lag(analysis_data$temp_max, 2)
analysis_data$vpd_max_lag2    <- dplyr::lag(analysis_data$vpd_max, 2)
analysis_data$precip_sum_lag2 <- dplyr::lag(analysis_data$precip_sum, 2)

analysis_data$temp_max_lag3   <- dplyr::lag(analysis_data$temp_max, 3)
analysis_data$vpd_max_lag3    <- dplyr::lag(analysis_data$vpd_max, 3)
analysis_data$precip_sum_lag3 <- dplyr::lag(analysis_data$precip_sum, 3)

model_linear_lag1 <- lm(
  GI_daily_inc_mean ~ temp_max_lag1 + vpd_max_lag1 + precip_sum_lag1,
  data = analysis_data
)

model_linear_lag2 <- lm(
  GI_daily_inc_mean ~ temp_max_lag2 + vpd_max_lag2 + precip_sum_lag2,
  data = analysis_data
)

model_linear_lag3 <- lm(
  GI_daily_inc_mean ~ temp_max_lag3 + vpd_max_lag3 + precip_sum_lag3,
  data = analysis_data
)

summary(model_linear_lag1)
summary(model_linear_lag2)
summary(model_linear_lag3)

# Nur positive Wachstumstage
model_linear_positive <- lm(
  GI_daily_inc_mean ~ temp_max + vpd_max + precip_sum,
  data = analysis_positive
)

summary(model_linear_positive)

# Zusätzliche lineare Modelle mit Trockenperioden

# Kontinuierliche Länge der vorangehenden Trockenperiode
model_linear_dry_streak <- lm(
  GI_daily_inc_mean ~ dry_streak_before + temp_max + vpd_max + precip_sum,
  data = analysis_data
)

summary(model_linear_dry_streak)

# Binäre 5-Tage-Dürre
model_linear_drought5 <- lm(
  GI_daily_inc_mean ~ drought_5d_before + temp_max + vpd_max + precip_sum,
  data = analysis_data
)

summary(model_linear_drought5)

# Optional: nur positive Wachstumstage
model_linear_positive_dry_streak <- lm(
  GI_daily_inc_mean ~ dry_streak_before + temp_max + vpd_max + precip_sum,
  data = analysis_positive
)

summary(model_linear_positive_dry_streak)

model_linear_positive_drought5 <- lm(
  GI_daily_inc_mean ~ drought_5d_before + temp_max + vpd_max + precip_sum,
  data = analysis_positive
)

summary(model_linear_positive_drought5)

