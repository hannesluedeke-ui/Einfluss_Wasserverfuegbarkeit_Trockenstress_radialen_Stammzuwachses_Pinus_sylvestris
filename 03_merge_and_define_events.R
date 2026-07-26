# =========================================================
# 03_merge_and_define_events.R
# Dendro- und Meteodaten zusammenführen, Ereignisse definieren
# =========================================================

source("./Skripte/00_setup.R")

dendro_daily_trees <- readRDS("./data_out/dendro_daily_trees.rds")
dendro_src_daily   <- readRDS("./data_out/dendro_src_daily.rds")
meteo_daily        <- readRDS("./data_out/meteo_daily.rds")

analysis_data <- merge(
  dendro_daily_trees[, c("date", "GI_mean", "GI_daily_inc_mean")],
  meteo_daily,
  by = "date"
)

analysis_data <- merge(
  analysis_data,
  dendro_src_daily,
  by = "date"
)

# ---------------------------------------------------------
# Ereignisdefinitionen
# ---------------------------------------------------------
analysis_data <- analysis_data %>%
  arrange(date)

# Hitzetag
analysis_data$heat_day <- ifelse(analysis_data$temp_max >= 30, 1, 0)

# Einzelner trockener Tag
analysis_data$dry_day <- ifelse(analysis_data$precip_sum == 0, 1, 0)

# Hohes VPD
analysis_data$high_vpd <- ifelse(
  analysis_data$vpd_max >= quantile(analysis_data$vpd_max, 0.9, na.rm = TRUE),
  1, 0
)

# ---------------------------------------------------------
# Funktion: zählt aufeinanderfolgende trockene Tage
# ---------------------------------------------------------
count_dry_streak <- function(x) {
  out <- integer(length(x))
  run <- 0L
  
  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      out[i] <- NA_integer_
    } else if (x[i] == 1) {
      run <- run + 1L
      out[i] <- run
    } else {
      run <- 0L
      out[i] <- 0L
    }
  }
  
  return(out)
}

# Länge der aktuellen Trockenperiode bis einschließlich heute
analysis_data$dry_streak_current <- count_dry_streak(analysis_data$dry_day)

# Länge der Trockenperiode VOR dem aktuellen Tag
analysis_data$dry_streak_before <- dplyr::lag(
  analysis_data$dry_streak_current,
  1,
  default = 0
)

# Dürredefinition:
# 1 = mindestens 5 trockene Tage in Folge vor dem Beobachtungstag
analysis_data$drought_5d_before <- ifelse(
  analysis_data$dry_streak_before >= 5,
  1,
  0
)

# ---------------------------------------------------------
# Vegetationsperiode
# ---------------------------------------------------------
analysis_data$month <- lubridate::month(analysis_data$date)

analysis_growing <- analysis_data %>%
  filter(month >= 4 & month <= 9)

# Wachstumstag definieren
analysis_growing$growth_day <- ifelse(analysis_growing$GI_daily_inc_mean > 0, 1, 0)

# Positive Wachstumstage
analysis_positive <- analysis_growing %>%
  filter(GI_daily_inc_mean > 0)

# ---------------------------------------------------------
# Speichern
# ---------------------------------------------------------
saveRDS(analysis_data, "./data_out/analysis_data.rds")
saveRDS(analysis_growing, "./data_out/analysis_growing.rds")
saveRDS(analysis_positive, "./data_out/analysis_positive.rds")



