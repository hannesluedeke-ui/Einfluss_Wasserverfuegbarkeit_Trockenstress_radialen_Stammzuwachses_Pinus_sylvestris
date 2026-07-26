# =========================================================
# 02_meteo_prep.R
# Meteodaten einlesen und Tageswerte bilden
# =========================================================

source("./Skripte/00_setup.R")

meteo <- read_tsv(
  "./Daten/Pfynwald_meteo.tab",
  skip = 19,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

meteo_use <- meteo[, c(
  "Date/Time",
  "TTT [°C]",
  "RH [%]",
  "VPD [hPa]",
  "Precip [mm/h]",
  "NET [W/m**2]"
)]

colnames(meteo_use) <- c("datetime", "temp", "rh", "vpd", "precip", "net")

meteo_use$datetime <- as.POSIXct(meteo_use$datetime, tz = "UTC")
meteo_use$date <- as.Date(meteo_use$datetime)

temp_mean   <- aggregate(temp ~ date, data = meteo_use, FUN = mean, na.rm = TRUE)
temp_max    <- aggregate(temp ~ date, data = meteo_use, FUN = max,  na.rm = TRUE)
rh_mean     <- aggregate(rh ~ date, data = meteo_use, FUN = mean, na.rm = TRUE)
vpd_mean    <- aggregate(vpd ~ date, data = meteo_use, FUN = mean, na.rm = TRUE)
vpd_max     <- aggregate(vpd ~ date, data = meteo_use, FUN = max,  na.rm = TRUE)
precip_sum  <- aggregate(precip ~ date, data = meteo_use, FUN = sum, na.rm = TRUE)
net_mean    <- aggregate(net ~ date, data = meteo_use, FUN = mean, na.rm = TRUE)

colnames(temp_mean)[2]  <- "temp_mean"
colnames(temp_max)[2]   <- "temp_max"
colnames(rh_mean)[2]    <- "rh_mean"
colnames(vpd_mean)[2]   <- "vpd_mean"
colnames(vpd_max)[2]    <- "vpd_max"
colnames(precip_sum)[2] <- "precip_sum"
colnames(net_mean)[2]   <- "net_mean"

meteo_daily <- merge(temp_mean, temp_max, by = "date")
meteo_daily <- merge(meteo_daily, rh_mean, by = "date")
meteo_daily <- merge(meteo_daily, vpd_mean, by = "date")
meteo_daily <- merge(meteo_daily, vpd_max, by = "date")
meteo_daily <- merge(meteo_daily, precip_sum, by = "date")
meteo_daily <- merge(meteo_daily, net_mean, by = "date")

saveRDS(meteo_use, "./data_out/meteo_use.rds")
saveRDS(meteo_daily, "./data_out/meteo_daily.rds")



