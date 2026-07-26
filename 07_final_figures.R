# =========================================================
# 07_final_figures.R
# Endgrafiken für das Poster
# =========================================================

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# ---------------------------------------------------------
# 1) Basisordner setzen
#    funktioniert sowohl aus "Datensatz" als auch aus "Skripte"
# ---------------------------------------------------------
base_dir <- if (basename(getwd()) == "Skripte") ".." else "."

data_out_dir <- file.path(base_dir, "data_out")
output_dir   <- file.path(base_dir, "Ausgabe")

dir.create(output_dir, showWarnings = FALSE)

# ---------------------------------------------------------
# 2) Daten einlesen
# ---------------------------------------------------------
analysis_growing   <- readRDS(file.path(data_out_dir, "analysis_growing.rds"))
best_logit         <- readRDS(file.path(data_out_dir, "best_logit.rds"))
logit_lag_results  <- readRDS(file.path(data_out_dir, "logit_lag_results.rds"))
model_logit_best   <- readRDS(file.path(data_out_dir, "model_logit_best.rds"))
# ---------------------------------------------------------
# 3) Daten für Plots bereinigen
# ---------------------------------------------------------
best_logit_plot <- best_logit %>%
  filter(!is.na(growth_day), !is.na(precip_sum_best))

analysis_growing_plot <- analysis_growing %>%
  filter(!is.na(growth_day), !is.na(vpd_max))

# ---------------------------------------------------------
# 4) Einheitliches Poster-Theme
# ---------------------------------------------------------
poster_theme <- theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 20, face = "bold", lineheight = 1.1),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 15),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(12, 12, 12, 12)
  )

# ---------------------------------------------------------
# 5) Grafik zu Frage 1
#    Einfluss von Hitze-/Trockenereignissen
#    -> VPD an Tagen mit und ohne Wachstum
# ---------------------------------------------------------
p_frage1 <- ggplot(analysis_growing_plot, aes(x = factor(growth_day), y = vpd_max)) +
  geom_boxplot() +
  labs(
    title = "Frage 1: Trockenstress (VPD)\nan Tagen mit und ohne Wachstum",
    x = "Wachstumstag (0 = nein, 1 = ja)",
    y = "VPD max (hPa)"
  ) +
  poster_theme

# ---------------------------------------------------------
# 6) Grafik zu Frage 2
#    Wichtigste meteorologische Variable
#    -> Niederschlag beim besten Lag
# ---------------------------------------------------------
# -----------------------------------------
# Abb. 3: Modellbasierte Vorhersage aus dem vollständigen logistischen Modell
# -----------------------------------------

newdata_precip <- data.frame(
  precip_sum_best = seq(
    min(best_logit_plot$precip_sum_best, na.rm = TRUE),
    max(best_logit_plot$precip_sum_best, na.rm = TRUE),
    length.out = 200
  ),
  temp_max_best = mean(best_logit_plot$temp_max_best, na.rm = TRUE),
  vpd_max_best  = mean(best_logit_plot$vpd_max_best, na.rm = TRUE)
)

pred <- predict(
  model_logit_best,
  newdata = newdata_precip,
  type = "link",
  se.fit = TRUE
)

newdata_precip$fit <- plogis(pred$fit)
newdata_precip$lwr <- plogis(pred$fit - 1.96 * pred$se.fit)
newdata_precip$upr <- plogis(pred$fit + 1.96 * pred$se.fit)

p_frage2 <- ggplot(newdata_precip, aes(x = precip_sum_best, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line(linewidth = 1) +
  labs(
    title = "Frage 2: Vorhergesagte Wahrscheinlichkeit eines Wachstumstages\nin Abhängigkeit vom Niederschlag (Lag = 2 Tage)",
    x = "Niederschlag zwei Tage zuvor (mm)",
    y = "Vorhergesagte Wahrscheinlichkeit"
  ) +
  poster_theme

p_frage2

# ---------------------------------------------------------
# 7) Grafik zu Frage 3
#    Verzögerte Reaktion / Lag
# ---------------------------------------------------------
p_frage3 <- ggplot(logit_lag_results, aes(x = lag, y = aic)) +
  geom_line() +
  geom_point() +
  geom_point(
    data = subset(logit_lag_results, aic == min(aic)),
    size = 3
  ) +
  labs(
    title = "Frage 3: Modellgüte der\nlogistischen Modelle in Abhängigkeit vom Lag",
    x = "Lag (Tage)",
    y = "AIC"
  ) +
  poster_theme

# Grafiken ohne Titel für Projektbericht

p_frage12 <- ggplot(analysis_growing_plot, aes(x = factor(growth_day), y = vpd_max)) +
  geom_boxplot() +
  labs(
    x = "Wachstumstag (0 = nein, 1 = ja)",
    y = "VPD max (hPa)"
  ) +
  poster_theme

# ---------------------------------------------------------
# Grafik zu Frage 2
# Modellbasierte Vorhersage aus dem vollständigen logistischen Modell
# ---------------------------------------------------------

newdata_precip <- data.frame(
  precip_sum_best = seq(
    min(best_logit_plot$precip_sum_best, na.rm = TRUE),
    max(best_logit_plot$precip_sum_best, na.rm = TRUE),
    length.out = 200
  ),
  temp_max_best = mean(best_logit_plot$temp_max_best, na.rm = TRUE),
  vpd_max_best  = mean(best_logit_plot$vpd_max_best, na.rm = TRUE)
)

pred <- predict(
  model_logit_best,
  newdata = newdata_precip,
  type = "link",
  se.fit = TRUE
)

newdata_precip$fit <- plogis(pred$fit)
newdata_precip$lwr <- plogis(pred$fit - 1.96 * pred$se.fit)
newdata_precip$upr <- plogis(pred$fit + 1.96 * pred$se.fit)

p_frage22 <- ggplot(newdata_precip, aes(x = precip_sum_best, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line(linewidth = 1) +
  labs(
    x = "Niederschlag zwei Tage zuvor (mm)",
    y = "Vorhergesagte Wahrscheinlichkeit"
  ) +
  poster_theme

p_frage2

# ---------------------------------------------------------
# Grafik zu Frage 3
# Verzögerte Reaktion / Lag
# ---------------------------------------------------------

p_frage32 <- ggplot(logit_lag_results, aes(x = lag, y = aic)) +
  geom_line() +
  geom_point() +
  geom_point(
    data = subset(logit_lag_results, aic == min(aic)),
    size = 3
  ) +
  labs(
    x = "Lag (Tage)",
    y = "AIC"
  ) +
  poster_theme
# ---------------------------------------------------------
# 8) Einheitlich speichern
# ---------------------------------------------------------
plot_width  <- 8.5
plot_height <- 5.5
plot_dpi    <- 300

ggsave(
  filename = file.path(output_dir, "Frage1_VPD_Wachstum.png"),
  plot = p_frage1,
  width = plot_width, height = plot_height, units = "in", dpi = plot_dpi, bg = "white"
)

ggsave(
  filename = file.path(output_dir, "Frage2_Niederschlag_besterLag.png"),
  plot = p_frage2,
  width = plot_width, height = plot_height, units = "in", dpi = plot_dpi, bg = "white"
)

ggsave(
  filename = file.path(output_dir, "Frage3_Modellguete_Lag.png"),
  plot = p_frage3,
  width = plot_width, height = plot_height, units = "in", dpi = plot_dpi, bg = "white"
)

ggsave(
  filename = file.path(output_dir, "Frage12_VPD_Wachstum.png"),
  plot = p_frage12,
  width = plot_width, height = plot_height, units = "in", dpi = plot_dpi, bg = "white"
)

ggsave(
  filename = file.path(output_dir, "Frage22_Niederschlag_besterLag.png"),
  plot = p_frage22,
  width = plot_width, height = plot_height, units = "in", dpi = plot_dpi, bg = "white"
)

ggsave(
  filename = file.path(output_dir, "Frage32_Modellguete_Lag.png"),
  plot = p_frage32,
  width = plot_width, height = plot_height, units = "in", dpi = plot_dpi, bg = "white"
)

