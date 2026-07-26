# =========================================================
# 04_explorative_plots.R
# Explorative Grafiken/ für Projektbericht
# =========================================================

source("./Skripte/00_setup.R")

dendro_daily_trees <- readRDS("./data_out/dendro_daily_trees.rds")
dendro_src_daily   <- readRDS("./data_out/dendro_src_daily.rds")
analysis_data      <- readRDS("./data_out/analysis_data.rds")
analysis_growing   <- readRDS("./data_out/analysis_growing.rds")

# 1) Täglicher GI-Zuwachs
ggplot(dendro_daily_trees, aes(x = date, y = GI_daily_inc_mean)) +
  geom_line(na.rm = TRUE) +
  coord_cartesian(ylim = c(0, 70)) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(
    x = "Jahr",
    y = "Täglicher GI (µm)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(face = "bold")
  )

# 2) SRC
ggplot(dendro_src_daily, aes(x = date, y = SRC_mean)) +
  geom_line() +
  labs(
    title = "Mittlere tägliche Stammradiusänderung (SRC)",
    x = "Datum",
    y = "SRC (µm)"
  ) +
  theme_minimal()

# 3) Scatter: GI-Zuwachs und Niederschlag
ggplot(analysis_data, aes(x = precip_sum, y = GI_daily_inc_mean)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Täglicher GI-Zuwachs und Niederschlag",
    x = "Täglicher Niederschlag (mm)",
    y = "Täglicher GI-Zuwachs (µm)"
  ) +
  theme_minimal()

# 4) Boxplot: Hitzetag / kein Hitzetag
ggplot(analysis_data, aes(x = factor(heat_day), y = GI_daily_inc_mean)) +
  geom_boxplot() +
  labs(
    title = "Täglicher GI-Zuwachs an Hitzetagen vs. Nicht-Hitzetagen",
    x = "Hitzetag (0 = nein, 1 = ja)",
    y = "Täglicher GI-Zuwachs (µm)"
  ) +
  theme_minimal()

# 5) Boxplot: VPD an Tagen mit und ohne Wachstum
ggplot(analysis_growing, aes(x = factor(growth_day), y = vpd_max)) +
  geom_boxplot() +
  labs(
    title = "Maximales VPD an Tagen mit und ohne Wachstum",
    x = "Wachstumstag (0 = nein, 1 = ja)",
    y = "VPD max (hPa)"
  ) +
  theme_minimal()

# 6) Boxplot: Niederschlag an Wachstums- und Nichtwachstumstagen
ggplot(
  analysis_growing_plot,
  aes(
    x = factor(
      growth_day,
      levels = c(0, 1),
      labels = c("Kein Wachstum", "Wachstum")
    ),
    y = precip_sum
  )
) +
  geom_boxplot() +
  labs(
    x = NULL,
    y = "Tägliche Niederschlagssumme (mm)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(face = "bold"),
    axis.title = element_text(
      face = "bold",
      size = 14
    ),
    axis.text = element_text(
      face = "bold",
      size = 12
    )
  )

# 7) Boxplot: Niederschlag vor 2 Tagen an Wachstums- und Nichtwachstumstagen
ggplot(
  best_logit_plot,
  aes(
    x = factor(
      growth_day,
      levels = c(0, 1),
      labels = c("Kein Wachstum", "Wachstum")
    ),
    y = precip_sum_best
  )
) +
  geom_boxplot() +
  labs(
    x = NULL,
    y = "Niederschlag zwei Tage zuvor (mm)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(face = "bold"),
    axis.title = element_text(
      face = "bold",
      size = 14
    ),
    axis.text = element_text(
      face = "bold",
      size = 12
    )
  )

# 8) Scatterplot: Niederschlag am Vortag und Zuwachsstärke
ggplot(
  analysis_data,
  aes(
    x = precip_sum_lag1,
    y = GI_daily_inc_mean
  )
) +
  geom_point(
    alpha = 0.4,
    na.rm = TRUE
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    na.rm = TRUE
  ) +
  labs(
    x = "Niederschlag des Vortages (mm)",
    y = "Täglicher GI-Zuwachs (µm)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(face = "bold"),
    axis.title = element_text(
      face = "bold",
      size = 14
    ),
    axis.text = element_text(
      face = "bold",
      size = 12
    )
  )
