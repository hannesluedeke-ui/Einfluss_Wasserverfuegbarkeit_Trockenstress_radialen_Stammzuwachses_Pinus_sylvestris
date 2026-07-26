Dieses Repository enthält alle R-Skripte zur Analyse des radialen Stammzuwachses von Pinus sylvestris
im Pfynwald unter variierenden meteorologischen Bedingungen ab. Die Skripte bilden den vollständigen
Workflow von Aufbereitung bis Visualisierung und Analyse ab. 

Wichtig: Die Daten müssen über Link im Ordner Daten heruntergeladen werden.

Skripte
00_setup.R – Paketverwaltung, Pfadprüfung, grundlegende Projektinitialisierung

01_dendro_prep.R – Aufbereitung der Dendrometerdaten

02_meteo_prep.R – Aufbereitung der meteorologischen Daten

03_merge_and_define_events.R – Zusammenführung der Datensätze, Definition von Wachstumsereignissen

04_explorative_plots.R – Explorative Visualisierungen

05_linear_models.R – Lineare Modelle zur Erklärung der täglichen Zuwachsstärke

06_logistic_models_and_lags.R – Logistische Modelle und Lag‑Analyse zum Wachstumseintritt

07_final_figures.R – Erstellung der finalen Abbildungen

99_run_all.R – Pipeline‑Skript zum Ausführen aller Schritte in korrekter Reihenfolge
