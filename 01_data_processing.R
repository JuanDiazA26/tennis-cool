# =============================================================
# 01_carga_limpieza.R
# Proyecto: Predictor de partidos ATP + perfiles de juego
# Entrada : data/raw/atp_matches_2017.csv ... atp_matches_2026.csv
# Salida  : data/processed/partidos.rds
# =============================================================

library(tidyverse)
library(lubridate)

# -------------------------------------------------------------
# 1. CARGA
# Leemos todo como texto para evitar conflictos de tipos entre
# años (p. ej. winner_seed numérico un año y texto otro) y
# después dejamos que R detecte el tipo correcto de cada columna.
# -------------------------------------------------------------
archivos <- list.files("data/raw",
                       pattern = "^atp_matches_20[0-9]{2}\\.csv$",
                       full.names = TRUE)

print(archivos)   # comprueba que salen los 10 ficheros

partidos_raw <- archivos |>
  map_dfr(read_csv,
          col_types = cols(.default = col_character()),
          show_col_types = FALSE) |>
  type_convert(guess_integer = TRUE)

cat("Partidos cargados:", nrow(partidos_raw), "\n")

# -------------------------------------------------------------
# 2. EXPLORACIÓN INICIAL (para entender los datos)
# -------------------------------------------------------------
glimpse(partidos_raw)

partidos_raw |> count(tourney_level)   # G, M, A, F, D, O...
partidos_raw |> count(surface)         # Hard, Clay, Grass, Carpet
partidos_raw |> count(round)

# % de valores nulos en columnas clave
partidos_raw |>
  summarise(across(c(winner_rank, loser_rank, winner_age, winner_ht,
                     w_ace, w_svpt, w_1stIn, minutes),
                   ~ round(mean(is.na(.)) * 100, 1)))

# -------------------------------------------------------------
# 3. LIMPIEZA
# -------------------------------------------------------------
orden_rondas <- c("R128", "R64", "R32", "R16", "RR",
                  "QF", "SF", "BR", "F")

partidos <- partidos_raw |>
  # 3.1 Fechas: vienen como número 20240115 -> fecha real
  mutate(tourney_date = ymd(tourney_date),
         year = year(tourney_date)) |>
  
  # 3.2 Quitamos Copa Davis (D) y superficies poco frecuentes
  filter(tourney_level != "D",
         surface %in% c("Hard", "Clay", "Grass")) |>
  
  # 3.3 Quitamos retiradas, walkovers y partidos sin resultado
  filter(!is.na(score),
         !str_detect(score, regex("RET|W/O|DEF|ABD|unfinished",
                                  ignore_case = TRUE))) |>
  
  # 3.4 Identificador único de partido y eliminación de duplicados
  mutate(match_id = paste(tourney_id, match_num, sep = "_")) |>
  distinct(match_id, .keep_all = TRUE) |>
  
  # 3.5 Orden cronológico (CLAVE para el Elo):
  # todos los partidos de un torneo tienen la misma fecha,
  # así que desempatamos por ronda
  mutate(round = factor(round, levels = orden_rondas)) |>
  arrange(tourney_date, tourney_id, round, match_num) |>
  
  # 3.6 Etiquetas legibles para Tableau y la memoria
  mutate(
    surface_es = recode(surface, Hard = "Dura",
                        Clay = "Tierra", Grass = "Hierba"),
    nivel = recode(tourney_level,
                   G = "Grand Slam", M = "Masters 1000",
                   A = "ATP 250/500", F = "ATP Finals",
                   O = "Juegos Olímpicos", .default = "Otros")
  ) |>
  
  # 3.7 Nos quedamos solo con las columnas que vamos a usar
  select(match_id, year, tourney_date, tourney_id, tourney_name,
         surface, surface_es, tourney_level, nivel, round, best_of,
         winner_id, winner_name, winner_hand, winner_ht, winner_age,
         winner_rank, winner_rank_points,
         loser_id, loser_name, loser_hand, loser_ht, loser_age,
         loser_rank, loser_rank_points,
         score, minutes,
         starts_with("w_"), starts_with("l_"))

cat("Partidos tras limpieza:", nrow(partidos), "\n")

# -------------------------------------------------------------
# 4. COMPROBACIONES FINALES
# -------------------------------------------------------------
partidos |> count(year)           # ¿están todos los años?
partidos |> count(surface_es)
partidos |> count(is.na(round))   # rondas no reconocidas (debería ser 0)

# Baseline: ¿con qué frecuencia gana el mejor clasificado?
partidos |>
  filter(!is.na(winner_rank), !is.na(loser_rank)) |>
  summarise(pct_gana_favorito = mean(winner_rank < loser_rank) * 100)

# -------------------------------------------------------------
# 5. GUARDADO
# -------------------------------------------------------------
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write_rds(partidos, "data/processed/partidos.rds")

partidos_ori <- readr::read_rds("data/processed/partidos.rds")
View(partidos_ori)