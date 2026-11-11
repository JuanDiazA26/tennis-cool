# =============================================================
# 02_elo.R
# Proyecto: Predictor de partidos ATP + perfiles de juego
# Entrada : data/processed/partidos.rds   (sale de 01_carga_limpieza.R)
# Salida  : data/processed/partidos_elo.rds  (partidos + Elo PREVIO)
#           data/processed/elo_historial.rds (formato largo, para Tableau)
# =============================================================

library(tidyverse)

partidos <- read_rds("data/processed/partidos.rds")

# -------------------------------------------------------------
# 1. PARÁMETROS DEL ELO
# -------------------------------------------------------------
ELO_INICIAL <- 1500

# K dinámico (versión usada por FiveThirtyEight para tenis):
# los jugadores con pocos partidos cambian mucho de Elo,
# los veteranos cambian poco.
k_factor <- function(n_partidos) {
  250 / (n_partidos + 5)^0.4
}

# Probabilidad de que A gane a B según sus Elo
prob_esperada <- function(elo_a, elo_b) {
  1 / (1 + 10^((elo_b - elo_a) / 400))
}

# -------------------------------------------------------------
# 2. "MEMORIA" DE LOS JUGADORES
# Usamos entornos (environments) como diccionarios:
# clave = id del jugador (o id + superficie), valor = Elo actual.
# Son mucho más rápidos que buscar en un data frame en cada vuelta.
# -------------------------------------------------------------
elo_gen   <- new.env()   # Elo general
elo_sup   <- new.env()   # Elo por superficie  (clave "id_Clay", ...)
n_gen     <- new.env()   # nº de partidos jugados (general)
n_sup     <- new.env()   # nº de partidos jugados (por superficie)

leer <- function(env, clave, defecto) get0(clave, envir = env, ifnotfound = defecto)

# -------------------------------------------------------------
# 3. RECORRIDO CRONOLÓGICO
# Los partidos ya vienen ordenados desde 01_carga_limpieza.R
# (fecha -> torneo -> ronda). ¡No reordenar aquí!
# -------------------------------------------------------------
n <- nrow(partidos)

# Vectores vacíos donde guardaremos el Elo ANTES de cada partido
w_elo <- l_elo <- w_elo_sup <- l_elo_sup <- numeric(n)
w_n   <- l_n   <- w_n_sup   <- l_n_sup   <- integer(n)
# Y el Elo DESPUÉS (para el historial de Tableau)
w_elo_post <- l_elo_post <- w_elo_sup_post <- l_elo_sup_post <- numeric(n)

for (i in seq_len(n)) {
  
  w  <- as.character(partidos$winner_id[i])
  l  <- as.character(partidos$loser_id[i])
  ws <- paste(w, partidos$surface[i], sep = "_")
  ls <- paste(l, partidos$surface[i], sep = "_")
  
  # --- 3.1 Leer el estado actual (antes del partido) ---
  ew  <- leer(elo_gen, w,  ELO_INICIAL); el  <- leer(elo_gen, l,  ELO_INICIAL)
  ews <- leer(elo_sup, ws, ELO_INICIAL); els <- leer(elo_sup, ls, ELO_INICIAL)
  nw  <- leer(n_gen, w,  0L);            nl  <- leer(n_gen, l,  0L)
  nws <- leer(n_sup, ws, 0L);            nls <- leer(n_sup, ls, 0L)
  
  # Guardamos el Elo PREVIO: es lo único que el modelo puede "saber"
  w_elo[i] <- ew;      l_elo[i] <- el
  w_elo_sup[i] <- ews; l_elo_sup[i] <- els
  w_n[i] <- nw;        l_n[i] <- nl
  w_n_sup[i] <- nws;   l_n_sup[i] <- nls
  
  # --- 3.2 Actualizar Elo general ---
  p_w <- prob_esperada(ew, el)                 # prob. de que ganara el ganador
  ew_new <- ew + k_factor(nw) * (1 - p_w)      # el ganador sube
  el_new <- el - k_factor(nl) * (1 - p_w)      # el perdedor baja
  
  # --- 3.3 Actualizar Elo por superficie ---
  p_ws <- prob_esperada(ews, els)
  ews_new <- ews + k_factor(nws) * (1 - p_ws)
  els_new <- els - k_factor(nls) * (1 - p_ws)
  
  # --- 3.4 Guardar el nuevo estado ---
  assign(w,  ew_new,  envir = elo_gen); assign(l,  el_new,  envir = elo_gen)
  assign(ws, ews_new, envir = elo_sup); assign(ls, els_new, envir = elo_sup)
  assign(w,  nw + 1L,  envir = n_gen);  assign(l,  nl + 1L,  envir = n_gen)
  assign(ws, nws + 1L, envir = n_sup);  assign(ls, nls + 1L, envir = n_sup)
  
  w_elo_post[i] <- ew_new;      l_elo_post[i] <- el_new
  w_elo_sup_post[i] <- ews_new; l_elo_sup_post[i] <- els_new
}

# -------------------------------------------------------------
# 4. TABLA DE PARTIDOS CON ELO PREVIO
# -------------------------------------------------------------
partidos_elo <- partidos |>
  mutate(
    winner_elo = w_elo,          loser_elo = l_elo,
    winner_elo_sup = w_elo_sup,  loser_elo_sup = l_elo_sup,
    winner_n = w_n,              loser_n = l_n,
    winner_n_sup = w_n_sup,      loser_n_sup = l_n_sup,
    # Elo combinado: mezcla del general y el de la superficie
    winner_elo_mix = (winner_elo + winner_elo_sup) / 2,
    loser_elo_mix  = (loser_elo  + loser_elo_sup)  / 2,
    # Probabilidad que el Elo daba al ganador real
    prob_elo     = prob_esperada(winner_elo, loser_elo),
    prob_elo_sup = prob_esperada(winner_elo_sup, loser_elo_sup),
    prob_elo_mix = prob_esperada(winner_elo_mix, loser_elo_mix)
  )

# -------------------------------------------------------------
# 5. EVALUACIÓN RÁPIDA (sin el año de calentamiento 2017)
# Acierto = el Elo daba más del 50 % al que finalmente ganó
# -------------------------------------------------------------
evaluacion <- partidos_elo |>
  filter(year >= 2018) |>
  summarise(
    partidos           = n(),
    acierto_ranking    = mean(winner_rank < loser_rank, na.rm = TRUE),
    acierto_elo        = mean(prob_elo > 0.5),
    acierto_elo_sup    = mean(prob_elo_sup > 0.5),
    acierto_elo_mix    = mean(prob_elo_mix > 0.5),
    brier_elo_mix      = mean((1 - prob_elo_mix)^2)  # cuanto más bajo, mejor
  )
print(evaluacion)

# Acierto por superficie (útil para la memoria)
partidos_elo |>
  filter(year >= 2018) |>
  group_by(surface_es) |>
  summarise(partidos = n(), acierto_elo_mix = mean(prob_elo_mix > 0.5)) |>
  print()

# -------------------------------------------------------------
# 6. HISTORIAL EN FORMATO LARGO (una fila por jugador y partido)
# Es el formato ideal para el gráfico de evolución en Tableau.
# -------------------------------------------------------------
historial_w <- partidos_elo |>
  transmute(match_id, tourney_date, year, tourney_name, surface_es, nivel, round,
            player_id = winner_id, player_name = winner_name,
            rival_name = loser_name, resultado = "Victoria",
            elo = w_elo_post, elo_sup = w_elo_sup_post)

historial_l <- partidos_elo |>
  transmute(match_id, tourney_date, year, tourney_name, surface_es, nivel, round,
            player_id = loser_id, player_name = loser_name,
            rival_name = winner_name, resultado = "Derrota",
            elo = l_elo_post, elo_sup = l_elo_sup_post)

elo_historial <- bind_rows(historial_w, historial_l) |>
  arrange(player_id, tourney_date, round)

# -------------------------------------------------------------
# 7. COMPROBACIÓN: TOP 10 POR ELO ACTUAL
# (último Elo de cada jugador con al menos 30 partidos)
# -------------------------------------------------------------
elo_historial |>
  group_by(player_id, player_name) |>
  filter(n() >= 30) |>
  slice_tail(n = 1) |>
  ungroup() |>
  arrange(desc(elo)) |>
  select(player_name, ultima_fecha = tourney_date, elo) |>
  head(10) |>
  print()

# -------------------------------------------------------------
# 8. GUARDADO
# -------------------------------------------------------------
write_rds(partidos_elo,  "data/processed/partidos_elo.rds")
write_rds(elo_historial, "data/processed/elo_historial.rds")

partidos <- readr::read_rds("data/processed/elo_historial.rds")
View(partidos)