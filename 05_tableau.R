# =============================================================
# 05_export_tableau.R
# Proyecto: Predictor de partidos ATP + perfiles de juego
# Entrada : los .rds de data/processed/ (scripts 02, 03 y 04)
# Salida  : data/tableau/*.csv  -> lo que se conecta en Tableau
#
# Ficheros que genera (y para qué dashboard sirven):
#   1. elo_historial.csv          -> Evolución del Elo
#   2. jugadores.csv              -> Tabla maestra de jugadores (filtros, fichas)
#   3. predicciones.csv           -> Rendimiento del modelo / sorpresas
#   4. metricas_modelos.csv       -> Rendimiento del modelo
#   5. calibracion.csv            -> Rendimiento del modelo
#   6. importancia_variables.csv  -> Rendimiento del modelo
#   7. cara_a_cara.csv            -> Simulador cara a cara
#   8. perfiles_jugadores.csv     -> Perfiles de juego (scatter)
#   9. perfiles_stats_largo.csv   -> Perfiles de juego (barras / radar)
#  10. perfiles_resumen.csv       -> Perfiles de juego (fichas de perfil)
# =============================================================

library(tidyverse)

dir_in  <- "data/processed"
dir_out <- "data/tableau"
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

leer <- function(nombre) read_rds(file.path(dir_in, paste0(nombre, ".rds")))
guardar <- function(df, nombre) {
  write_csv(df, file.path(dir_out, paste0(nombre, ".csv")), na = "")
  cat(sprintf("%-28s %7d filas\n", paste0(nombre, ".csv"), nrow(df)))
}

partidos_elo  <- leer("partidos_elo")
elo_historial <- leer("elo_historial")
predicciones  <- leer("predicciones")
metricas      <- leer("metricas")
calibracion   <- leer("calibracion")
importancia   <- leer("importancia")
modelos       <- leer("modelos")
perfiles      <- leer("perfiles")
centros       <- leer("centros")

# Debe coincidir con el reparto usado en 02_elo.R para el elo_mix
PESO_GENERAL <- 0.5

# Jugadores que aparecerán en el simulador cara a cara
TOP_CARA_A_CARA <- 50

# -------------------------------------------------------------
# 1. HISTORIAL DE ELO (formato largo: una fila por jugador y partido)
# -------------------------------------------------------------
partidos_por_jugador <- elo_historial |> count(player_id, name = "partidos_totales")

elo_hist_out <- elo_historial |>
  left_join(partidos_por_jugador, by = "player_id") |>
  mutate(es_top30 = player_id %in% perfiles$player_id,
         elo = round(elo, 1),
         elo_sup = round(elo_sup, 1)) |>
  rename(elo_superficie = elo_sup, superficie = surface_es)

guardar(elo_hist_out, "elo_historial")

# -------------------------------------------------------------
# 2. TABLA MAESTRA DE JUGADORES (estado ACTUAL de cada uno)
# -------------------------------------------------------------
# Datos personales y ranking del último partido de cada jugador
ultimo_partido <- bind_rows(
  partidos_elo |> transmute(tourney_date, round, player_id = winner_id,
                            player_name = winner_name, mano = winner_hand,
                            altura = winner_ht, edad = winner_age,
                            ranking = winner_rank),
  partidos_elo |> transmute(tourney_date, round, player_id = loser_id,
                            player_name = loser_name, mano = loser_hand,
                            altura = loser_ht, edad = loser_age,
                            ranking = loser_rank)
) |>
  arrange(player_id, tourney_date, round) |>
  group_by(player_id) |>
  slice_tail(n = 1) |>
  ungroup() |>
  select(player_id, player_name, mano, altura, edad, ranking,
         ultima_fecha = tourney_date)

# Elo general actual
elo_actual <- elo_historial |>
  group_by(player_id) |>
  slice_tail(n = 1) |>
  ungroup() |>
  select(player_id, elo_actual = elo)

# Elo actual en cada superficie (último partido jugado en esa superficie)
elo_sup_actual <- elo_historial |>
  group_by(player_id, surface_es) |>
  slice_tail(n = 1) |>
  ungroup() |>
  select(player_id, surface_es, elo_sup) |>
  pivot_wider(names_from = surface_es, values_from = elo_sup,
              names_prefix = "elo_")

# Balance de victorias
balance <- elo_historial |>
  group_by(player_id) |>
  summarise(partidos = n(),
            victorias = sum(resultado == "Victoria"),
            pct_victorias = victorias / partidos,
            .groups = "drop")

jugadores <- ultimo_partido |>
  left_join(elo_actual, by = "player_id") |>
  left_join(elo_sup_actual, by = "player_id") |>
  left_join(balance, by = "player_id") |>
  left_join(perfiles |> select(player_id, perfil), by = "player_id") |>
  mutate(mano = recode(mano, R = "Diestro", L = "Zurdo", .default = "Desconocida"),
         perfil = replace_na(perfil, "Fuera del top 30"),
         rank_elo = min_rank(desc(elo_actual)),
         edad = round(edad, 1),
         pct_victorias = round(pct_victorias, 4),
         across(starts_with("elo_"), ~ round(., 1))) |>
  arrange(rank_elo)

guardar(jugadores, "jugadores")

# -------------------------------------------------------------
# 3. PREDICCIONES (una fila por partido de test / 2026)
# -------------------------------------------------------------
pred_out <- predicciones |>
  mutate(
    tramo_confianza = cut(prob_favorito,
                          breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1),
                          labels = c("50-60 %", "60-70 %", "70-80 %",
                                     "80-90 %", "90-100 %"),
                          include.lowest = TRUE),
    acierto_num = as.integer(acierto),          # para promediar en Tableau
    across(starts_with("prob"), ~ round(., 4))
  )

guardar(pred_out, "predicciones")

# -------------------------------------------------------------
# 4. MÉTRICAS (formato largo: una fila por modelo, conjunto y métrica)
# -------------------------------------------------------------
metricas_out <- metricas |>
  pivot_longer(c(accuracy, auc, brier, log_loss),
               names_to = "metrica", values_to = "valor") |>
  filter(!is.na(valor)) |>
  mutate(metrica = recode(metrica,
                          accuracy = "Accuracy", auc = "AUC",
                          brier = "Brier score", log_loss = "Log loss"),
         mejor_si = if_else(metrica %in% c("Accuracy", "AUC"),
                            "Más alto", "Más bajo"))

guardar(metricas_out, "metricas_modelos")

# -------------------------------------------------------------
# 5. CALIBRACIÓN
# -------------------------------------------------------------
guardar(calibracion |> mutate(tramo = as.character(tramo)), "calibracion")

# -------------------------------------------------------------
# 6. IMPORTANCIA DE VARIABLES (nombres legibles)
# -------------------------------------------------------------
nombres_vars <- c(
  diff_elo      = "Diferencia de Elo general",
  diff_elo_sup  = "Diferencia de Elo en la superficie",
  diff_elo_mix  = "Diferencia de Elo combinado",
  diff_log_rank = "Diferencia de ranking",
  diff_edad     = "Diferencia de edad",
  diff_altura   = "Diferencia de altura",
  diff_exp      = "Diferencia de experiencia",
  cinco_sets    = "Partido a 5 sets",
  superficie    = "Superficie"
)

importancia_out <- importancia |>
  mutate(variable_nombre = coalesce(nombres_vars[variable], variable))

guardar(importancia_out, "importancia_variables")

# -------------------------------------------------------------
# 7. SIMULADOR CARA A CARA
# Tableau no puede ejecutar el modelo de R, así que precalculamos la
# probabilidad para TODAS las parejas del top N por Elo actual, en las
# 3 superficies y a 3 y 5 sets. En Tableau, el usuario elige jugador A,
# jugador B, superficie y formato con parámetros, y se filtra la fila.
# -------------------------------------------------------------
modelo_log <- modelos$logistica
niveles_sup <- modelo_log$xlevels$superficie      # superficies tal y como las vio el modelo

mediana_altura <- median(jugadores$altura, na.rm = TRUE)
mediana_edad   <- median(jugadores$edad,   na.rm = TRUE)

top <- jugadores |>
  filter(partidos >= 30) |>
  slice_min(rank_elo, n = TOP_CARA_A_CARA, with_ties = FALSE) |>
  transmute(player_id, player_name, elo_actual,
            ranking = pmax(replace_na(ranking, 2000), 1),
            edad    = replace_na(edad, mediana_edad),
            altura  = replace_na(altura, mediana_altura),
            n_partidos = partidos)

# Elo de cada jugador del top en cada superficie (1500 si nunca jugó en ella)
top_sup <- top |>
  select(player_id) |>
  cross_join(tibble(superficie = niveles_sup)) |>
  left_join(elo_historial |>
              group_by(player_id, superficie = surface_es) |>
              slice_tail(n = 1) |>
              ungroup() |>
              select(player_id, superficie, elo_sup),
            by = c("player_id", "superficie")) |>
  mutate(elo_sup = replace_na(elo_sup, 1500))

cara_a_cara <- top |>
  rename_with(~ paste0("a_", .)) |>
  cross_join(top |> rename_with(~ paste0("b_", .))) |>
  filter(a_player_id != b_player_id) |>
  cross_join(tibble(superficie = niveles_sup)) |>
  cross_join(tibble(best_of = c(3, 5))) |>
  left_join(top_sup |> rename(a_player_id = player_id, a_elo_sup = elo_sup),
            by = c("a_player_id", "superficie")) |>
  left_join(top_sup |> rename(b_player_id = player_id, b_elo_sup = elo_sup),
            by = c("b_player_id", "superficie")) |>
  mutate(
    a_elo_mix = PESO_GENERAL * a_elo_actual + (1 - PESO_GENERAL) * a_elo_sup,
    b_elo_mix = PESO_GENERAL * b_elo_actual + (1 - PESO_GENERAL) * b_elo_sup,
    # mismas variables y con el mismo nombre que en 03_modelo.R
    diff_elo_mix  = a_elo_mix - b_elo_mix,
    diff_elo_sup  = a_elo_sup - b_elo_sup,
    diff_log_rank = log(b_ranking) - log(a_ranking),
    diff_edad     = a_edad - b_edad,
    diff_altura   = a_altura - b_altura,
    diff_exp      = log1p(a_n_partidos) - log1p(b_n_partidos),
    cinco_sets    = as.integer(best_of == 5),
    superficie    = factor(superficie, levels = niveles_sup)
  )

cara_a_cara$prob_a <- predict(modelo_log, newdata = cara_a_cara, type = "response")

cara_a_cara_out <- cara_a_cara |>
  transmute(
    jugador_a = a_player_name, jugador_b = b_player_name,
    superficie = as.character(superficie),
    formato = if_else(best_of == 5, "5 sets (Grand Slam)", "3 sets"),
    prob_a = round(prob_a, 4),
    prob_b = round(1 - prob_a, 4),
    elo_a = round(a_elo_mix), elo_b = round(b_elo_mix),
    ranking_a = a_ranking, ranking_b = b_ranking,
    favorito = if_else(prob_a >= 0.5, jugador_a, jugador_b)
  )

guardar(cara_a_cara_out, "cara_a_cara")

# -------------------------------------------------------------
# 8. PERFILES: UNA FILA POR JUGADOR (scatter saque vs. resto)
# -------------------------------------------------------------
perfiles_out <- perfiles |>
  select(player_id, player_name, perfil, cluster, partidos, pct_victorias,
         starts_with("pct_"), any_of(c("ind_nivel_jugador", "ind_saque_jugador",
                                       "ind_resto_jugador")),
         pc1, pc2) |>
  left_join(jugadores |> select(player_id, elo_actual, ranking, mano),
            by = "player_id") |>
  mutate(across(where(is.double), ~ round(., 4)))

guardar(perfiles_out, "perfiles_jugadores")

# -------------------------------------------------------------
# 9. PERFILES EN FORMATO LARGO (una fila por jugador y estadística)
# Ideal para barras comparativas: cada jugador vs. la media de su perfil
# -------------------------------------------------------------
nombres_stats <- c(
  pct_victorias      = "% victorias",
  pct_aces           = "% aces",
  pct_dobles         = "% dobles faltas",
  pct_1er_dentro     = "% 1er saque dentro",
  pct_1er_ganado     = "% puntos ganados con 1er saque",
  pct_2o_ganado      = "% puntos ganados con 2º saque",
  pct_bp_salvados    = "% break points salvados",
  pct_resto_ganado   = "% puntos ganados al resto",
  pct_bp_convertidos = "% break points convertidos"
)

stats_largo <- perfiles |>
  select(player_name, perfil, all_of(names(nombres_stats))) |>
  pivot_longer(-c(player_name, perfil), names_to = "variable", values_to = "valor") |>
  group_by(variable) |>
  mutate(media_top30 = mean(valor),
         valor_z = (valor - mean(valor)) / sd(valor)) |>   # vs. el resto del top 30
  group_by(variable, perfil) |>
  mutate(media_perfil = mean(valor)) |>
  ungroup() |>
  mutate(estadistica = nombres_stats[variable],
         bloque = if_else(variable %in% c("pct_resto_ganado", "pct_bp_convertidos"),
                          "Resto", if_else(variable == "pct_victorias", "Nivel", "Saque")),
         across(where(is.double), ~ round(., 4)))

guardar(stats_largo, "perfiles_stats_largo")

# -------------------------------------------------------------
# 10. RESUMEN DE CADA PERFIL
# -------------------------------------------------------------
guardar(centros |> mutate(across(where(is.double), ~ round(., 4))),
        "perfiles_resumen")

cat("\nListo. Conecta en Tableau los CSV de la carpeta", dir_out, "\n")