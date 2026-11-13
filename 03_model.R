# =============================================================
# 03_modelo.R
# Proyecto: Predictor de partidos ATP + perfiles de juego
# Entrada : data/processed/partidos_elo.rds   (sale de 02_elo.R)
# Salida  : data/processed/predicciones.rds   (una fila por partido de test)
#           data/processed/metricas.rds        (comparación de modelos)
#           data/processed/importancia.rds     (peso de cada variable)
#           data/processed/calibracion.rds     (prob. predicha vs real)
#           data/processed/modelos.rds         (modelos entrenados)
#
# Paquetes: install.packages(c("tidyverse", "ranger", "pROC"))
# =============================================================

library(tidyverse)
library(ranger)   # random forest rápido
library(pROC)     # curva ROC y AUC

set.seed(2026)    # para que el sorteo j1/j2 y el random forest sean reproducibles

partidos <- read_rds("data/processed/partidos_elo.rds")

# -------------------------------------------------------------
# 1. DE "GANADOR/PERDEDOR" A "JUGADOR 1 / JUGADOR 2"
# Si dejamos las columnas como winner/loser, el modelo "sabe" quién
# ganó. Sorteamos al azar quién es j1 y la variable objetivo pasa a
# ser: ¿ganó el jugador 1? (aprox. 50 % de unos y 50 % de ceros)
# -------------------------------------------------------------
partidos <- partidos |>
  filter(year >= 2018) |>                 # 2017 = calentamiento del Elo
  mutate(gana_j1 = rbinom(n(), 1, 0.5))   # 1 = j1 es el ganador real

# Función auxiliar: devuelve el valor de j1 o j2 según el sorteo
elige <- function(es_j1, valor_winner, valor_loser) {
  if_else(es_j1 == 1, valor_winner, valor_loser)
}

datos <- partidos |>
  transmute(
    match_id, year, tourney_date, tourney_name, surface_es, nivel, round,
    best_of,
    gana_j1,
    
    j1_name = elige(gana_j1, winner_name, loser_name),
    j2_name = elige(gana_j1, loser_name,  winner_name),
    
    j1_elo     = elige(gana_j1, winner_elo,     loser_elo),
    j2_elo     = elige(gana_j1, loser_elo,      winner_elo),
    j1_elo_sup = elige(gana_j1, winner_elo_sup, loser_elo_sup),
    j2_elo_sup = elige(gana_j1, loser_elo_sup,  winner_elo_sup),
    # elo mix es media de elo y elo superficie
    j1_elo_mix = elige(gana_j1, winner_elo_mix, loser_elo_mix),
    j2_elo_mix = elige(gana_j1, loser_elo_mix,  winner_elo_mix),
    j1_rank    = elige(gana_j1, winner_rank,    loser_rank),
    j2_rank    = elige(gana_j1, loser_rank,     winner_rank),
    j1_edad    = elige(gana_j1, winner_age,     loser_age),
    j2_edad    = elige(gana_j1, loser_age,      winner_age),
    j1_altura  = elige(gana_j1, winner_ht,      loser_ht),
    j2_altura  = elige(gana_j1, loser_ht,       winner_ht),
    j1_n       = elige(gana_j1, winner_n,       loser_n),
    j2_n       = elige(gana_j1, loser_n,        winner_n)
  )

# -------------------------------------------------------------
# 2. TRATAMIENTO DE NULOS
# - Ranking vacío = jugador sin ranking -> le damos 2000
# - Edad / altura vacías -> mediana de todos los jugadores
# -------------------------------------------------------------
mediana_edad   <- median(c(datos$j1_edad,   datos$j2_edad),   na.rm = TRUE)
mediana_altura <- median(c(datos$j1_altura, datos$j2_altura), na.rm = TRUE)

datos <- datos |>
  mutate(
    across(c(j1_rank, j2_rank),     ~ pmax(replace_na(., 2000), 1)),
    across(c(j1_edad, j2_edad),     ~ replace_na(., mediana_edad)),
    across(c(j1_altura, j2_altura), ~ replace_na(., mediana_altura))
  )

# -------------------------------------------------------------
# 3. VARIABLES DEL MODELO (todas como diferencia j1 - j2)
# Trabajar con diferencias hace el modelo simétrico y fácil de leer:
# "cuanto más Elo tiene j1 respecto a j2, más probable que gane".
# -------------------------------------------------------------
datos <- datos |>
  mutate(
    diff_elo      = j1_elo - j2_elo,
    diff_elo_sup  = j1_elo_sup - j2_elo_sup,
    diff_elo_mix  = j1_elo_mix - j2_elo_mix,
    # log del ranking: pasar del 1 al 10 importa mucho más que del 101 al 110
    diff_log_rank = log(j2_rank) - log(j1_rank),   # positivo = j1 mejor clasificado
    diff_edad     = j1_edad - j2_edad,
    diff_altura   = j1_altura - j2_altura,
    diff_exp      = log1p(j1_n) - log1p(j2_n),    # experiencia (nº de partidos)
    cinco_sets    = as.integer(best_of == 5),
    superficie    = factor(surface_es),
    gana_j1_f     = factor(gana_j1, levels = c(0, 1))
  )

# -------------------------------------------------------------
# 4. DIVISIÓN TEMPORAL (nunca aleatoria: no se puede usar el futuro)
# -------------------------------------------------------------
train <- datos |> filter(year <= 2023)
test  <- datos |> filter(year %in% 2024:2025)
vivo  <- datos |> filter(year == 2026)          # temporada en curso

cat("Train:", nrow(train), "| Test:", nrow(test), "| 2026:", nrow(vivo), "\n")

# -------------------------------------------------------------
# 5. MODELOS
# -------------------------------------------------------------

# 5.1 Regresión logística
# Las variables de superficie y 5 sets entran como interacción con el Elo:
# no hacen ganar a nadie por sí mismas, pero cambian cuánto pesa el Elo.
formula_log <- gana_j1 ~ diff_elo_mix + diff_elo_sup + diff_log_rank +
  diff_edad + diff_altura + diff_exp +
  diff_elo_mix:cinco_sets + diff_elo_mix:superficie

modelo_log <- glm(formula_log, data = train, family = binomial)
summary(modelo_log)

# 5.2 Random forest
vars_rf <- c("diff_elo", "diff_elo_sup", "diff_elo_mix", "diff_log_rank",
             "diff_edad", "diff_altura", "diff_exp", "cinco_sets", "superficie")

modelo_rf <- ranger(
  x = train[, vars_rf], y = train$gana_j1_f,
  num.trees = 500,
  min.node.size = 50,        # hojas grandes = menos sobreajuste
  probability = TRUE,
  importance = "permutation"
)

# -------------------------------------------------------------
# 6. PREDICCIONES (prob. de que gane j1)
# -------------------------------------------------------------
predecir <- function(df) {
  df |>
    mutate(
      prob_ranking = if_else(diff_log_rank > 0, 1, if_else(diff_log_rank < 0, 0, 0.5)),
      prob_elo     = 1 / (1 + 10^(-diff_elo_mix / 400)),
      prob_log     = predict(modelo_log, newdata = df, type = "response"),
      prob_rf      = predict(modelo_rf, data = df[, vars_rf])$predictions[, "1"]
    )
}

test <- predecir(test)
vivo <- predecir(vivo)

# -------------------------------------------------------------
# 7. MÉTRICAS
# - Accuracy: % de partidos donde el favorito del modelo ganó
# - AUC: capacidad de ordenar (0,5 = azar, 1 = perfecto)
# - Brier: error medio de las probabilidades (más bajo = mejor)
# - Log loss: penaliza mucho estar muy seguro y fallar
# -------------------------------------------------------------
metricas_de <- function(real, prob, modelo, conjunto) {
  p <- pmin(pmax(prob, 1e-6), 1 - 1e-6)
  tibble(
    conjunto = conjunto,
    modelo   = modelo,
    partidos = length(real),
    accuracy = mean((prob > 0.5) == real),
    auc      = as.numeric(auc(roc(real, prob, quiet = TRUE))),
    brier    = mean((prob - real)^2),
    log_loss = -mean(real * log(p) + (1 - real) * log(1 - p))
  )
}

nombres_modelos <- c(prob_ranking = "Baseline: ranking",
                     prob_elo     = "Baseline: Elo",
                     prob_log     = "Regresión logística",
                     prob_rf      = "Random forest")

metricas <- bind_rows(
  map_dfr(names(nombres_modelos), function(col) {
    metricas_de(test$gana_j1, test[[col]], nombres_modelos[[col]], "Test 2024-2025")
  }),
  map_dfr(names(nombres_modelos), function(col) {
    metricas_de(vivo$gana_j1, vivo[[col]], nombres_modelos[[col]], "Temporada 2026")
  })
) |>
  mutate(across(c(auc, brier, log_loss), ~ if_else(modelo == "Baseline: ranking", NA_real_, .)))
# (el baseline de ranking solo da 0/1, así que solo tiene sentido su accuracy)

print(metricas, width = Inf)

# Accuracy por superficie (test)
test |>
  group_by(superficie) |>
  summarise(partidos = n(),
            acc_ranking = mean((prob_ranking > 0.5) == gana_j1),
            acc_log     = mean((prob_log > 0.5) == gana_j1),
            acc_rf      = mean((prob_rf  > 0.5) == gana_j1)) |>
  print()

# -------------------------------------------------------------
# 8. IMPORTANCIA DE VARIABLES
# -------------------------------------------------------------
# Logística: coeficientes como odds ratio (por cada 100 puntos de Elo, etc.)
coef_log <- tibble(
  variable    = names(coef(modelo_log)),
  coeficiente = unname(coef(modelo_log)),
  p_valor     = summary(modelo_log)$coefficients[, 4]
) |>
  filter(variable != "(Intercept)")
print(coef_log)

importancia <- tibble(
  variable    = names(modelo_rf$variable.importance),
  importancia = unname(modelo_rf$variable.importance)
) |>
  mutate(importancia_rel = importancia / max(importancia)) |>
  arrange(desc(importancia))
print(importancia)

# -------------------------------------------------------------
# 9. CALIBRACIÓN: cuando el modelo dice 70 %, ¿gana el 70 %?
# -------------------------------------------------------------
calibracion <- test |>
  select(gana_j1, prob_log, prob_rf, prob_elo) |>
  pivot_longer(-gana_j1, names_to = "modelo", values_to = "prob") |>
  mutate(modelo = nombres_modelos[modelo],
         tramo  = cut(prob, breaks = seq(0, 1, 0.1), include.lowest = TRUE)) |>
  group_by(modelo, tramo) |>
  summarise(partidos      = n(),
            prob_media    = mean(prob),
            victorias_real = mean(gana_j1),
            .groups = "drop")

# -------------------------------------------------------------
# 10. TABLA DE PREDICCIONES PARA TABLEAU
# Elegimos la regresión logística como modelo final (o cámbialo a
# prob_rf si el random forest sale claramente mejor en las métricas).
# -------------------------------------------------------------
predicciones <- bind_rows(
  test |> mutate(conjunto = "Test 2024-2025"),
  vivo |> mutate(conjunto = "Temporada 2026")
) |>
  transmute(
    match_id, conjunto, year, tourney_date, tourney_name, nivel, round,
    superficie = as.character(superficie),
    j1_name, j2_name, j1_elo_mix, j2_elo_mix, j1_rank, j2_rank,
    prob_j1        = prob_log,
    prob_j1_rf     = prob_rf,
    prob_j1_elo    = prob_elo,
    ganador_real   = if_else(gana_j1 == 1, j1_name, j2_name),
    favorito       = if_else(prob_log >= 0.5, j1_name, j2_name),
    prob_favorito  = pmax(prob_log, 1 - prob_log),
    acierto        = (prob_log > 0.5) == gana_j1,
    sorpresa       = !acierto & prob_favorito >= 0.75   # "upsets" llamativos
  )

# -------------------------------------------------------------
# 11. GUARDADO
# -------------------------------------------------------------
write_rds(predicciones, "data/processed/predicciones.rds")
write_rds(metricas,     "data/processed/metricas.rds")
write_rds(importancia,  "data/processed/importancia.rds")
write_rds(coef_log,     "data/processed/coeficientes_log.rds")
write_rds(calibracion,  "data/processed/calibracion.rds")
write_rds(list(logistica = modelo_log, random_forest = modelo_rf,
               vars_rf = vars_rf),
          "data/processed/modelos.rds")

predicciones <- readr::read_rds("data/processed/predicciones.rds")
View(predicciones)

metricas <- readr::read_rds("data/processed/metricas.rds")
View(metricas)

importancia <- readr::read_rds("data/processed/importancia.rds")
View(importancia)

coef_log <- readr::read_rds("data/processed/coeficientes_log.rds")
View(coef_log)

calibracion <- readr::read_rds("data/processed/calibracion.rds")
View(calibracion)