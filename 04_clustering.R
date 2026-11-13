# =============================================================
# 04_clustering.R
# Proyecto: Predictor de partidos ATP + perfiles de juego
# Entrada : data/processed/partidos.rds   (sale de 01_carga_limpieza.R)
# Salida  : data/processed/perfiles.rds   (una fila por jugador + su perfil)
#           data/processed/centros.rds    (estadísticas medias de cada perfil)
#
# Paquetes: tidyverse y cluster (cluster viene instalado con R)
# =============================================================

library(tidyverse)
library(cluster)   # silhouette()

set.seed(2026)

partidos <- read_rds("data/processed/partidos.rds")

# -------------------------------------------------------------
# 1. PARÁMETROS
# -------------------------------------------------------------
ANIO_DESDE   <- 2021   # perfiles "actuales": últimas temporadas
MIN_PARTIDOS <- 60     # mínimo de partidos con estadísticas por jugador
TOP_N        <- 30     # nos quedamos con los N jugadores con más % de victorias
K            <- 4      # nº de perfiles (con 30 jugadores, 3 o 4 es lo razonable)

# Peso del % de victorias frente a cada variable de estilo.
# Como ya filtramos a los mejores, el nivel importa menos: con 1 cuenta
# como una variable más y el clustering se centra en el ESTILO.
#  0 = solo estilo  |  1 = una variable más  |  2 = el nivel pesa como 4 variables
PESO_VICTORIAS <- 1

# -------------------------------------------------------------
# 2. DE PARTIDOS A "UNA FILA POR JUGADOR Y PARTIDO"
# Para cada jugador necesitamos:
#  - sus estadísticas de saque (las suyas)
#  - las estadísticas de saque de su rival (para medir su resto)
# -------------------------------------------------------------
stats <- c("ace", "df", "svpt", "1stIn", "1stWon", "2ndWon",
           "SvGms", "bpSaved", "bpFaced")

base <- partidos |>
  filter(year >= ANIO_DESDE, !is.na(w_svpt), !is.na(l_svpt),
         w_svpt > 0, l_svpt > 0)

lado_ganador <- base |>
  transmute(match_id, surface_es,
            player_id = winner_id, player_name = winner_name,
            victoria = 1,
            across(all_of(paste0("w_", stats)), .names = "{str_remove(.col, '^w_')}"),
            across(all_of(paste0("l_", stats)), .names = "rival_{str_remove(.col, '^l_')}"))

lado_perdedor <- base |>
  transmute(match_id, surface_es,
            player_id = loser_id, player_name = loser_name,
            victoria = 0,
            across(all_of(paste0("l_", stats)), .names = "{str_remove(.col, '^l_')}"),
            across(all_of(paste0("w_", stats)), .names = "rival_{str_remove(.col, '^w_')}"))

por_partido <- bind_rows(lado_ganador, lado_perdedor)

# -------------------------------------------------------------
# 3. ESTADÍSTICAS AGREGADAS POR JUGADOR
# Sumamos primero y dividimos después (no media de porcentajes):
# así un partido de 5 sets pesa más que uno de 2 sets.
# -------------------------------------------------------------
jugadores <- por_partido |>
  group_by(player_id, player_name) |>
  summarise(
    partidos     = n(),
    pct_victorias = mean(victoria),
    across(c(all_of(stats), starts_with("rival_")), ~ sum(., na.rm = TRUE)),
    .groups = "drop"
  ) |>
  filter(partidos >= MIN_PARTIDOS) |>
  # --- Solo los mejores: top N por % de victorias ---
  slice_max(pct_victorias, n = TOP_N, with_ties = FALSE) |>
  mutate(
    # --- SAQUE ---
    pct_aces        = ace / svpt,
    pct_dobles      = df / svpt,
    pct_1er_dentro  = `1stIn` / svpt,
    pct_1er_ganado  = `1stWon` / `1stIn`,
    pct_2o_ganado   = `2ndWon` / (svpt - `1stIn`),
    pct_bp_salvados = bpSaved / bpFaced,
    # --- RESTO (lo que el rival NO gana con su saque) ---
    pct_resto_ganado = 1 - (rival_1stWon + rival_2ndWon) / rival_svpt,
    pct_bp_convertidos = (rival_bpFaced - rival_bpSaved) / rival_bpFaced
  ) |>
  select(player_id, player_name, partidos, pct_victorias, starts_with("pct_"))

cat("Top", nrow(jugadores), "jugadores (mín.", MIN_PARTIDOS, "partidos desde", ANIO_DESDE, ")\n")
print(jugadores |> select(player_name, partidos, pct_victorias), n = Inf)

# -------------------------------------------------------------
# 4. PREPARACIÓN: ESCALADO
# OJO: al escalar SOLO con el top N, las comparaciones son relativas a
# ese grupo. "Restador" significará "resta mejor que el resto de los
# mejores", no "resta mejor que la media del circuito".
# k-means usa distancias: sin escalar, una variable que va de 0,6 a 0,8
# pesaría distinto que otra que va de 0,02 a 0,15. Con scale() todas
# pasan a tener media 0 y desviación 1.
# -------------------------------------------------------------
vars_cluster <- c("pct_victorias",
                  "pct_aces", "pct_dobles", "pct_1er_dentro", "pct_1er_ganado",
                  "pct_2o_ganado", "pct_bp_salvados",
                  "pct_resto_ganado", "pct_bp_convertidos")

X <- jugadores |>
  select(all_of(vars_cluster)) |>
  scale()

# Damos más peso al % de victorias multiplicando su columna ya escalada.
# k-means usa distancias al cuadrado, así que con peso 2 esta variable
# aporta 4 veces más a la distancia que cualquier variable de estilo.
X[, "pct_victorias"] <- X[, "pct_victorias"] * PESO_VICTORIAS

# -------------------------------------------------------------
# 5. ¿CUÁNTOS PERFILES? MÉTODO DEL CODO + SILUETA
# - Codo: buscamos el k a partir del cual añadir grupos ya no reduce
#   mucho la variabilidad interna.
# - Silueta: de -1 a 1; cuanto más alta, mejor separados los grupos.
# -------------------------------------------------------------
eleccion_k <- map_dfr(2:6, function(k) {   # con ~30 jugadores no tiene sentido ir más allá
  km  <- kmeans(X, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist(X))
  tibble(k = k,
         var_intra = km$tot.withinss,
         silueta   = mean(sil[, "sil_width"]))
})
print(eleccion_k)

grafico_codo <- ggplot(eleccion_k, aes(k, var_intra)) +
  geom_line() + geom_point(size = 2) +
  labs(title = "Método del codo", x = "Número de perfiles (k)",
       y = "Variabilidad dentro de los grupos") +
  theme_minimal()
print(grafico_codo)

# -------------------------------------------------------------
# 6. K-MEANS FINAL
# nstart = 25: prueba 25 puntos de partida y se queda con el mejor
# -------------------------------------------------------------
km <- kmeans(X, centers = K, nstart = 25)
jugadores$cluster <- km$cluster

# -------------------------------------------------------------
# 7. DAR NOMBRE A LOS PERFILES
# Resumimos cada grupo en tres índices (en desviaciones respecto a la media):
#  - indice_nivel: % de victorias
#  - indice_saque: aces, 1er y 2º saque ganados, break points salvados
#  - indice_resto: puntos ganados al resto y break points convertidos
# Primero miramos el nivel y después el estilo. REVISA los nombres con la tabla
# de centros: puedes sobrescribirlos a mano al final de este bloque.
# -------------------------------------------------------------
centros_z <- as_tibble(km$centers) |>
  mutate(cluster = row_number(),
         indice_nivel = pct_victorias / PESO_VICTORIAS,   # deshacemos el peso
         indice_saque = (pct_aces + pct_1er_ganado + pct_2o_ganado + pct_bp_salvados) / 4,
         indice_resto = (pct_resto_ganado + pct_bp_convertidos) / 2)

umbral <- 0.25   # cuánto por encima/debajo de la media para considerarlo "fuerte"

centros_z <- centros_z |>
  mutate(perfil = case_when(
    # Todos son jugadores top: los nombres describen el ESTILO
    # (siempre en comparación con el resto del top N)
    indice_saque >  umbral & indice_resto >  umbral ~ "Completo",
    indice_saque >  umbral & indice_resto <= umbral ~ "Gran sacador",
    indice_saque <= umbral & indice_resto >  umbral ~ "Restador / defensivo",
    indice_saque < -umbral & indice_resto < -umbral ~ "Competidor / mental",   # gana sin dominar las estadísticas
    TRUE                                            ~ "Equilibrado"
  )) |>
  # si dos grupos reciben el mismo nombre, se numeran
  group_by(perfil) |>
  mutate(perfil = if (n() > 1) paste(perfil, row_number()) else perfil) |>
  ungroup()

print(centros_z |> select(cluster, perfil, indice_nivel, indice_saque, indice_resto,
                          all_of(vars_cluster)), width = Inf)

# Para poner nombres a mano, descomenta y edita (el orden es el de cluster 1, 2, 3...):
# centros_z$perfil <- c("Gran sacador", "Restador", "Completo", "Equilibrado")

jugadores <- jugadores |>
  left_join(centros_z |> select(cluster, perfil, indice_nivel, indice_saque, indice_resto),
            by = "cluster")

# -------------------------------------------------------------
# 8. COORDENADAS 2D PARA EL GRÁFICO (PCA)
# Resume las 8 variables en 2 ejes para pintar un scatter en Tableau.
# -------------------------------------------------------------
pca <- prcomp(X)
jugadores <- jugadores |>
  mutate(pc1 = pca$x[, 1], pc2 = pca$x[, 2])

cat("Varianza explicada por PC1 + PC2:",
    round(sum(summary(pca)$importance[2, 1:2]) * 100, 1), "%\n")
print(round(pca$rotation[, 1:2], 2))   # qué variables "tiran" de cada eje

# Índices INDIVIDUALES (cada jugador con los suyos), a partir de sus
# variables escaladas. Los de arriba (indice_*) son los del PERFIL.
Xdf <- as_tibble(X)
jugadores <- jugadores |>
  mutate(
    ind_nivel_jugador = as.numeric(scale(pct_victorias)),
    ind_saque_jugador = (Xdf$pct_aces + Xdf$pct_1er_ganado +
                           Xdf$pct_2o_ganado + Xdf$pct_bp_salvados) / 4,
    ind_resto_jugador = (Xdf$pct_resto_ganado + Xdf$pct_bp_convertidos) / 2
  )

# -------------------------------------------------------------
# 9. RESUMEN DE CADA PERFIL (en unidades reales, para las fichas)
# -------------------------------------------------------------
centros <- jugadores |>
  group_by(cluster, perfil) |>
  summarise(jugadores = n(),
            # todos los jugadores del perfil, de más a menos % de victorias
            # (se calcula ANTES de convertir pct_victorias en media)
            jugadores_lista = paste(player_name[order(-pct_victorias)],
                                    collapse = ", "),
            # rango de % de victorias dentro del perfil
            min_victorias = min(pct_victorias),
            max_victorias = max(pct_victorias),
            across(all_of(vars_cluster), mean),
            .groups = "drop")

print(centros, width = Inf)

# -------------------------------------------------------------
# 10. GUARDADO
# -------------------------------------------------------------
write_rds(jugadores,  "data/processed/perfiles.rds")
write_rds(centros,    "data/processed/centros.rds")
write_rds(eleccion_k, "data/processed/eleccion_k.rds")

jugadores <- readr::read_rds("data/processed/perfiles.rds")
View(jugadores)

centros <- readr::read_rds("data/processed/centros.rds")
View(centros)

eleccion_k <- readr::read_rds("data/processed/eleccion_k.rds")
View(eleccion_k)