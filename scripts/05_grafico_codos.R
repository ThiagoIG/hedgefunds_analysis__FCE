# 1. Cargar librerías necesarias
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(factoextra)

# 2. Cargar datasets
# Dataset crudo (formato largo)
hedge_fund_long <- read_csv("data/clean/hedge_fund_long.csv")

# Dataset con winsorización y otras columnas
hedge_fund_long_winzo <- read_csv("data/clean/hedge_fund_long_winzo.csv")

# 3. Filtrar columnas necesarias del dataset winzo
hedge_fund_winzo_filtered <- hedge_fund_long_winzo %>%
  select(Fecha, Estrategia, Rendimiento_winsorized)

# 4. Convertir a formato ancho ambos datasets
hedge_fund_wide_original <- hedge_fund_long %>%
  select(Fecha, Estrategia, Rendimiento) %>%
  pivot_wider(names_from = Estrategia, values_from = Rendimiento)

hedge_fund_wide_winzo <- hedge_fund_winzo_filtered %>%
  pivot_wider(names_from = Estrategia, values_from = Rendimiento_winsorized)

# 5. Guardar los datasets transformados
write_csv(hedge_fund_wide_original, "data/clean/hedge_fund_wide_original.csv")
write_csv(hedge_fund_wide_winzo, "data/clean/hedge_fund_wide_winzo.csv")


# 6. Estandarizar datasets
hedge_fund_wide_origina_scaled <- hedge_fund_wide_original %>%
  select(-Fecha) %>%
  scale()

hedge_fund_wide_winzo_scaled <- hedge_fund_wide_winzo %>%
  select(-Fecha) %>%
  scale()

# 7. Graficar el Codo - Original
fviz_nbclust(hedge_fund_wide_origina_scaled, kmeans, method = "wss", k.max = 10) +
  labs(title = "Gráfico del Codo - Dataset Original")

# 8. Graficar el Codo - Winzo (sin outliers)
fviz_nbclust(hedge_fund_wide_winzo_scaled, kmeans, method = "wss", k.max = 10) +
  labs(title = "Gráfico del Codo")

## Grafico Original Dendrograma ##

# Calcular matriz de correlación y distancia para el dataset original
cor_matrix_original <- cor(hedge_fund_wide_origina_scaled, use = "pairwise.complete.obs", method = "pearson")
dist_cor_original <- as.dist(1 - cor_matrix_original)

# Clustering jerárquico
hc_original <- hclust(dist_cor_original, method = "complete")

# Graficar dendrograma
dendrograma_original <- fviz_dend(hc_original, 
          main = "Dendrograma - Dataset Original", 
          cex = 0.8, 
          k = 3, # opcional: mostrar 3 clusters
          rect = TRUE, 
          rect_border = "jco",
          rect_fill = TRUE)

 ## Grafico sin outliers Dendrograma ##

# Calcular matriz de correlación y distancia para el dataset winzo
cor_matrix_winzo <- cor(hedge_fund_wide_winzo_scaled, use = "pairwise.complete.obs", method = "pearson")
dist_cor_winzo <- as.dist(1 - cor_matrix_winzo)

# Clustering jerárquico
hc_winzo <- hclust(dist_cor_winzo, method = "complete")

# Graficar dendrograma
dendrograma_winzo <- fviz_dend(hc_winzo, 
          main = "Dendrograma Outliers tratados", 
          cex = 0.8, 
          k = 3, # opcional: mostrar 3 clusters
          rect = TRUE, 
          rect_border = "jco",
          rect_fill = TRUE)

library(patchwork)
dendrograma_original + dendrograma_winzo

# ============================================================================
# 9. Dendrograma y gráfico del codo EXCLUYENDO CTA Global y Short Selling
# ============================================================================

# Eliminar esas dos estrategias del dataset winzorizado escalado
estrategias_a_excluir <- c("CTA Global", "Short Selling")

# Filtrar columnas excluyendo las estrategias
winzo_filtrado <- hedge_fund_wide_winzo %>%
  select(-Fecha) %>%
  select(-all_of(estrategias_a_excluir))

# Escalar nuevamente el dataset filtrado
winzo_filtrado_scaled <- scale(winzo_filtrado)

# ---- Dendrograma sin CTA Global y Short Selling ----
cor_matrix_filtrada <- cor(winzo_filtrado_scaled, use = "pairwise.complete.obs", method = "pearson")
dist_filtrada <- as.dist(1 - cor_matrix_filtrada)
hc_filtrado <- hclust(dist_filtrada, method = "complete")

fviz_dend(hc_filtrado,
          main = "Dendrograma (sin CTA Global y Short Selling)",
          cex = 0.8,
          k = 3,
          rect = TRUE,
          rect_border = "jco",
          rect_fill = TRUE)

# ---- Gráfico del codo sin CTA Global y Short Selling ----
fviz_nbclust(winzo_filtrado_scaled, kmeans, method = "wss", k.max = 10) +
  labs(title = "Gráfico del Codo - Winzo (sin CTA Global y Short Selling)")

## PARA BASE DE DATOS SIN TRATAR##

# Preparar el dataset eliminando fecha
original_filtrado <- hedge_fund_wide_original %>%
  select(-Fecha) %>%
  select(-all_of(estrategias_a_excluir))

# Escalar nuevamente el dataset sin filtrar
original_filtrado_scaled <- scale(original_filtrado)

# ---- Dendrograma con CTA Global y Short Selling ----
cor_matrix_filtrada <- cor(original_filtrado_scaled, use = "pairwise.complete.obs", method = "pearson")
dist_filtrada <- as.dist(1 - cor_matrix_filtrada)
hc_filtrado <- hclust(dist_filtrada, method = "complete")

fviz_dend(hc_filtrado,
          main = "Dendrograma base original (sin CTA Global y Short Selling)",
          cex = 0.8,
          k = 3,
          rect = TRUE,
          rect_border = "jco",
          rect_fill = TRUE)

# ---- Gráfico del codo ----
fviz_nbclust(original_filtrado_scaled, kmeans, method = "wss", k.max = 10) +
  labs(title = "Gráfico del Codo - Original (sin CTA Global y Short Selling)")
