# ============================================================================
# 0. CONFIGURACIÓN INICIAL
# ============================================================================

# Limpiar entorno de trabajo
rm(list = ls())

# Cargar librerías necesarias
library(tidyverse)    # Manipulación de datos
library(readr)
library(psych)        # KMO, test de Bartlett (funciones psicométricas)
library(corrplot)     # Gráficos de matriz de correlación
library(factoextra)   # Visualización de PCA (fviz_pca_biplot, fviz_pca_var, etc.)
library(ggrepel)      # Etiquetas en gráficos ggplot
library(kableExtra)   # (Opcional) Mejoras en tablas
library(patchwork)    # (Opcional) Combinar gráficos
library(ggplot2)

# Opcional: establecer tema global de ggplot para gráficos consistentes
theme_set(theme_minimal(base_size = 12))

# Semilla para reproducibilidad (si aplica aleatoriedad en PCA o segmentación)
set.seed(2025)


# ============================================================================
# 1. CARGA Y PREPARACIÓN DE LOS DATOS
# ============================================================================

# Leer la base de datos (formato CSV) - **ajusta la ruta si es necesario**
datos_raw <- read_csv("data/clean/hedge_fund_wide_winzo.csv")

# Inspeccionar brevemente los datos
cat("Columnas en el dataset:\n")
print(names(datos_raw))

# Eliminar la primera columna Fecha
datos <- datos_raw[, -1]

# Extraer matriz numérica para PCA
matriz_pca <- as.data.frame(datos)

# Comprobar las dimensiones finales de la matriz para PCA
p <- ncol(matriz_pca)  # número final de variables
n <- nrow(matriz_pca)  # número final de observaciones
cat("\nDatos listos para PCA: observaciones =", n, ", variables =", p, "\n")


# ============================================================================
# 2. ANÁLISIS EXPLORATORIO INICIAL
# ============================================================================

# 2. Matriz de correlaciones entre variables
cor_matrix <- cor(matriz_pca)
cat("\nMatriz de correlaciones (primeras filas/columnas redondeadas):\n")
print(round(cor_matrix[1:min(5,p), 1:min(5,p)], 2))  # mostrar una porción si p es grande

# Visualización de la matriz de correlaciones
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         addCoef.col = "black", number.cex = 0.6,
         tl.col = "black", tl.srt = 45, tl.cex = 0.8,
         title = "Matriz de correlación entre variables", mar = c(0,0,2,0),
         col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", 
                                  "#77AADD", "#4477AA"))(200))


# Eliminar "Short Selling" y "CTA Global" dada su baja correlacion con las demas.
datos_filtrados <- datos %>%
  select(-`Short Selling`, -`CTA Global`)

# Calcular matriz de correlación
cor_matrix_filtrada <- cor(datos_filtrados)

# Visualizar la nueva matriz de correlación
corrplot(cor_matrix_filtrada, method = "color", type = "upper", order = "hclust",
         addCoef.col = "black", number.cex = 0.6,
         tl.col = "black", tl.srt = 45, tl.cex = 0.8,
         title = "Matriz de correlación sin estrategias atípicas", mar = c(0,0,2,0),
         col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", 
                                  "#77AADD", "#4477AA"))(200))

# . DETECCIÓN DE CORRELACIONES FUERTES (|r| > 0.7)


correlaciones_fuertes_filtradas <- as.data.frame(as.table(cor_matrix_filtrada)) %>% 
  filter(Var1 != Var2) %>%
  mutate(abs_cor = abs(Freq)) %>%
  filter(abs_cor > 0.7) %>%
  arrange(desc(abs_cor)) %>%
  distinct(paste(pmin(Var1,Var2), pmax(Var1,Var2), sep = "_"), .keep_all = TRUE)

cat("\nCorrelaciones fuertes detectadas (|r| > 0.7) tras remover estrategias atípicas:\n")
if(nrow(correlaciones_fuertes_filtradas) == 0) {
  cat("No hay pares de variables con |r| > 0.7.\n")
} else {
  print(correlaciones_fuertes_filtradas, row.names = FALSE)
}

# Crear un heatmap solo con los pares fuertemente correlacionados
library(ggplot2)

# Usamos el mismo dataframe de correlaciones fuertes
ggplot(correlaciones_fuertes_filtradas, aes(x = Var1, y = Var2, fill = abs_cor)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(abs_cor, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "white", high = "steelblue", midpoint = 0.85, limits = c(0.7, 1)) +
  labs(title = "Correlaciones Fuertes entre Estrategias (|r| > 0.7)",
       x = "Estrategia 1", y = "Estrategia 2", fill = "|Correlación|") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Elimnamos la variable (Estrategia) CTA GLOBAL del dataframe ##

matriz_pca_sin_cta <- matriz_pca %>% select(-`CTA Global`)



# ============================================================================
# 3. VERIFICACIÓN DE SUPUESTOS PARA PCA
# ============================================================================

cat("\n========== VERIFICACIÓN DE SUPUESTOS PCA ==========\n")

# 3.1 Proporción de muestras a variables (n:p)
ratio_np <- n / p
cat("Número de observaciones (n):", n, "\n")
cat("Número de variables (p):", p, "\n")
cat("Ratio n:p =", round(ratio_np, 2), "\n")
if(ratio_np >= 10) {
  cat("✓ Ratio n:p ≥ 10: Tamaño muestral IDEAL para PCA.\n")
} else if(ratio_np >= 5) {
  cat("✓ Ratio n:p ≥ 5: Tamaño muestral ACEPTABLE para PCA.\n")
} else if(ratio_np >= 2) {
  cat("⚠ Ratio n:p ≥ 2: Tamaño muestral algo limitado (PCA puede ser menos robusto).\n")
} else {
  cat("✗ Ratio n:p < 2: Tamaño muestral insuficiente para PCA confiable.\n")
}

# 3.2 Test de Kaiser-Meyer-Olkin (KMO) - adecuación muestral
kmo_result <- KMO(matriz_pca)
cat("\nKMO global:", round(kmo_result$MSA, 3), "\n")
if(kmo_result$MSA >= 0.9) {
  cat("✓ KMO > 0.9: MARAVILLOSO (excelente adecuación para PCA).\n")
} else if(kmo_result$MSA >= 0.8) {
  cat("✓ KMO > 0.8: MERITORIO (muy bueno para PCA).\n")
} else if(kmo_result$MSA >= 0.7) {
  cat("✓ KMO > 0.7: MEDIO (aceptable para PCA).\n")
} else if(kmo_result$MSA >= 0.6) {
  cat("⚠ KMO > 0.6: REGULAR (algo cuestionable para PCA).\n")
} else if(kmo_result$MSA >= 0.5) {
  cat("⚠ KMO > 0.5: BAJO (PCA de poca utilidad con estos datos).\n")
} else {
  cat("✗ KMO < 0.5: INACEPTABLE (variables no adecuadas para PCA).\n")
}

# Mostrar KMO individual por variable
kmo_por_var <- data.frame(Variable = names(kmo_result$MSAi),
                          KMO = round(kmo_result$MSAi, 3)) %>% arrange(KMO)
cat("\nKMO por variable:\n")
print(kmo_por_var, row.names = FALSE)
if(any(kmo_por_var$KMO < 0.5)) {
  cat("⚠ Variables con KMO < 0.5 (podrían eliminarse para mejorar PCA):\n")
  print(kmo_por_var %>% filter(KMO < 0.5), row.names = FALSE)
}

# 3.3 Test de esfericidad de Bartlett
bartlett_result <- cortest.bartlett(cor_matrix, n = n)
cat("\nTest de Bartlett (esfericidad):\n")
cat("Chi-cuadrado =", round(bartlett_result$chisq, 2), 
    " | gl =", bartlett_result$df, 
    " | p-value =", format(bartlett_result$p.value, scientific = TRUE), "\n")
if(bartlett_result$p.value < 0.05) {
  cat("✓ p < 0.05: Las variables están correlacionadas (rechaza H0 de matriz identidad) → PCA es pertinente.\n")
} else {
  cat("✗ p ≥ 0.05: No se rechaza H0 (matriz de correlación ~ identidad) → PCA no sería adecuado.\n")
}

# 3.4 Determinante de la matriz de correlación (indicador de multicolinealidad)
det_cor <- det(cor_matrix)
cat("\nDeterminante de la matriz de correlación =", format(det_cor, scientific = TRUE), "\n")
if(det_cor < 1e-5) {
  cat("✓ Determinante muy pequeño → Alta multicolinealidad (variables redundantes, bueno para PCA).\n")
} else if(det_cor < 1e-3) {
  cat("✓ Determinante pequeño → Cierta multicolinealidad (adecuado para PCA).\n")
} else {
  cat("⚠ Determinante relativamente grande → Baja colinealidad (PCA podría ser menos útil al no haber mucha redundancia).\n")
}


# ============================================================================
# 4. REALIZAR PCA
# ============================================================================

cat("\n========== EJECUTANDO PCA ==========\n")

# 4.1 Ejecutar PCA con variables estandarizadas (scale. = TRUE)
pca_result <- prcomp(matriz_pca_sin_cta, scale. = TRUE)
cat("✓ PCA calculado exitosamente.\n")

# 4.2 Varianza explicada por componentes principales
summary_pca <- summary(pca_result)
var_exp_porcent <- summary_pca$importance[2, ] * 100       # % de varianza explicada por cada PC
var_exp_acum_porcent <- summary_pca$importance[3, ] * 100  # % acumulado

cat("\nResumen de varianza explicada (%):\n")
# Imprimir proporción de varianza explicada (y acumulada) por los primeros componentes
porcentaje_var <- round(var_exp_porcent, 2)
porcentaje_acum <- round(var_exp_acum_porcent, 2)
for(i in 1:length(porcentaje_var)) {
  cat(paste0("PC", i, ": ", porcentaje_var[i], "% (acumulado: ", porcentaje_acum[i], "%)\n"))
  if(i == min(10, length(porcentaje_var))) break  # limitar a primeros 10 para brevedad
}

# Tabla de varianza explicada (primeros 10 componentes o menos)
tabla_varianza <- data.frame(
  Componente = paste0("PC", 1:length(var_exp_porcent)),
  Varianza_Porcent = round(var_exp_porcent, 2),
  Varianza_Acumulada = round(var_exp_acum_porcent, 2),
  Eigenvalue = round(pca_result$sdev^2, 3)
)
tabla_varianza_10 <- head(tabla_varianza, 10)
cat("\nTabla de varianza explicada (primeros 10 PC):\n")
print(tabla_varianza_10, row.names = FALSE)

# Criterios para decidir número de componentes a retener
eigenvalues <- pca_result$sdev^2
n_kaiser <- sum(eigenvalues > 1)             # criterio de Kaiser (autovalor > 1)
n_var80  <- min(which(var_exp_acum_porcent >= 80))  # componentes para llegar al 80% varianza
n_var90  <- min(which(var_exp_acum_porcent >= 90))  # componentes para llegar al 90% varianza

cat("\nComponentes a retener según criterios:\n")
cat("• Criterio de Kaiser (λ > 1):", n_kaiser, "componentes\n")
cat("• Varianza acumulada ≥ 80%:", n_var80, "componentes\n")
cat("• Varianza acumulada ≥ 90%:", n_var90, "componentes\n")

# 4.3 Gráfico Scree plot (autovalores) y Varianza acumulada
scree_data <- data.frame(
  PC = 1:length(eigenvalues),
  Eigenvalue = eigenvalues,
  Varianza = var_exp_porcent
)
# Gráfico de autovalores (Scree)
p_scree <- ggplot(scree_data[1:10, ], aes(x = PC, y = Eigenvalue)) +
  geom_line(col="#2196F3", size=1) +
  geom_point(col="#2196F3", size=3) +
  geom_hline(yintercept = 1, linetype="dashed", col="red") +
  labs(title="Scree plot - Autovalores",
       x="Componente Principal", y="Autovalor") +
  annotate("text", x = max(1:10), y = 1.1, label="Eigenvalue = 1", col="red", hjust=1) +
  theme_minimal()

# Gráfico de varianza explicada acumulada
p_scree_acum <- ggplot(scree_data[1:10, ], aes(x = PC, y = cumsum(Varianza))) +
  geom_line(col="#4CAF50", size=1) +
  geom_point(col="#4CAF50", size=3) +
  geom_hline(yintercept = 80, linetype="dashed", col="orange") +
  geom_hline(yintercept = 90, linetype="dashed", col="red") +
  labs(title="Varianza explicada acumulada",
       x="Componente Principal", y="Varianza acumulada (%)") +
  theme_minimal()
# Mostrar los gráficos
print(p_scree) + print(p_scree_acum)


# ============================================================================
# 5. INTERPRETACIÓN DE COMPONENTES
# ============================================================================

# 5.1 Loadings (pesos de variables en cada componente)
loadings <- pca_result$rotation  # matriz de loadings: variables x componentes
cat("\n========== LOADINGS (Pesos de variables en PCs) ==========\n")
# Mostrar loadings de los primeros 5 componentes (o menos si p<5)
num_pc_mostrar <- min(3, ncol(loadings))
cat("Loadings de las primeras", num_pc_mostrar, "componentes:\n")
print(round(loadings[, 1:num_pc_mostrar], 3))

# Interpretación de PC1: identificar variables con mayor contribución (valor absoluto más alto)
loadings_pc1 <- data.frame(Variable = rownames(loadings),
                           Loading = loadings[,1]) %>% 
  mutate(AbsLoading = abs(Loading)) %>% 
  arrange(desc(AbsLoading))
cat("\nPrincipales contribuyentes a PC1:\n")
print(head(loadings_pc1, 5), row.names = FALSE)
cat("\nInterpretación PC1:\n")
if(all(head(loadings_pc1, 5)$Loading > 0) || all(head(loadings_pc1, 5)$Loading < 0)) {
  cat("PC1 parece ser un factor común donde las variables más influyentes apuntan en la misma dirección.\n")
  cat("Un valor alto de PC1 indicaría que la mayoría de estas variables están por encima de su media (y viceversa).\n")
} else {
  cat("PC1 captura una oposición entre variables: algunas con cargas positivas y otras negativas.\n")
  cat("Un valor alto de PC1 indicaría altas en variables con carga positiva y bajas en las de carga negativa (y viceversa).\n")
}

# Interpretación de PC2 de forma similar (opcional, aquí solo mostramos principales cargas)
loadings_pc2 <- data.frame(Variable = rownames(loadings),
                           Loading = loadings[,2]) %>% 
  mutate(AbsLoading = abs(Loading)) %>% 
  arrange(desc(AbsLoading))
cat("\nPrincipales contribuyentes a PC2:\n")
print(head(loadings_pc2, 5), row.names = FALSE)
cat("\nInterpretación PC2:\n")
if(all(head(loadings_pc2, 5)$Loading > 0) || all(head(loadings_pc2, 5)$Loading < 0)) {
  cat("Las variables de mayor peso en PC2 también apuntan en una dirección similar,\n")
  cat("lo que sugiere otro factor común (distinto de PC1) que las agrupa.\n")
} else {
  cat("PC2 refleja contraste entre variables: influye positivamente en unas y negativamente en otras,\n")
  cat("indicando que estas variables se comportan de manera inversa en este componente.\n")
}

# Visualización de los loadings en forma de mapa de calor (para los primeros 5 PCs)
loadings_df <- as.data.frame(loadings[, 1:num_pc_mostrar])
loadings_df$Variable <- rownames(loadings_df)
loadings_long <- loadings_df %>% pivot_longer(cols = -Variable, names_to = "PC", values_to = "Valor")
p_loadings_heatmap <- ggplot(loadings_long, aes(x = PC, y = Variable, fill = Valor)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#D32F2F", mid = "white", high = "#1976D2", midpoint = 0) +
  geom_text(aes(label = round(Valor, 2)), size = 3) +
  labs(title = "Cargas de variables en componentes (PC1-PC3)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_loadings_heatmap)

# 5.2 Scores: coordenadas de cada observación en PCs
scores <- as.data.frame(pca_result$x)
if(exists("fecha_completa_char")) {
  scores$Fecha <- fecha_completa_char
}

# Identificar observaciones con valores extremos en PC1 (por ejemplo, las 10 mayores y 10 menores)
cat("\n========== PUNTUACIONES (Scores) DE PCA ==========\n")
if("Fecha" %in% names(scores)) {
  # Ordenar por PC1
  top_pc1 <- scores %>% arrange(desc(PC1)) %>% select(Fecha, PC1) %>% head(10)
  bottom_pc1 <- scores %>% arrange(PC1) %>% select(Fecha, PC1) %>% head(10)
  cat("Top 10 observaciones (fechas) con PC1 más alto:\n"); print(top_pc1, row.names = FALSE)
  cat("\nTop 10 observaciones (fechas) con PC1 más bajo:\n"); print(bottom_pc1, row.names = FALSE)
} else {
  # Si no hay una columna de identificación, simplemente indicamos índices de fila
  top_pc1 <- scores %>% mutate(Index = row_number()) %>% arrange(desc(PC1)) %>% select(Index, PC1) %>% head(10)
  bottom_pc1 <- scores %>% mutate(Index = row_number()) %>% arrange(PC1) %>% select(Index, PC1) %>% head(10)
  cat("Top 10 observaciones (filas) con PC1 más alto:\n"); print(top_pc1, row.names = FALSE)
  cat("\nTop 10 observaciones (filas) con PC1 más bajo:\n"); print(bottom_pc1, row.names = FALSE)
}

# ============================================================================
# 6. VISUALIZACIÓN DE RESULTADOS PCA
# ============================================================================

# 6.1 Biplot de individuos y variables en espacio de los dos primeros componentes
p_biplot <- fviz_pca_biplot(pca_result,
                            geom.ind = "point", # mostrar observaciones como puntos
                            col.ind = "gray50",  # color de puntos
                            label = "var",      # etiquetar solo las variables
                            col.var = "blue",
                            repel = TRUE,
                            title = "Biplot PCA - PC1 vs PC2")
print(p_biplot)

# 6.2 Scatter plot de observaciones en PC1-PC2, destacando extremos
# Identificar observaciones extremas en el plano PC1-PC2 (percentil 90 de distancia)
scores$dist_pc1pc2 <- sqrt(scores$PC1^2 + scores$PC2^2)
umbral <- quantile(scores$dist_pc1pc2, 0.90)
scores_extremos <- scores %>% 
  filter(dist_pc1pc2 >= umbral)
p_scatter_ext <- ggplot(scores, aes(x = PC1, y = PC2)) +
  geom_point(color="gray60", alpha=0.6) +
  geom_point(data = scores_extremos, aes(x = PC1, y = PC2), color="red", size=2) +
  geom_text_repel(data = scores_extremos, aes(label = ifelse(exists("Fecha") , Fecha, rownames(scores_extremos))),
                  size = 3) +
  labs(title="Puntuaciones PC1 vs PC2 (destacando observaciones extremas)") +
  theme_minimal()
print(p_scatter_ext)

# 6.3 Círculo de correlaciones (variables en el plano PC1-PC2)
p_var <- fviz_pca_var(pca_result,
                      col.var = "contrib", # color según contribución
                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                      repel = TRUE,
                      title = "Círculo de correlaciones (PC1 vs PC2)",
                      legend.title = "Contribución")
print(p_var)


print(p_var) + print(p_biplot)

# ============================================================================
# 7. ANÁLISIS ADICIONAL: ÍNDICE COMPUESTO BASADO EN PCA (PC1)
# ============================================================================

# 7.1 Crear un índice compuesto usando PC1 (por ejemplo, un índice agregado de rendimiento)
indice_pc1 <- scores$PC1
# Escalar el índice a rango 0-100 para interpretabilidad
indice_pc1_esc <- if(!is.null(indice_pc1)) scales::rescale(indice_pc1, to = c(0, 100)) else NULL
if(!is.null(indice_pc1_esc)) {
  cat("\nSe creó un índice compuesto basado en PC1, escalado de 0 a 100.\n")
}

# 7.2 Validación: correlación del índice PC1 con las variables originales
if(!is.null(indice_pc1)) {
  correlaciones_indice <- cor(matriz_pca_sin_cta, indice_pc1)
  cor_con_indice <- data.frame(Variable = rownames(correlaciones_indice),
                               Correlacion = correlaciones_indice[,1]) %>% 
    arrange(desc(abs(Correlacion)))
  cat("\nCorrelación de cada variable original con el índice (PC1):\n")
  print(cor_con_indice, row.names = FALSE)
  # Gráfico de correlación de variables con el índice PC1
  p_corr_index <- ggplot(cor_con_indice, aes(x = reorder(Variable, Correlacion), y = Correlacion)) +
    geom_col(aes(fill = Correlacion > 0), width = 0.6) +
    coord_flip() +
    scale_fill_manual(values = c("red", "steelblue"), guide = "none") +
    labs(title = "Correlación de variables con Índice (PC1)", x = NULL, y = "Correlación") +
    theme_minimal()
  print(p_corr_index)
}


print(p_corr_index)
# ============================================================================
# 8. (OPCIONAL) EXPORTAR RESULTADOS
# ============================================================================

# Guardar los scores de PCA y el índice en CSV (opcional, puede omitirse si no es requerido)
output_scores <- scores
if(exists("fecha_completa_char")) {
  # Reordenar para tener Fecha primero
  output_scores <- output_scores %>% relocate(Fecha)
}
# Añadir índice escalado si existe
if(exists("indice_pc1_esc") && !is.null(indice_pc1_esc)) {
  output_scores$Indice_PC1_esc <- indice_pc1_esc
}
write.csv(output_scores, "PCA_scores_con_indice.csv", row.names = FALSE)
cat("\n✓ Resultados de PCA (scores e índice) exportados a 'PCA_scores_con_indice.csv'\n")

# Guardar también la matriz de loadings
loadings_out <- data.frame(Variable = rownames(loadings), loadings[, 1:num_pc_mostrar])
write.csv(loadings_out, "PCA_loadings.csv", row.names = FALSE)
cat("✓ Loadings de PCA exportados a 'PCA_loadings.csv'\n")


# ============================================================================
# 9. RESUMEN Y CONCLUSIONES
# ============================================================================

cat("\n============================================================\n")
cat("                    RESUMEN DEL ANÁLISIS PCA                  \n")
cat("============================================================\n")
cat("Datos originales: ", nrow(datos_raw), " observaciones, ", ncol(datos_raw)-1, " variables numéricas (aprox)\n")
cat("Tras limpieza: ", n, " observaciones completas, ", p, " variables usadas en PCA\n")
cat("KMO global: ", round(kmo_result$MSA, 3), ifelse(kmo_result$MSA >= 0.7, " (Adecuado)", " (Bajo)"), "\n")
cat("Bartlett p-value: ", format(bartlett_result$p.value, scientific=TRUE), " → ", 
    ifelse(bartlett_result$p.value < 0.05, "Correlaciones significativas entre variables (PCA válido)", "Sin correlaciones significativas (PCA no recomendable)"), "\n")
cat("Varianza explicada por PC1: ", round(var_exp_porcent[1],1), "%\n")
cat("Varianza explicada acumulada PC1-PC2: ", round(var_exp_acum_porcent[2],1), "%\n")
cat("Componentes retenidos (λ>1): ", n_kaiser, "\n")
cat("Componentes para ≥80% var: ", n_var80, "\n")
cat("PC1 representa un factor general que influye en la mayoría de las variables.\n")
cat("PC2 representa un factor secundario (ortogonal a PC1) con contraste entre grupos de variables.\n")
cat("El análisis PCA permitió reducir ", p, " variables originales a ", n_var80, 
    " componentes manteniendo ~", round(var_exp_acum_porcent[n_var80],1), "% de la información.\n")
cat("============================================================\n")

