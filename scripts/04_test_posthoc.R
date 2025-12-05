# Post hoc: Games-Howell y Dunn test tras ANOVA y Kruskal

# 1. Cargar librerías
library(readr)
library(dplyr)
library(ggplot2)
library(rstatix)
library(ggpubr)
library(car)

# 2. Cargar el CSV limpio
hedge_fund_winsorized <- read_csv("data/clean/hedge_fund_long_winzo.csv")

# 3. Agregar columna de riesgo
riesgo_colores <- tibble::tibble(
  Estrategia = c("Convertible Arbitrage", "CTA Global", "Distressed Securities",
                 "Emerging Markets", "Equity Market Neutral", "Event Driven",
                 "Fixed Income Arbitrage", "Global Macro", "Long/Short Equity",
                 "Merger Arbitrage", "Relative Value", "Short Selling", "Funds Of Funds"),
  Riesgo = c("Medio", "Alto", "Alto", "Alto", "Bajo", "Medio", "Bajo", "Alto",
             "Medio", "Medio", "Bajo", "Alto", "Medio")
)

# Unir con la base
hedge_fund_winsorized <- hedge_fund_winsorized %>%
  left_join(riesgo_colores, by = "Estrategia")

# 4. Convertir Riesgo a factor
hedge_fund_winsorized$Riesgo <- as.factor(hedge_fund_winsorized$Riesgo)

# 5. Test de Games–Howell (post hoc Welch ANOVA)
games_result <- hedge_fund_winsorized %>%
  games_howell_test(Rendimiento_winsorized ~ Riesgo)

# 6. Ver resultado
print(games_result)


# --------------------------
# POST HOC: Dunn test (tras Kruskal)
# --------------------------
dunn_result <- hedge_fund_winsorized %>%
  dunn_test(Rendimiento_winsorized ~ Riesgo, p.adjust.method = "bonferroni")

cat("\n\n== POST HOC: Dunn test (Kruskal) - Bonferroni ==\n")
print(dunn_result)



