# Test de normalidad (Shapiro-Wilk) y Q-Q plots por grupo de riesgo

# 1. Cargar librerías
library(readr)
library(dplyr)
library(ggplot2)
library(rstatix)
library(ggpubr)
library(car)



# 2. Cargar datos
hedge_fund_winsorized <- read_csv ("data/clean/hedge_fund_long_winzo.csv")

# 3. Agregar columna de riesgo por estrategia
riesgo_colores <- tibble::tibble(
  Estrategia = c("Convertible Arbitrage", "CTA Global", "Distressed Securities",
                 "Emerging Markets", "Equity Market Neutral", "Event Driven",
                 "Fixed Income Arbitrage", "Global Macro", "Long/Short Equity",
                 "Merger Arbitrage", "Relative Value", "Short Selling", "Funds Of Funds"),
  Riesgo = c("Medio", "Alto", "Alto", "Alto", "Bajo", "Medio", "Bajo", "Alto",
             "Medio", "Medio", "Bajo", "Alto", "Medio")
)

hedge_fund_winsorized <- hedge_fund_winsorized %>%
  left_join(riesgo_colores, by = "Estrategia")

# 4. Test de Shapiro-Wilk por grupo de riesgo
shapiro_result <- hedge_fund_winsorized %>%
  group_by(Riesgo) %>%
  summarise(
    statistic = shapiro.test(Rendimiento_winsorized)$statistic,
    p_value = shapiro.test(Rendimiento_winsorized)$p.value
  )

print(shapiro_result)

# 5. Visualización Q-Q plots por grupo de riesgo
qq_plot <- ggplot(hedge_fund_winsorized, aes(sample = Rendimiento_winsorized)) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  facet_wrap(~ Riesgo) +
  labs(
    title = "Q-Q Plots por Riesgo (Rendimiento Winsorizado)",
    x = "Cuantiles Teóricos", y = "Cuantiles Observados"
  ) +
  theme_minimal()

# Mostrar plot
print(qq_plot)

# 6. Guardar gráfico
ggsave("outputs/figures/qqplot_shapiro_winsor.png", plot = qq_plot, width = 10, height = 6)


# 8. Test de Levene (homogeneidad de varianzas)
levene_result <- leveneTest(Rendimiento_winsorized ~ Riesgo, data = hedge_fund_winsorized)
print(levene_result)

# 7. Welch ANOVA: Rendimiento_winsorizado ~ Riesgo
welch_result <- oneway.test(Rendimiento_winsorized ~ Riesgo, 
                            data = hedge_fund_winsorized,
                            var.equal = FALSE)  # Welch ANOVA

# 8. Mostrar resultados
print(welch_result)

#10. Kruskal test
kruskal.test(Rendimiento_winsorized ~ Riesgo, data = hedge_fund_winsorized)
