#Winsorizacion de dataset

# 1. Cargar librerías
library(readr)
library(dplyr)
library(ggplot2)
library(scales)

# 2. Cargar datos
hedge_fund_long <- read_csv("data/clean/hedge_fund_long.csv")

# 3. Aplicar Z-score original para detectar outliers
hedge_fund_long <- hedge_fund_long %>%
  group_by(Estrategia) %>%
  mutate(
    zscore_original = scale(Rendimiento)[, 1],
    outlier_z_original = abs(zscore_original) > 3
  ) %>%
  ungroup()

# 4. Gráfico de outliers Z-score original
p1 <- ggplot(hedge_fund_long, aes(x = Fecha, y = Rendimiento, color = outlier_z_original)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ Estrategia, scales = "free_y") +
  labs(title = "Outliers por Estrategia - Z-Score Original",
       x = "Fecha", y = "Rendimiento", color = "Outlier") +
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red")) +
  theme_minimal()

p1

ggsave("outputs/figures/zscore_outliers_original.png", plot = p1, width = 12, height = 8)

# 5. Winsorización por estrategia (2% y 98%)
hedge_fund_winsor <- hedge_fund_long %>%
  group_by(Estrategia) %>%
  mutate(
    p2 = quantile(Rendimiento, 0.02, na.rm = TRUE),
    p98 = quantile(Rendimiento, 0.98, na.rm = TRUE),
    Rendimiento_winsorized = pmin(pmax(Rendimiento, p2), p98)
  ) %>%
  ungroup()

# 6. Aplicar Z-score luego de winsorización
hedge_fund_winsor <- hedge_fund_winsor %>%
  group_by(Estrategia) %>%
  mutate(
    zscore_winsor = scale(Rendimiento_winsorized)[, 1],
    outlier_z_winsor = abs(zscore_winsor) > 3
  ) %>%
  ungroup()

# 7. Visualizar Z-score luego de winsorización
p2 <- ggplot(hedge_fund_winsor, aes(x = Fecha, y = Rendimiento_winsorized, color = outlier_z_winsor)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ Estrategia, scales = "free_y") +
  labs(title = "Outliers por Estrategia - Z-Score Winsorizado",
       x = "Fecha", y = "Rendimiento (Winsor)", color = "Outlier") +
  scale_color_manual(values = c("FALSE" = "darkgreen", "TRUE" = "orange")) +
  theme_minimal()
p2

ggsave("outputs/figures/zscore_outliers_winsorized.png", plot = p2, width = 12, height = 8)

# Contar q de Outliers
cat("Outliers originales (Z > 3):", sum(hedge_fund_winsor$outlier_z_original, na.rm = TRUE), "\n")
cat("Outliers tras winsorización (Z > 3):", sum(hedge_fund_winsor$outlier_z_winsor, na.rm = TRUE), "\n")

# 9. REEMPLAZAR los outliers en "Fixed Income Arbitrage" por el percentil 2%
p2_fia <- hedge_fund_winsor %>%
  filter(Estrategia == "Fixed Income Arbitrage") %>%
  summarise(p2 = quantile(Rendimiento_winsorized, 0.02, na.rm = TRUE)) %>%
  pull(p2)

hedge_fund_clean <- hedge_fund_winsor %>%
  mutate(
    Rendimiento_winsorized = if_else(
      Estrategia == "Fixed Income Arbitrage" & outlier_z_winsor,
      p2_fia,
      Rendimiento_winsorized
    ),
    outlier_z_winsor = if_else(
      Estrategia == "Fixed Income Arbitrage" & outlier_z_winsor,
      FALSE,  # Ya no es outlier
      outlier_z_winsor
    )
  )


# 10. Guardar versión limpia final
write_csv(hedge_fund_clean, "data/clean/hedge_fund_long_winzo.csv")



