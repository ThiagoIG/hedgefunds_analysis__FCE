#1 CARGA DE LIBRERÍAS
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gt)


#2 SETEAR DIRECTORIO DE TRABAJO DESDE CONSOLA (correr en CONSOLA)
setwd("C:/Users/thiag/Documents/analisis_hedgefunds")

#3 CARGAR CSV
hedge_fund <- read_csv("data/raw/edhec-hedgefundindices.csv")

#4 RENOMBRAR PRIMERA COLUMNA A 'Fecha' Y CONVERTIR A TIPO FECHA
colnames(hedge_fund)[1] <- "Fecha"
hedge_fund$Fecha <- as.Date(hedge_fund$Fecha, format = "%d/%m/%Y")



#5 FORMATO LARGO PARA VISUALIZACIONES
hedge_fund_long <- hedge_fund %>%
  pivot_longer(cols = -Fecha,
               names_to = "Estrategia",
               values_to = "Rendimiento")

#6 EXPLORACIÓN INICIAL (EDA)
str(hedge_fund)       # tipo de columnas
dim(hedge_fund)       # dimensiones
head(hedge_fund)      # primeras observaciones
summary(hedge_fund)   # resumen estadístico


#7 TABLA RESUMEN: MEDIA, MÍNIMO Y MÁXIMO POR ESTRATEGIA
eda_resumen <- hedge_fund_long %>%
  group_by(Estrategia) %>%
  summarise(
    Media = mean(Rendimiento, na.rm = TRUE),
    Maximo = max(Rendimiento, na.rm = TRUE),
    Minimo = min(Rendimiento, na.rm = TRUE)
  ) %>%
  arrange(desc(Media)) 


# GENERAR TABLA VISUAL CON gt
tabla_gt <- eda_resumen %>%
  gt() %>%
  tab_header(
    title = "Resumen Estadístico por Estrategia"
  ) %>%
  fmt_number(
    columns = c(Media, Maximo, Minimo),
    decimals = 2
  )

tabla_gt

# GUARDAR TABLA HTML EN outputs/tables
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
gtsave(tabla_gt, "outputs/tables/eda_resumen_visual.html")


# AGREGAR COLORES DE RIESGO A CADA ESTRATEGIA
riesgo_colores <- tibble::tibble(
  Estrategia = c("Convertible Arbitrage", "CTA Global", "Distressed Securities",
                 "Emerging Markets", "Equity Market Neutral", "Event Driven",
                 "Fixed Income Arbitrage", "Global Macro", "Long/Short Equity",
                 "Merger Arbitrage", "Relative Value", "Short Selling", "Funds Of Funds"),
  Riesgo = c("Medio", "Alto", "Alto", "Alto", "Bajo", "Medio", "Bajo", "Alto",
             "Medio", "Medio", "Bajo", "Alto", "Medio")
)
# Mapear riesgo a colores
color_map <- c(
  "Bajo" = "forestgreen",
  "Medio" = "goldenrod",
  "Alto" = "firebrick"
)

# UNIR INFO DE RIESGO
hedge_fund_long <- hedge_fund_long %>%
  left_join(riesgo_colores, by = "Estrategia")

# GRAFICO BOXPLOT CON COLORES POR RIESGO
p <- ggplot(hedge_fund_long, aes(x = Estrategia, y = Rendimiento, fill = Riesgo)) +
  geom_boxplot(alpha = 0.8, outlier.color = "black") +
  scale_fill_manual(values = color_map) +
  labs(title = "Boxplot de Rendimientos Mensuales por Estrategia",
       x = "Estrategia",
       y = "Rendimiento mensual (%)",
       fill = "Riesgo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

#9 MOSTRAR Y GUARDAR EL GRÁFICO
print(p)


# GUARDAR IMAGEN
ggsave("outputs/figures/boxplot_riesgo_colores.png", plot = p, width = 11, height = 6, dpi = 300)

#11 GUARDAR CSV "LIMPIO"
write_csv(hedge_fund_long, "data/clean/hedge_fund_long.csv")
