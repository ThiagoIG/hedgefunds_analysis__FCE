# CARGA DE LIBRERÍAS
library(gt)
library(dplyr)

# CREAR Y TRANSFORMAR DATAFRAME
df <- tibble::tibble(
  Estrategia = c("Convertible Arbitrage", "CTA Global", "Distressed Securities",
                 "Emerging Markets", "Equity Market Neutral", "Event Driven",
                 "Fixed Income Arbitrage", "Global Macro", "Long/Short Equity",
                 "Merger Arbitrage", "Relative Value", "Short Selling", "Fund of Funds"),
  Definicion = c("Compra bonos convertibles y vende acciones subyacentes.",
                 "Sigue tendencias de mercado usando derivados globales.",
                 "Invierte en deuda de empresas en dificultades.",
                 "Invierte en países emergentes, alto riesgo/retorno.",
                 "Neutraliza exposición al mercado tomando posiciones opuestas.",
                 "Apuesta a eventos como fusiones, bancarrotas o spin-offs.",
                 "Explota ineficiencias en renta fija (bonos, swaps, etc).",
                 "Apuesta macroeconómica a tasas, monedas, commodities.",
                 "Posiciones largas y cortas en acciones.",
                 "Captura ganancias en procesos de fusión/adquisición.",
                 "Aprovecha diferencias entre instrumentos similares.",
                 "Apuesta a que un activo va a bajar de precio.",
                 "Diversifica a través de múltiples hedge funds."),
  `Expectativa del Mercado` = c("Parcial", "Direccional", "Bajista", "Direccional", "Neutral", "Parcial",
                                "Neutral", "Direccional", "Parcial", "Parcial", "Neutral", "Bajista", "Direccional"),
  Riesgo = c("Medio", "Alto", "Alto", "Alto", "Bajo", "Medio", "Bajo", "Alto",
             "Medio", "Medio", "Bajo", "Alto", "Medio")
) %>%
  # Reemplazar texto por íconos
  mutate(
    Riesgo = case_when(
      Riesgo == "Alto" ~ "🔴",
      Riesgo == "Medio" ~ "🟡",
      Riesgo == "Bajo" ~ "🟢"
    ),
    `Expectativa del Mercado` = case_when(
      `Expectativa del Mercado` == "Direccional" ~ "⤭ Direccional",
      `Expectativa del Mercado` == "Neutral" ~ "＝ Neutral",
      `Expectativa del Mercado` == "Bajista" ~ "📉 Bajista",
      `Expectativa del Mercado` == "Parcial" ~ "📈＝📉 Parcial"
    )
  ) %>%
  # Ordenar por Expectativa del Mercado
  mutate(`Expectativa del Mercado` = factor(
    `Expectativa del Mercado`,
    levels = c("＝ Neutral", "📉 Bajista", "⤭ Direccional", "📈＝📉 Parcial")
  )) %>%
  arrange(`Expectativa del Mercado`)

# CREAR TABLA gt
tabla <- df %>%
  gt() %>%
  tab_header(
    title = "Estrategias de Inversión y Nivel de Riesgo"
  ) %>%
  tab_source_note(
    source_note = md("📈＝📉 *Parcial* combina elementos de estrategias **Bajistas** y **Alcistas**, diferenciándose de las estrategias puramente Direccionales o Neutrales.")
  )

# MOSTRAR
tabla

# GUARDAR COMO HTML EN outputs/tables
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
gtsave(tabla, "outputs/tables/estrategias_riesgo.html")
