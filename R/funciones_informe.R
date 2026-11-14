# Funciones de apoyo para los informes de titulación (se cargan con source())

library(tidyverse)

# Porcentaje con coma decimal (formato español)
pct <- function(x, accuracy = 0.1) scales::percent(x, accuracy = accuracy, decimal.mark = ",")

# Resumen de titulación a tiempo, por la variable de agrupación que se indique
resumen_titulacion <- function(datos, grupo) {
  datos |>
    summarise(n = n(),
              a_tiempo = sum(titulo_a_tiempo),
              pct_a_tiempo = mean(titulo_a_tiempo),
              .by = {{ grupo }}) |>
    arrange({{ grupo }})
}

# Gráfico: % que se titula a tiempo según tramo de horas semanales, por beca
grafico_horas <- function(datos) {
  datos |>
    mutate(tramo = cut(horas_investigacion_sem,
                       breaks = c(0, 15, 25, 35, Inf),
                       labels = c("Menos de 15", "15 a 24", "25 a 34", "35 o más"),
                       right = FALSE)) |>
    summarise(n = n(), pct = mean(titulo_a_tiempo), .by = c(tramo, beca)) |>
    ggplot(aes(tramo, pct, fill = beca)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_y_continuous(labels = \(x) scales::percent(x, decimal.mark = ","), limits = c(0, 1)) +
    scale_fill_manual(values = c("Sin beca" = "#7A8CA3", "Con beca" = "#065A82")) +
    labs(x = "Horas semanales de investigación",
         y = "Se tituló a tiempo", fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top")
}

# Commit de Git con que se generó el informe (o un aviso si no hay repositorio)
commit_actual <- function() {
  out <- tryCatch(system("git rev-parse --short HEAD", intern = TRUE, ignore.stderr = TRUE),
                  error = function(e) character(0), warning = function(w) character(0))
  if (length(out) == 0) "sin repositorio Git" else out
}
