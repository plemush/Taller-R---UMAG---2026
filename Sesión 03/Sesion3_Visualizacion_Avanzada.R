# ==============================================================================
# Práctica — Sesión 3: Visualización avanzada
# Análisis de Datos en R · Nivel Básico-Intermedio · Duración: 60 minutos
# ==============================================================================
#
# CÓMO TRABAJAR ESTE ARCHIVO
#
# Este archivo es TU SCRIPT DE TRABAJO: lo vas a completar directamente aquí.
# En el paso 0.4 lo guardas como R/sesion3.R dentro de tu proyecto.
#
#   * Los espacios marcados con ___ son lo que debes completar.
#   * Ejecuta el código línea por línea o por bloques con Ctrl + Enter
#     (Cmd + Enter en Mac). No uses "Source" hasta haber completado los ___,
#     porque R no entiende ___ y mostrará un error.
#   * Las líneas que empiezan con "#" son instrucciones o comentarios: no se ejecutan.
#   * Las líneas marcadas "TERMINAL" NO se escriben en la consola de R: se escriben
#     (sin el "#") en la pestaña Terminal de RStudio.
#   * Escribe tus respuestas a las preguntas donde dice "Mi respuesta:".
#   * Donde dice "Escribe tu código aquí" no hay esqueleto: escribe tú la solución.
#   * Si te trabas más de 3 minutos en un ejercicio, pasa al siguiente y vuelve
#     después: el objetivo es que practiques el flujo completo, no que todo salga perfecto.
#   * Guarda cada gráfico en un objeto (p_tend, p_rank...) y míralo antes de
#     pasar al siguiente: un gráfico se afina probándolo, no pensándolo.
#   * Tip RStudio: abre el panel "Outline" (Ctrl + Shift + O) para navegar entre bloques.
#   * Si ves caracteres raros en las tildes: File > Reopen with Encoding > UTF-8.
#
# BLOQUES DE LA PRÁCTICA
#
#   Bloque 0 · Preparación .................................... 5 min
#   Bloque 1 · ggplot2 avanzado ............................... 20 min
#   Bloque 2 · Combinar y exportar gráficos ................... 15 min
#   Bloque 3 · Visualizaciones interactivas ................... 15 min
#   Cierre y dudas ............................................ 5 min
#
# DATOS (todos simulados, no corresponden a personas reales)
#
#   produccion_anual.csv  (NUEVO) 66 filas: la producción de cada programa entre
#                         2015 y 2025 (n_articulos, n_citas y financiamiento_mm,
#                         en millones de pesos).
#   encuesta_limpia.csv   la encuesta de la sesión 2 (150 filas).
#   programas.csv         6 programas con su área.
#
# REQUISITOS
#
#   R >= 4.1, RStudio, el proyecto "curso-r-intermedio" (con su repositorio de Git)
#   y los paquetes tidyverse, here, scales, patchwork, ggrepel, plotly y DT.
# ==============================================================================


# Bloque 0 · Preparación (5 min) -----------------------------------------------


# 0.1  Abre el proyecto `curso-r-intermedio` en RStudio. En la terminal, `git
#      status` debería indicar que no hay cambios pendientes.


# 0.2  Copia `produccion_anual.csv` a `data/raw/` (los otros dos archivos ya
#      deberían estar ahí).


# 0.3  Instala los paquetes que te falten (solo una vez), en la consola:
install.packages(c("scales", "patchwork", "ggrepel", "plotly", "DT"))


# 0.4  Crea un script nuevo, guárdalo como `R/sesion3.R` y escribe al comienzo:
library(tidyverse)
library(here)
library(scales)
library(patchwork)
library(ggrepel)
library(plotly)
library(DT)

encuesta   <- read_csv(here("data", "raw", "encuesta_limpia.csv"))
programas  <- read_csv(here("data", "raw", "programas.csv"))
produccion <- read_csv(here("data", "raw", "produccion_anual.csv")) |>
  left_join(programas, by = "programa_id")

# Paletas con nombres: cada categoría conserva su color en todos los gráficos
pal_area  <- c("Ciencias Naturales" = "#0072B2", "Ciencias Sociales" = "#E69F00", "Ingeniería" = "#009E73")
pal_nivel <- c("Doctorado" = "#0072B2", "Magíster" = "#E69F00")

glimpse(produccion)

# Regla de trabajo para toda la práctica: guarda cada gráfico en un objeto
# (`p_tend`, `p_rank`…) y míralo antes de pasar al siguiente. Un gráfico se
# afina probándolo, no pensándolo.


# Bloque 1 · ggplot2 avanzado (20 min) -----------------------------------------


# 1.1  Escalas y color. Haz el gráfico de líneas de los artículos por área y
#      año, con años enteros en el eje y la paleta accesible `pal_area`.
por_area <- produccion |>
  summarise(n_articulos = sum(n_articulos), .by = c(anio, area))

p_tend <- ggplot(por_area, aes(___, ___, color = area)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = seq(2015, 2025, by = ___)) +
  scale_color_manual(values = ___) +
  labs(title = "La producción casi se duplicó",
       x = NULL, y = "Artículos", color = NULL)
p_tend

# Pregunta: ¿qué pasaría si escribieras `scale_fill_manual()` en vez de
#           `scale_color_manual()`? Pruébalo.
# Mi respuesta:


# 1.2  Ejes con `scales`. Haz un gráfico de barras del financiamiento total por
#      año, con el eje formateado y las barras partiendo exactamente en cero.
fin_anual <- produccion |>
  summarise(financiamiento = sum(financiamiento_mm), .by = anio)

ggplot(fin_anual, aes(anio, financiamiento)) +
  geom_col(fill = "#1C7293") +
  scale_x_continuous(breaks = 2015:2025) +
  scale_y_continuous(
    labels = label_number(suffix = " M", big.mark = ___, decimal.mark = ","),
    expand = expansion(mult = c(___, 0.05))
  ) +
  labs(x = NULL, y = "Financiamiento (millones de CLP)")

# Pregunta: ¿por qué en un gráfico de barras el eje debe partir en cero?
# Mi respuesta:


# 1.3  Ordenar con `forcats`. Haz un ranking de programas según el total de
#      artículos, con barras horizontales ordenadas de mayor a menor.
total_prog <- produccion |>
  summarise(n = sum(n_articulos), .by = programa)

p_rank <- ggplot(total_prog, aes(n, fct_reorder(programa, ___))) +
  geom_col(fill = "#1C7293") +
  labs(x = "Artículos (2015–2025)", y = NULL)
p_rank

# Pregunta: ¿qué programa publica más? Quita `fct_reorder()` y compara: ¿cuál
#           de las dos versiones te lo dice más rápido?
# Mi respuesta:


# 1.4  Anotaciones. Haz la serie del total de artículos por año y marca la
#      pandemia (2020–2021) con un rectángulo sombreado y una etiqueta.
total_anual <- produccion |>
  summarise(n = sum(n_articulos), .by = anio)

ggplot(total_anual, aes(anio, n)) +
  geom_line(color = "#21295C", linewidth = 1) +
  annotate("rect", xmin = ___, xmax = ___, ymin = -Inf, ymax = Inf, alpha = 0.12) +
  annotate("label", x = 2020.5, y = 60, label = "___") +
  geom_hline(yintercept = 80, linetype = "dashed") +
  scale_x_continuous(breaks = seq(2015, 2025, by = 2)) +
  labs(title = "Producción total del posgrado", x = NULL, y = "Artículos")

# Ahora agrega tú una anotación de texto, "Meta: 80 artículos", sobre la línea
# punteada (usa `annotate("text", ...)`).


# 1.5  Etiquetas sin choques. Dibuja cada programa como un punto (horas de
#      investigación por semana contra publicaciones, en promedio) y etiquétalo
#      con `ggrepel`.
prom_prog <- encuesta |>
  left_join(programas, by = "programa_id") |>
  summarise(horas = mean(horas_investigacion_sem, na.rm = TRUE),
            publicaciones = mean(n_publicaciones, na.rm = TRUE),
            n = n(), .by = c(programa, area))

p_disp <- ggplot(prom_prog, aes(horas, publicaciones, color = area)) +
  geom_point(aes(size = n)) +
  geom_text_repel(aes(label = ___), size = 3, seed = 1, box.padding = 0.6) +
  scale_color_manual(values = pal_area, guide = "none") +
  scale_size(range = c(3, 8), guide = "none") +
  scale_x_continuous(expand = expansion(mult = 0.25)) +
  scale_y_continuous(expand = expansion(mult = 0.2)) +
  labs(x = "Horas de investigación por semana", y = "Publicaciones (promedio)")
p_disp

# Pregunta: cambia `geom_text_repel()` por `geom_text()`: ¿qué le pasa a las
#           etiquetas?
# Mi respuesta:


# 1.6  Facetas con escalas libres. Dibuja la serie de artículos de cada
#      programa en su propio panel, con escalas libres en y y los nombres
#      largos partidos en dos líneas.
ggplot(produccion, aes(anio, n_articulos)) +
  geom_line(color = "#1C7293") +
  facet_wrap(vars(programa), scales = "___", labeller = label_wrap_gen(___)) +
  scale_x_continuous(breaks = c(2015, 2025)) +
  labs(x = NULL, y = "Artículos")

# Pregunta: ¿qué ventaja tiene `scales = "free_y"` y qué se pierde? Si vas a
#           comparar programas entre sí, ¿qué escala conviene?
# Mi respuesta:


# 1.7  Un tema propio. Escribe una función que devuelva tu tema y actívala para
#      todo el script.
theme_posgrado <- function(base_size = ___) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "___", color = "#21295C"),
      panel.grid.minor = element_blank(),
      legend.position = "___"
    )
}

theme_set(theme_posgrado())
p_tend

# Pregunta: ¿qué ventaja tiene que el estilo viva en una función y no repetido
#           en cada gráfico?
# Mi respuesta:


# 1.8  Guardar tu trabajo. Guarda el script (Ctrl + S). En la TERMINAL:
#   git add R/sesion3.R
#   git commit -m "Mejora los graficos con escalas, orden, anotaciones y tema"


# Bloque 2 · Combinar y exportar (15 min) --------------------------------------


# 2.1  Tres gráficos, una figura. Combina `p_tend` y `p_rank` lado a lado, y
#      `p_disp` debajo, ocupando todo el ancho.
fig1 <- (p_tend ___ p_rank) ___ p_disp
fig1


# 2.2  Leyendas y etiquetas. Junta las leyendas, dale más alto a la fila de
#      arriba y agrega las letras A, B, C y un título general.
fig2 <- fig1 +
  plot_layout(guides = "___", heights = c(___, 1)) +
  plot_annotation(tag_levels = "___", title = "Producción científica del posgrado")
fig2


# 2.3  `+` contra `&`. Aplica un tema a una figura combinada de las dos maneras
#      y compara.
(p_tend | p_rank) + theme_bw()
(p_tend | p_rank) & theme_bw()

# Pregunta: ¿en cuál de las dos el tema llega a todos los gráficos? ¿Cuándo te
#           interesaría que llegara solo al último?
# Mi respuesta:


# 2.4  Exportar. Guarda `fig2` como PNG de 300 dpi (18 × 12 cm) y como PDF, y
#      compara el tamaño de los archivos.
dir.create(here("outputs", "figuras"), recursive = TRUE, showWarnings = FALSE)

ggsave(here("outputs", "figuras", "figura1.png"), fig2,
       width = ___, height = 12, units = "___", dpi = ___)
ggsave(here("outputs", "figuras", "figura1.pdf"), fig2,
       width = 18, height = 12, units = "cm", device = cairo_pdf)

file.info(list.files(here("outputs", "figuras"), full.names = TRUE))[, "size", drop = FALSE]

# Pregunta: ¿cuál archivo pesa más? ¿Cuándo preferirías el PDF?
# Mi respuesta:


# (`device = cairo_pdf` hace que los acentos y símbolos salgan bien en el PDF.
# Recuerda: `outputs/` está en tu `.gitignore` desde la sesión 1, por eso no se
# versiona.)


# 2.5  Función + `map2()` + patchwork. Escribe `panel()`, que reciba el nombre
#      de una variable como texto y dibuje su serie por área; luego arma tres
#      paneles con `map2()` y combínalos con `wrap_plots()`.
panel <- function(datos, var, titulo) {
  datos |>
    summarise(valor = sum(.data[[___]]), .by = c(anio, area)) |>
    ggplot(aes(anio, valor, color = area)) +
    geom_line(linewidth = 1) +
    scale_x_continuous(breaks = c(2015, 2020, 2025)) +
    scale_color_manual(values = pal_area) +
    labs(title = titulo, x = NULL, y = NULL, color = NULL)
}

paneles <- map2(
  c("n_articulos", "n_citas", "financiamiento_mm"),
  c("Artículos", "Citas", "Financiamiento"),
  \(v, t) panel(produccion, ___, ___)
)

wrap_plots(paneles, nrow = 1) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")

# Pregunta: ¿qué cambias para tener los tres paneles uno sobre otro? (Pista:
#           `ncol`.)
# Mi respuesta:


# Bloque 3 · Visualizaciones interactivas (15 min) -----------------------------


# 3.1  `ggplotly()`. Haz un gráfico de dispersión de horas de investigación
#      contra publicaciones, coloreado por nivel, y hazlo interactivo.
datos_disp <- encuesta |>
  left_join(programas, by = "programa_id") |>
  filter(!is.na(horas_investigacion_sem))

p_horas <- ggplot(datos_disp, aes(horas_investigacion_sem, n_publicaciones, color = nivel)) +
  geom_jitter(width = 0, height = 0.15, alpha = 0.7) +
  scale_color_manual(values = pal_nivel) +
  labs(x = "Horas de investigación por semana", y = "Publicaciones", color = NULL)

ggplotly(___)

# Pasa el cursor por los puntos, haz zoom y oculta una serie desde la leyenda.


# 3.2  Tooltips a medida. Cambia el mensaje: que muestre el programa, las horas
#      y las publicaciones.
p_tip <- ggplot(datos_disp,
    aes(horas_investigacion_sem, n_publicaciones, color = nivel,
        text = paste0("Programa: ", ___,
                      "<br>Horas/sem: ", horas_investigacion_sem,
                      "<br>Publicaciones: ", ___))) +
  geom_jitter(width = 0, height = 0.15, alpha = 0.7) +
  scale_color_manual(values = pal_nivel) +
  labs(x = "Horas de investigación por semana", y = "Publicaciones", color = NULL)

ggplotly(p_tip, tooltip = "___") |>
  config(displayModeBar = FALSE)

# (Es normal que R avise que `text` es una estética desconocida: `ggplot2` la
# ignora y `plotly` la usa.)


# 3.3  Guardar y compartir. Guarda el gráfico en un único archivo HTML y ábrelo
#      en tu navegador.
w <- ggplotly(p_tip, tooltip = "text")

dir.create(here("outputs"), showWarnings = FALSE)
htmlwidgets::saveWidget(w, here("outputs", "grafico_interactivo.html"), selfcontained = ___)

file.size(here("outputs", "grafico_interactivo.html")) / 1e6    # tamaño en MB

# (Si aparece una carpeta `grafico_interactivo_files` junto al HTML, es un
# residuo: el archivo ya incluye todo y puedes borrarla.)

# Pregunta: ¿cuánto pesa el archivo? ¿Por qué pesa tanto para tan pocos datos?
# Mi respuesta:


# 3.4  Interactivo y patchwork. Prueba qué pasa al convertir una figura
#      combinada, y arma la alternativa.
ggplotly(p_tend | p_rank)                     # ¿qué ocurre?

subplot(ggplotly(p_tend), ggplotly(p_rank), nrows = ___, margin = 0.05)

# Pregunta: ¿qué diferencia ves entre las dos salidas?
# Mi respuesta:


# 3.5  Tablas interactivas. Convierte la tabla de producción en una tabla con
#      filtros por columna.
produccion |>
  select(programa, anio, n_articulos, n_citas, financiamiento_mm) |>
  datatable(filter = "___", rownames = FALSE)


# 3.6  Guardar tu trabajo. Guarda el script (Ctrl + S). En la TERMINAL:
#   git add R/sesion3.R
#   git commit -m "Agrega figuras combinadas e interactivas"


# Retos (si te sobra tiempo) ---------------------------------------------------


# Reto 1 · Etiquetas directas. Modifica `p_tend` para quitar la leyenda y
#          escribir el nombre de cada área al final de su línea (pista: una
#          tabla con solo el último año, `geom_text()` con `hjust = 0` y
#          `scale_x_continuous(expand = expansion(mult = c(0.02, 0.4)))`).
# (Escribe tu código aquí.)


# Reto 2 · Mapa de calor. Con `encuesta` y `programas`, calcula la media de
#          cada dimensión `sat_` por programa y dibújala como mapa de calor
#          (`geom_tile()`), con el valor escrito en cada celda y una paleta
#          adecuada para valores ordenados. (Pista:
#          `pivot_longer(starts_with("sat_"))`.)
# (Escribe tu código aquí.)


# Reto 3 · Guardar con una función. Escribe `guardar_figura(p, nombre, ancho =
#          18, alto = 12)`, que guarde `p` en `outputs/figuras/` como PNG (300
#          dpi) y como PDF. Aplícala con `walk2()` a una lista con `fig1` y
#          `fig2`.
# (Escribe tu código aquí.)


# Cierre (5 min) ---------------------------------------------------------------

# Revisa tu lista (cambia `[ ]` por `[x]`):

#   [ ] Escribí mi código en `R/sesion3.R` y guardé cada gráfico en un objeto.
#   [ ] Mis gráficos usan una paleta con nombres y un tema propio.
#   [ ] `outputs/figuras/` contiene figuras generadas por código, no con el
#       botón Export.
#   [ ] `git log --oneline` muestra mis commits de hoy.


# TAREA PARA LA PRÓXIMA SESIÓN (Regresión lineal):
#   1. Termina los retos que te falten.

#   2. Ordena tu código: pasa `pal_area`, `pal_nivel` y `theme_posgrado()` a
#      `R/funciones.R` y, en este script, reemplázalos por `source(here("R",
#      "funciones.R"))`. Comprueba que todo sigue funcionando y haz un commit:

# En la TERMINAL:
#   git add R/funciones.R R/sesion3.R
#   git commit -m "Mueve la paleta y el tema a R/funciones.R"

#   3. Trae dos variables de tu propia investigación (una que quieras explicar
#      y otra que la explique) y haz con ellas un gráfico de dispersión con
#      `geom_smooth(method = "lm")`. Es el punto de partida de la próxima
#      sesión.
