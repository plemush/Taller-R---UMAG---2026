# ==============================================================================
# Práctica — Sesión 2: Programación funcional
# Análisis de Datos en R · Nivel Básico-Intermedio · Duración: 60 minutos
# ==============================================================================
#
# CÓMO TRABAJAR ESTE ARCHIVO
#
# Este archivo es TU SCRIPT DE TRABAJO: lo vas a completar directamente aquí.
# En el paso 0.3 lo guardas como R/sesion2.R dentro de tu proyecto.
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
#   * Tip RStudio: abre el panel "Outline" (Ctrl + Shift + O) para navegar entre bloques.
#   * Si ves caracteres raros en las tildes: File > Reopen with Encoding > UTF-8.
#
# BLOQUES DE LA PRÁCTICA
#
#   Bloque 0 · Preparación .................................... 5 min
#   Bloque 1 · Funciones propias .............................. 15 min
#   Bloque 2 · Iteración con purrr ............................ 15 min
#   Bloque 3 · Automatizar un análisis ........................ 20 min
#   Cierre y dudas ............................................ 5 min
#
# DATOS (todos simulados, no corresponden a personas reales)
#
#   encuesta_limpia.csv  150 filas: la encuesta de la sesión 1 ya limpia
#                        (`nivel` con dos categorías).
#   programas.csv        6 programas con su área.
#   olas/                cinco archivos, encuesta_2022.csv a encuesta_2026.csv, con
#                        una encuesta de seguimiento anual (58 a 83 filas cada uno).
#                        UNO DE ELLOS TIENE UNA SORPRESA.
#
# REQUISITOS
#
#   R >= 4.1, RStudio, paquetes tidyverse y here, y el proyecto "curso-r-intermedio"
#   de la sesión 1 (con su repositorio de Git).
#
# REGLA DE TRABAJO
#
#   Cada función nueva se escribe en este script, se ejecuta y se prueba enseguida
#   con un caso cuya respuesta conoces. Al final (tarea) las moverás a R/funciones.R.
# ==============================================================================


# Bloque 0 · Preparación (5 min) ----------------------------------------------

# 0.1  Abre el proyecto "curso-r-intermedio" en RStudio. En la TERMINAL:
#   git status
#      debería indicar que no hay cambios pendientes.

# 0.2  Copia encuesta_limpia.csv y programas.csv a data/raw/, y la carpeta olas/
#      completa a data/raw/olas/.

# 0.3  Guarda ESTE archivo dentro del proyecto con File > Save As... como
#      R/sesion2.R (tu R/analisis.R de la sesión 1 se queda como está).
#      Luego ejecuta estas líneas:
library(tidyverse)
library(here)

encuesta  <- read_csv(here("data", "raw", "encuesta_limpia.csv"))
programas <- read_csv(here("data", "raw", "programas.csv"))


# Bloque 1 · Funciones propias (15 min) ---------------------------------------

# 1.1  Tu primera función. Completa error_estandar() (desviación estándar dividida
#      por la raíz del número de datos) y pruébala.
error_estandar <- function(x, na.rm = ___) {
  if (na.rm) x <- x[!is.na(x)]
  sd(___) / sqrt(length(x))
}

error_estandar(c(2, 4, 4, 4, 5, 5, 7, 9))    # debe dar 0.7559289
error_estandar(encuesta$sat_supervision)

# Pregunta: ¿por qué conviene que `na.rm` sea un argumento y no esté fijo dentro
#           de la función?
# Mi respuesta:


# 1.2  Una función desde cero. Escribe coef_variacion(x, na.rm = TRUE), que devuelve
#      la desviación estándar dividida por la media, por 100. Pruébala con
#      coef_variacion(c(10, 12, 14)); debe dar 16.67.
#      Luego aplícala a `horas_investigacion_sem`.
# (Escribe tu código aquí.)



# 1.3  Validar entradas. Mejora error_estandar() para que (a) se detenga con un
#      mensaje claro si `x` no es numérica y (b) avise y devuelva NA si quedan
#      menos de 2 valores.
error_estandar <- function(x, na.rm = TRUE) {
  if (!is.numeric(x)) {
    ___("`x` debe ser numérica, no ", class(x)[1], call. = FALSE)
  }
  if (na.rm) x <- x[!is.na(x)]
  if (length(x) < ___) {
    ___("Se necesitan al menos 2 valores; devuelvo NA", call. = FALSE)
    return(NA_real_)
  }
  sd(x) / sqrt(length(x))
}

try(error_estandar(encuesta$nivel))   # debe dar un error claro (try() evita que el script se detenga)
error_estandar(5)                 # debe dar una advertencia y NA

# Pregunta: ¿por qué un caso usa stop() y el otro warning()?
# Mi respuesta:


# 1.4  Devolver una tabla. Escribe resumen_num(x, na.rm = TRUE), que devuelve un
#      tibble de una fila con `n` (datos no faltantes), `media`, `de` y `ee`
#      (usando error_estandar()). Pruébala con `horas_investigacion_sem`.
resumen_num <- function(x, na.rm = TRUE) {
  tibble(
    n     = sum(!is.na(x)),
    media = ___(x, na.rm = na.rm),
    de    = sd(x, na.rm = na.rm),
    ee    = ___(x, na.rm = na.rm)
  )
}

resumen_num(encuesta$horas_investigacion_sem)


# 1.5  Una función que envuelve dplyr. Completa resumir_por() y úsala para las
#      horas de investigación por `nivel`, y para `sat_supervision` por
#      `programa_id`.
resumir_por <- function(datos, grupo, variable) {
  datos |>
    group_by({{ ___ }}) |>
    summarise(
      n     = sum(!is.na({{ ___ }})),
      media = mean({{ variable }}, na.rm = TRUE),
      ee    = error_estandar({{ variable }}),
      .groups = "drop"
    )
}

encuesta |> resumir_por(nivel, horas_investigacion_sem)
encuesta |> resumir_por(___, ___)


# 1.6  Un argumento con valor por defecto. Escribe porcentaje_alto(x, umbral = 4):
#      el porcentaje de respuestas mayores o iguales que `umbral`, ignorando los NA.
#      porcentaje_alto(c(5, 4, 3, NA, 2)) debe dar 50.
#      Luego úsala DENTRO de across() para todas las columnas `sat_` y repite con
#      umbral = 5.
# (Escribe tu función aquí.)


encuesta |>
  summarise(across(starts_with("sat_"), ___))
encuesta |>
  summarise(across(starts_with("sat_"), \(x) ___))

# Pregunta: ¿en qué dimensión hay un mayor porcentaje de puntajes altos (4 o más)?
# Mi respuesta:


# 1.7  Guardar tu trabajo. Guarda este archivo (Ctrl + S). En la TERMINAL:
#   git add R/sesion2.R
#   git commit -m "Agrega funciones de resumen y validacion"


# Bloque 2 · Iteración con purrr (15 min) -------------------------------------

# 2.1  map_dbl(). Selecciona las columnas `sat_` en un objeto `sat` y calcula la
#      media de cada una.
sat <- encuesta |> select(starts_with("___"))
map_dbl(sat, \(x) mean(x, na.rm = ___))


# 2.2  Tus funciones dentro de map().
#      (a) El error estándar de cada dimensión (un número por columna).
#      (b) La tabla completa de resumen_num() para cada dimensión, apilada en un
#          solo tibble con una columna `dimension`.
map_dbl(sat, ___)

sat |>
  map(___) |>
  list_rbind(names_to = "___")

# Pregunta: ¿qué pasa si en (b) usas map_dbl() en lugar de map()? ¿Por qué?
# Mi respuesta:


# 2.3  Variantes tipadas. ¿Qué columnas de `encuesta` son numéricas? ¿Cuántos NA
#      tiene cada columna? Muestra solo las que tienen al menos uno.
map_lgl(encuesta, ___)

conteo_na <- map_int(encuesta, \(x) sum(is.na(x)))
keep(conteo_na, \(n) n ___ 0)


# 2.4  Dos entradas en paralelo. Con map2_dbl(), calcula la correlación de cada
#      par de variables de la tabla `pares` (usa use = "complete.obs").
pares <- tibble(
  x = c("horas_investigacion_sem", "horas_investigacion_sem", "sat_supervision"),
  y = c("n_publicaciones", "sat_supervision", "sat_infraestructura")
)
pares$r <- map2_dbl(
  pares$x, pares$y,
  \(a, b) cor(encuesta[[___]], encuesta[[___]], use = "complete.obs")
)
pares


# 2.5  Leer muchos archivos. Lista los archivos de data/raw/olas/, léelos con
#      map() y revisa cuántas columnas y filas tiene cada uno.
archivos <- list.files(here("data", "raw", "olas"), pattern = "\\.csv$", full.names = TRUE)

olas_crudas <- archivos |>
  set_names(___) |>
  map(read_csv, show_col_types = FALSE)

map_int(olas_crudas, ncol)
map_int(olas_crudas, nrow)

# Pregunta: ¿qué archivo tiene algo raro? Mira
#           names(olas_crudas[["encuesta_2025.csv"]]) y explica qué pasó.
# Mi respuesta:


# 2.6  Un lector robusto. Escribe leer_ola(): lee el archivo con read_csv() y, si el
#      resultado tiene una sola columna, lo vuelve a leer con read_csv2()
#      (separador ";"). Úsala para leer los cinco archivos y apílalos con una
#      columna `archivo`.
leer_ola <- function(ruta) {
  d <- read_csv(ruta, show_col_types = FALSE)
  if (ncol(d) == ___) d <- read_csv2(ruta, show_col_types = FALSE)
  d
}

olas <- archivos |> set_names(basename) |> map(leer_ola)
map_int(olas, ncol)                       # ahora todos deben tener 10 columnas

todas <- olas |> list_rbind(names_to = "___")
dim(todas)                                # 353 filas


# 2.7  Errores con safely(). Aplica resumir_ola() a las lecturas crudas
#      (olas_crudas) usando safely() y detecta qué elemento falló y por qué.
resumir_ola <- function(d) {
  d |> summarise(n = n(), sup = mean(sat_supervision, na.rm = TRUE))
}

res <- olas_crudas |> map(safely(___))
map_lgl(res, \(r) !is.null(r$___))
conditionMessage(res[["encuesta_2025.csv"]]$error)

# Pregunta: si hubieras usado possibly(resumir_ola, otherwise = NULL), ¿qué habría
#           pasado con el resultado final? ¿Habrías notado el problema?
# Mi respuesta:


# Bloque 3 · Automatizar un análisis (20 min) ---------------------------------

# 3.1  De cinco archivos a una tabla. Encadena todo en un solo pipeline: lista de
#      archivos, nombres, lectura con leer_ola() y apilado.
datos_olas <- archivos |>
  set_names(basename) |>
  map(___) |>
  list_rbind(names_to = "archivo")

count(datos_olas, anio)      # 58, 64, 71, 77 y 83 filas


# 3.2  Tendencia anual. Calcula, por año (.by = anio), el número de respuestas y la
#      media de cada dimensión `sat_`. Ordena por año.
tendencia <- datos_olas |>
  summarise(
    n = n(),
    across(starts_with("sat_"), \(x) ___),
    .by = ___
  ) |>
  arrange(anio)
tendencia

# Pregunta: ¿qué dimensión cambia más entre 2022 y 2026?
# Mi respuesta:


# 3.3  Un modelo por área. Une `programas` para obtener el `area` y ajusta, para
#      cada área, una regresión lineal de `sat_conectividad` sobre `anio`.
#      Extrae la pendiente (el cambio medio por año) y el R².
datos_prog <- datos_olas |> left_join(programas, by = "programa_id")

modelos <- datos_prog |>
  nest(.by = ___) |>
  mutate(
    modelo    = map(data, \(d) lm(sat_conectividad ~ ___, data = d)),
    pendiente = map_dbl(modelo, \(m) coef(m)[["anio"]]),
    r2        = map_dbl(modelo, \(m) summary(m)$r.squared)
  )
modelos |> select(area, pendiente, r2)

# Pregunta: ¿en qué área aumenta más la satisfacción con la conectividad?
#           ¿Cuánto por año?
# Mi respuesta:


# 3.4  Figuras y tablas por área. Escribe graficar_tendencia(d, titulo), que dibuja
#      la media anual de `sat_conectividad` (línea y puntos). Aplícala a cada área
#      con map2() y guarda un PNG por área con walk2() en outputs/figuras/.
graficar_tendencia <- function(d, titulo) {
  d |>
    summarise(media = mean(sat_conectividad, na.rm = TRUE), .by = anio) |>
    ggplot(aes(anio, media)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(limits = c(1, 5)) +
    labs(title = titulo, x = NULL, y = "Satisfacción con conectividad (1–5)")
}

dir.create(here("outputs", "figuras"), recursive = TRUE, showWarnings = FALSE)

por_area <- datos_prog |>
  nest(.by = area) |>
  mutate(
    grafico = map2(data, area, ___),
    ruta    = here("outputs", "figuras", paste0("conectividad_", make.names(area), ".png"))
  )

walk2(por_area$grafico, por_area$ruta,
      \(g, r) ggsave(r, g, width = 6, height = 4, dpi = 150))

# Ahora guarda también una tabla por área con la media anual de todas las
# dimensiones (un CSV por área en outputs/):
rutas_csv <- here("outputs", paste0("tendencia_", make.names(por_area$area), ".csv"))
tablas <- map(por_area$data, \(d) {
  d |>
    summarise(across(starts_with("sat_"), \(x) mean(x, na.rm = TRUE)), .by = anio) |>
    arrange(anio)
})
walk2(tablas, ___, write_csv)


# 3.5  Guardar tu trabajo. Guarda este archivo (Ctrl + S). En la TERMINAL:
#   git add R/sesion2.R
#   git commit -m "Automatiza el analisis de las cinco olas"
#   git log --oneline
#
# META DEL BLOQUE: al terminar debes tener en outputs/ tres figuras y tres tablas,
# generadas SIN copiar ni pegar ningún bloque de código.
# (outputs/ está en tu .gitignore desde la sesión 1: por eso no se versiona.)


# Retos (si te sobra tiempo) --------------------------------------------------

# Reto 1 · Todo en una función. Escribe analizar_carpeta(carpeta), que reciba la
#          ruta de una carpeta con archivos de olas y devuelva la tabla de
#          tendencia anual del ejercicio 3.2. Compruébala con
#          all.equal(analizar_carpeta(here("data", "raw", "olas")), tendencia);
#          debe dar TRUE.
# (Escribe tu código aquí.)



# Reto 2 · reduce(). Con `encuesta`, calcula tres tablas por `programa_id` (media de
#          sat_supervision, media de horas_investigacion_sem y media de
#          n_publicaciones) y únelas en una sola con reduce() y left_join().
# (Escribe tu código aquí.)



# Reto 3 · Nombres como texto. Escribe resumir_texto(datos, variable) que reciba el
#          nombre de la variable ENTRE COMILLAS (usa .data[[variable]]) y devuelva
#          su media y su error estándar. Aplícala con map() a names(sat) y apila
#          los resultados.
# (Escribe tu código aquí.)



# Cierre (5 min) --------------------------------------------------------------

# Revisa tu lista (cambia [ ] por [x]):
#
#   [ ] Escribí y probé mis funciones en este script (R/sesion2.R).
#   [ ] Probé cada función con un caso cuya respuesta conozco.
#   [ ] outputs/ contiene figuras y tablas generadas por código.
#   [ ] `git log --oneline` muestra mis commits de hoy.
#
# TAREA PARA LA PRÓXIMA SESIÓN (Visualización avanzada):
#   1. Termina los retos que te falten.
#   2. Ordena tu código: crea el archivo R/funciones.R, mueve allí tus funciones
#      (error_estandar, coef_variacion, resumen_num, resumir_por, porcentaje_alto,
#      leer_ola, resumir_ola y graficar_tendencia), y en este script reemplázalas por
#          source(here("R", "funciones.R"))
#      Comprueba que todo sigue funcionando y haz un commit:
#   git add R/funciones.R R/sesion2.R
#   git commit -m "Mueve las funciones a R/funciones.R"
#   3. Si puedes, trae un gráfico propio (de tu investigación) que te gustaría mejorar.
