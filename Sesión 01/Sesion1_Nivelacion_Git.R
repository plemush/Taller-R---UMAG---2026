# ==============================================================================
# Práctica — Sesión 1: Nivelación y Git
# Análisis de Datos en R · Nivel Básico-Intermedio · Duración: 60 minutos
# ==============================================================================
#
# CÓMO TRABAJAR ESTE ARCHIVO
#
# Este archivo es TU SCRIPT DE TRABAJO: lo vas a completar directamente aquí.
# En el paso 0.4 lo guardas como R/analisis.R dentro de tu proyecto.
#
#   * Los espacios marcados con ___ son lo que debes completar.
#   * Ejecuta el código línea por línea o por bloques con Ctrl + Enter
#     (Cmd + Enter en Mac). No uses "Source" hasta haber completado los ___,
#     porque R no entiende ___ y mostrará un error.
#   * Las líneas que empiezan con "#" son instrucciones o comentarios: no se ejecutan.
#   * Las líneas marcadas "TERMINAL" NO se escriben en la consola de R: se escriben
#     (sin el "#") en la pestaña Terminal de RStudio.
#   * Escribe tus respuestas a las preguntas donde dice "Mi respuesta:".
#   * Si te trabas más de 3 minutos en un ejercicio, pasa al siguiente y vuelve
#     después: el objetivo es que practiques el flujo completo, no que todo salga perfecto.
#   * Tip RStudio: abre el panel "Outline" (Ctrl + Shift + O) para navegar entre bloques.
#   * Si ves caracteres raros en las tildes: File > Reopen with Encoding > UTF-8.
#
# BLOQUES DE LA PRÁCTICA
#
#   Bloque 0 · Preparación del proyecto ...................... 5 min
#   Bloque 1 · Nivelación: tidyverse esencial ................ 15 min
#   Bloque 2 · dplyr avanzado: across(), pick(), if_all(), .by  15 min
#   Bloque 3 · Control de versiones con Git .................. 20 min
#   Cierre y dudas ........................................... 5 min
#
# DATOS (simulados, no corresponden a personas reales)
#
#   encuesta_postgrado.csv  150 filas: encuesta a estudiantes de postgrado
#   programas.csv           6 programas con su área
#
# REQUISITOS
#
#   R >= 4.1, RStudio, paquetes tidyverse y here, y Git instalado
#   (en la Terminal, `git --version` debe responder).
# ==============================================================================


# Bloque 0 · Preparación (5 min) ----------------------------------------------

# 0.1  En RStudio: File > New Project > New Directory > New Project.
#      Nómbralo "curso-r-intermedio".
#      Marca la casilla "Create a git repository" solo si la ves; si no, lo
#      haremos a mano en el Bloque 3.

# 0.2  Crea la estructura de carpetas (ejecuta estas líneas en la consola de R,
#      con el proyecto ya abierto):
dir.create("data/raw", recursive = TRUE)
dir.create("R")
dir.create("outputs")

# 0.3  Copia encuesta_postgrado.csv y programas.csv a la carpeta data/raw/.

# 0.4  Guarda ESTE archivo dentro del proyecto con File > Save As... como
#      R/analisis.R (en la carpeta "R" que acabas de crear).
#      Luego ejecuta estas líneas:
library(tidyverse)
library(here)


# Bloque 1 · Nivelación (15 min) ----------------------------------------------

# 1.1  Importar. Lee ambos archivos con rutas construidas con here() y revisa
#      la estructura.
encuesta  <- read_csv(here("data", "raw", "___"))
programas <- read_csv(here("data", "raw", "___"))
glimpse(encuesta)

# Pregunta: ¿cuántas filas y columnas tiene `encuesta`? ¿Qué tipo de dato tiene `nivel`?
# Mi respuesta:


# 1.2  Diagnóstico de faltantes. Cuenta los NA de cada columna.
colSums(is.na(___))

# Pregunta: ¿en qué columnas hay valores faltantes?
# Mi respuesta:


# 1.3  Limpiar `nivel`. Mira cuántas categorías distintas tiene con
#      count(encuesta, nivel). Verás que hay más de 2 por mayúsculas y espacios
#      sobrantes. Normalízalo a solo "Doctorado" y "Magíster".
count(encuesta, nivel)

encuesta <- encuesta |>
  mutate(
    nivel = str_to_lower(str_trim(___)),
    nivel = case_when(
      nivel == "doctorado" ~ "Doctorado",
      nivel == "___"       ~ "Magíster",
      .default = NA_character_
    )
  )
count(encuesta, nivel)   # deben quedar 2 categorías


# 1.4  Variables nuevas. Crea `anios_en_programa` (año actual 2026 menos
#      `anio_ingreso`) y `perfil_publicaciones` con tres categorías:
#      "Sin publicaciones" (0), "1 a 2" y "3 o más".
#      Cuida que un NA en `n_publicaciones` siga siendo NA.
encuesta <- encuesta |>
  mutate(
    anios_en_programa = ___ - anio_ingreso,
    perfil_publicaciones = case_when(
      is.na(n_publicaciones) ~ NA_character_,
      n_publicaciones == 0   ~ "___",
      n_publicaciones <= ___ ~ "1 a 2",
      .default               = "3 o más"
    )
  )


# 1.5  Resumen por grupo. Por `nivel`: número de personas, media de horas de
#      investigación semanales y media de publicaciones (ignorando los NA).
encuesta |>
  group_by(___) |>
  summarise(
    n = n(),
    horas_media = mean(horas_investigacion_sem, na.rm = ___),
    pubs_media  = mean(___, na.rm = TRUE)
  )


# 1.6  Unir tablas. Agrega la información de `programas` y calcula las horas
#      medias por `area`, ordenadas de mayor a menor.
encuesta_prog <- encuesta |> left_join(programas, by = "___")

encuesta_prog |>
  group_by(area) |>
  summarise(horas_media = mean(horas_investigacion_sem, na.rm = TRUE)) |>
  arrange(___(horas_media))


# 1.7  Filtrar. ¿Qué doctorandos tienen 3 o más publicaciones y dedican más de
#      30 horas semanales? Muestra solo `id`, `programa`, `n_publicaciones` y
#      `horas_investigacion_sem`.
#      (Escribe tu código aquí, sin esqueleto.)



# Bloque 2 · dplyr avanzado (15 min) ------------------------------------------

# Hasta ahora aplicamos una operación a UNA columna a la vez.
# Con across() la aplicamos a MUCHAS.

# 2.1  across() en summarise(). Calcula la media (sin NA) de
#      `horas_investigacion_sem`, `n_publicaciones` y `n_congresos` en una sola
#      llamada.
encuesta_prog |>
  summarise(across(
    c(___, n_publicaciones, n_congresos),
    \(x) mean(x, na.rm = TRUE)
  ))

# Variante: repítelo eligiendo columnas con un selector en vez de nombres:
#   where(is.numeric) & !c(id, anio_ingreso, anios_en_programa, duracion_semestres)
# (Escribe tu código aquí.)



# 2.2  Varias funciones, por grupo. Para todas las columnas que empiezan con
#      `sat_`, calcula media y desviación estándar por `area`.
#      Usa .names = "{.col}_{.fn}" y redondea a 2 decimales.
encuesta_prog |>
  group_by(area) |>
  summarise(across(
    starts_with("___"),
    list(media = \(x) mean(x, na.rm = TRUE), sd = \(x) ___(x, na.rm = TRUE)),
    .names = "{.col}_{.fn}"
  )) |>
  mutate(across(where(is.numeric), \(x) round(x, 2)))


# 2.3  across() en mutate(). Centra cada puntaje `sat_` restándole su media,
#      guardando el resultado en columnas nuevas llamadas c_sat_...
encuesta_prog <- encuesta_prog |>
  mutate(across(starts_with("sat_"), \(x) x - mean(x, na.rm = TRUE), .names = "c_{.col}"))

# Pregunta: ¿por qué el prefijo es `c_` y no un sufijo como `_c`?
#   (Pista: piensa en qué pasaría con starts_with("sat_") en el paso siguiente
#    si las columnas nuevas también empezaran con `sat_`.)
# Mi respuesta:


# 2.4  pick(). Crea `indice_satisfaccion`: el promedio por fila de las columnas
#      `sat_` (ignorando NA).
encuesta_prog <- encuesta_prog |>
  mutate(indice_satisfaccion = rowMeans(pick(starts_with("___")), na.rm = TRUE))


# 2.5  if_all() e if_any().
#      (a) ¿Cuántas personas puntúan 4 o más en TODAS las dimensiones?
#      (b) ¿Quiénes tienen ALGUNA dimensión sin respuesta?
encuesta_prog |> filter(if_all(starts_with("sat_"), \(x) x >= ___)) |> nrow()
encuesta_prog |> filter(if_any(starts_with("sat_"), ___)) |> select(id, starts_with("sat_"))

# Mi respuesta (a):


# 2.6  Agrupar al vuelo con .by. Número de personas e índice medio por `area` y
#      `nivel`, sin usar group_by(). Luego obtén las 2 personas con mayor índice
#      en cada área.
encuesta_prog |>
  summarise(n = n(), indice = mean(indice_satisfaccion), .by = c(___, nivel))

encuesta_prog |>
  slice_max(indice_satisfaccion, n = 2, by = area, with_ties = FALSE)


# RETO (si te sobra tiempo)
# Arma una tabla con una fila por `area` y una columna por dimensión
# (supervisión, infraestructura, ...) con el puntaje medio.
# Pista: pivot_longer() -> summarise(.by = ) -> pivot_wider().
# (Escribe tu código aquí.)



# Bloque 3 · Git (20 min) -----------------------------------------------------

# Los comandos de este bloque se escriben en la TERMINAL de RStudio (pestaña
# "Terminal", junto a la consola), con el proyecto abierto. Cópialos SIN el "#"
# del comienzo. Antes de empezar, guarda este archivo (Ctrl + S).

# 3.1  Presentarte ante Git (una sola vez por computador). TERMINAL:
#   git config --global user.name  "Tu Nombre"
#   git config --global user.email "tu_correo@ejemplo.cl"
#   git config --global init.defaultBranch main
#   git config --global --list


# 3.2  Crear el repositorio. TERMINAL:
#   git init -b main
#   git status
#
# Pregunta: ¿qué archivos aparecen como "untracked" (sin seguimiento)?
# Mi respuesta:


# 3.3  Decidir qué NO se versiona. Crea un archivo llamado .gitignore en la raíz
#      del proyecto (File > New File > Text File) con este contenido:
#
#        .Rproj.user
#        .Rhistory
#        .RData
#        .Ruserdata
#        data/sensible/
#        outputs/
#
#      Comprueba que funciona: crea la carpeta data/sensible/ con un archivo
#      cualquiera dentro (por ejemplo prueba.csv) y verifica que Git lo ignora.
#      TERMINAL:
#   git check-ignore -v data/sensible/prueba.csv
#   git status
#
# Idea clave para investigadores: los datos con información personal o bajo
# acuerdo de confidencialidad NUNCA deben subirse a un repositorio, ni siquiera
# a uno privado.


# 3.4  Primer commit. TERMINAL:
#   git add .
#   git status
#   git commit -m "Estructura inicial del proyecto y datos de la encuesta"


# 3.5  Segundo commit: un cambio con sentido. Agrega AL FINAL de este archivo
#      (R/analisis.R) una sección "Conclusiones" con 2 o 3 comentarios sobre lo
#      que encontraste (por ejemplo, qué área dedica más horas de investigación).
#      Guarda (Ctrl + S) y revisa qué cambió ANTES de confirmarlo. TERMINAL:
#   git diff
#   git add R/analisis.R
#   git commit -m "Agrega conclusiones del analisis exploratorio"


# 3.6  Mirar el historial. TERMINAL:
#   git log --oneline


# 3.7  Deshacer un error. Borra por accidente TODO el contenido de este archivo
#      (selecciona todo con Ctrl + A, bórralo y guarda). No entres en pánico:
#      recupera la última versión confirmada. TERMINAL:
#   git diff --stat
#   git restore R/analisis.R
#
#      (Si RStudio pregunta si quieres recargar el archivo, di que sí.)
#
# Pregunta: ¿qué habría pasado si no hubieras hecho commit antes?
# Mi respuesta:


# 3.8  Ramas: probar sin miedo. Crea una rama para experimentar con across(),
#      agrega ahí AL FINAL de este archivo un experimento nuevo (por ejemplo, la
#      mediana de cada columna `sat_`) y confírmalo. Luego vuelve a main y
#      fusiona. TERMINAL:
#   git switch -c explorar-across
#   # ... agrega el experimento al final de R/analisis.R, guarda ...
#   git add R/analisis.R
#   git commit -m "Agrega experimento con across(): medianas de sat_"
#   git switch main
#   git merge --no-ff explorar-across -m "Incorpora el experimento con across()"
#   git log --oneline --graph
#
# META DEL BLOQUE: al terminar, `git log --oneline` debe mostrar AL MENOS 4
# commits con mensajes que expliquen QUÉ cambió y POR QUÉ, y `git status` debe
# decir "nothing to commit, working tree clean".


# 3.9  Opcional / tarea: conectar con GitHub. Crea un repositorio VACÍO en
#      github.com (sin README), y desde la TERMINAL:
#   git remote add origin https://github.com/TU_USUARIO/curso-r-intermedio.git
#   git push -u origin main
#
#      GitHub no acepta contraseñas en `git push`: necesitas un Personal Access
#      Token. En la consola de R (puedes ejecutar estas líneas):
# usethis::create_github_token()
# gitcreds::gitcreds_set()
#      En la próxima sesión retomamos GitHub con calma.


# Cierre (5 min) --------------------------------------------------------------

# Antes de terminar, comprueba tu lista (cambia [ ] por [x]):
#
#   [ ] Importo y limpio datos con read_csv(), mutate() y case_when().
#   [ ] Resumo por grupos con group_by() / .by y combino tablas con left_join().
#   [ ] Aplico una función a muchas columnas con across().
#   [ ] Mi proyecto tiene un repositorio Git con al menos 4 commits descriptivos.
#   [ ] Sé qué cosas NO debo versionar (datos sensibles, resultados temporales).
#
# TAREA PARA LA PRÓXIMA SESIÓN (Programación funcional): termina el Reto del
# Bloque 2 y, si puedes, sube tu repositorio a GitHub (3.9).


# Conclusiones ----------------------------------------------------------------
# (Aquí escribirás tus conclusiones en el ejercicio 3.5.)
