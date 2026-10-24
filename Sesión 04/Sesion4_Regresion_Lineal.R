# ==============================================================================
# Práctica — Sesión 4: Regresión lineal
# Análisis de Datos en R · Nivel Básico-Intermedio · Duración: 60 minutos
# ==============================================================================
#
# CÓMO TRABAJAR ESTE ARCHIVO
#
# Este archivo es TU SCRIPT DE TRABAJO: lo vas a completar directamente aquí.
# En el paso 0.4 lo guardas como R/sesion4.R dentro de tu proyecto.
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
#   * Guarda cada modelo en un objeto (m1, m2...) y míralo con summary() o con
#     tidy() antes de pasar al siguiente: un modelo se entiende leyéndolo.
#   * Tip RStudio: abre el panel "Outline" (Ctrl + Shift + O) para navegar entre bloques.
#   * Si ves caracteres raros en las tildes: File > Reopen with Encoding > UTF-8.
#
# BLOQUES DE LA PRÁCTICA
#
#   Bloque 0 · Preparación .................................... 5 min
#   Bloque 1 · Regresión lineal simple ........................ 20 min
#   Bloque 2 · Regresión lineal múltiple ...................... 20 min
#   Bloque 3 · Cuidados y reporte ............................. 10 min
#   Cierre y dudas ............................................ 5 min
#
# DATOS (todos simulados, no corresponden a personas reales)
#
#   egresados.csv   (NUEVO) 200 egresados: programa_id, nivel (Doctorado o Magíster),
#                   area, horas_investigacion_sem, sat_supervision (1 a 5; tiene 6
#                   valores faltantes), beca, edad_ingreso y la variable que queremos
#                   explicar, meses_titulacion.
#   programas.csv   los 6 programas con su área (de sesiones anteriores).
#
# REQUISITOS
#
#   R >= 4.1, RStudio, el proyecto "curso-r-intermedio" (con su repositorio de Git)
#   y los paquetes tidyverse, here y broom.
# ==============================================================================


# Bloque 0 · Preparación (5 min) -----------------------------------------------


# 0.1  Abre el proyecto `curso-r-intermedio` en RStudio. En la terminal, `git
#      status` debería indicar que no hay cambios pendientes.


# 0.2  Copia `egresados.csv` a `data/raw/`.


# 0.3  Instala `broom` si te falta (solo una vez), en la consola:
install.packages("broom")


# 0.4  Crea un script nuevo, guárdalo como `R/sesion4.R` y escribe al comienzo:
library(tidyverse)
library(here)
library(broom)

egresados <- read_csv(here("data", "raw", "egresados.csv")) |>
  mutate(nivel = factor(nivel, levels = c("Doctorado", "Magíster")),
         beca  = factor(beca,  levels = c("Con beca", "Sin beca")),
         area  = factor(area))

glimpse(egresados)
colSums(is.na(egresados))

# Regla de trabajo para toda la práctica: guarda cada modelo en un objeto
# (`m1`, `m2`…) y míralo con `summary()` o con `tidy()` antes de pasar al
# siguiente. Un modelo se entiende leyéndolo, no ajustándolo.


# Bloque 1 · Regresión lineal simple (20 min) ----------------------------------


# 1.1  Explorar antes de modelar. Dibuja las horas de investigación contra los
#      meses hasta titularse, con la recta de regresión, y calcula la
#      correlación.
ggplot(egresados, aes(___, ___)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE)

cor(egresados$horas_investigacion_sem, egresados$meses_titulacion)

# Pregunta: ¿la relación sube o baja? ¿Qué tan fuerte te parece?
# Mi respuesta:


# 1.2  Ajustar el modelo. Ajusta la regresión simple de `meses_titulacion` en
#      función de `horas_investigacion_sem`.
m1 <- lm(___ ~ horas_investigacion_sem, data = ___)
summary(m1)

# Pregunta: ¿cuánto valen el intercepto y la pendiente? Ubícalos en la tabla de
#           coeficientes.
# Mi respuesta:


# 1.3  Interpretar con intervalos. Obtén los coeficientes y sus intervalos de
#      confianza del 95%.
coef(m1)
confint(m1, level = ___)

# Pregunta: escribe una frase que interprete la pendiente, con unidades y con
#           la expresión «en promedio». ¿El intervalo incluye al cero?
# Mi respuesta:


# 1.4  Resultados ordenados con `broom`. Pasa el modelo a tablas con `tidy()` y
#      `glance()`.
tidy(m1, conf.int = ___)

glance(m1) |>
  select(r.squared, adj.r.squared, sigma, nobs)

# Pregunta: ¿qué columna de `tidy()` corresponde al error estándar? ¿Y qué
#           significa `sigma` en este modelo (¿en qué unidades está)?
# Mi respuesta:


# 1.5  R². Comprueba que, en regresión simple, R² es la correlación al
#      cuadrado.
cor(egresados$horas_investigacion_sem, egresados$meses_titulacion)^___
glance(m1)$___

# Pregunta: ¿qué porcentaje de la variación de los meses hasta titularse
#           explican las horas?
# Mi respuesta:


# 1.6  Predicciones. Predice los meses esperados para 15, 25 y 35 horas
#      semanales, con los dos tipos de intervalo.
nuevos <- tibble(horas_investigacion_sem = c(15, 25, ___))

predict(m1, newdata = nuevos, interval = "confidence")
predict(m1, newdata = nuevos, interval = "___")

# Pregunta: ¿por qué el segundo intervalo es mucho más ancho que el primero?
#           ¿Cuál usarías para hablar de una persona en particular?
# Mi respuesta:


# 1.7  Guardar tu trabajo. Guarda el script (Ctrl + S). En la TERMINAL:
#   git add R/sesion4.R
#   git commit -m "Agrega regresion simple de meses_titulacion"


# Bloque 2 · Regresión lineal múltiple (20 min) --------------------------------


# 2.1  La sospecha. El modelo simple dice que más horas se asocian con más
#      meses. Revisa si el nivel del programa tiene algo que ver.
egresados |>
  summarise(horas = mean(horas_investigacion_sem),
            meses = mean(meses_titulacion),
            n = n(), .by = ___)

ggplot(egresados, aes(horas_investigacion_sem, meses_titulacion, color = nivel)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE)

# Pregunta: ¿qué pasa con la pendiente de la recta dentro de cada nivel? ¿Por
#           qué la recta global subía?
# Mi respuesta:


# 2.2  Controlar por el nivel. Agrega `nivel` al modelo.
m2 <- lm(meses_titulacion ~ horas_investigacion_sem + ___, data = egresados)
summary(m2)

# Pregunta: compara el coeficiente de las horas en `m1` y en `m2`. ¿Cambió el
#           signo? Escribe la interpretación del coeficiente de `nivelMagíster`
#           (¿contra qué categoría se compara?).
# Mi respuesta:


# 2.3  El modelo completo. Suma la satisfacción con la supervisión, la beca, el
#      área y la edad de ingreso.
m3 <- lm(meses_titulacion ~ horas_investigacion_sem + nivel + ___ + beca + area + edad_ingreso,
         data = egresados)
summary(m3)
nobs(m3)

# Pregunta: ¿con cuántos egresados se ajustó `m3` y por qué no son 200? ¿Qué
#           variable no parece asociada con los meses hasta titularse una vez
#           controlado el resto?
# Mi respuesta:


# 2.4  Cambiar la categoría de referencia. Por defecto, `area` se compara
#      contra Ciencias Naturales. Cambia la referencia a Ingeniería y mira los
#      coeficientes de `area`.
egresados_b <- egresados |>
  mutate(area = relevel(area, ref = "___"))

m3b <- lm(formula(m3), data = egresados_b)

tidy(m3b, conf.int = TRUE) |>
  filter(str_detect(term, "___"))

c(glance(m3)$r.squared, glance(m3b)$r.squared)

# Pregunta: ¿cambió el R² al cambiar la referencia? ¿Qué cambió entonces?
# Mi respuesta:


# 2.5  R² ajustado y comparación de modelos. Compara el modelo con nivel y el
#      modelo completo. Para que sea justo, usa solo las filas completas.
dd <- egresados |> drop_na()

ma <- lm(meses_titulacion ~ horas_investigacion_sem + nivel, data = dd)
mb <- update(ma, . ~ . + sat_supervision + beca + area + edad_ingreso)

bind_rows(simple = glance(ma), completo = glance(mb), .id = "modelo") |>
  select(modelo, r.squared, ___)

anova(ma, mb)

# Pregunta: ¿las variables adicionales mejoran el modelo? ¿Cómo lo sabes (mira
#           el valor p de `anova()`)?
# Mi respuesta:


# 2.6  Gráfico de coeficientes. Dibuja los coeficientes de `m3` (sin el
#      intercepto) con su intervalo de confianza.
tidy(m3, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(term = fct_reorder(term, ___)) |>
  ggplot(aes(estimate, term)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_pointrange(aes(xmin = conf.low, xmax = ___)) +
  labs(x = "Cambio en meses (manteniendo lo demás constante)", y = NULL)

# Pregunta: ¿qué intervalos cruzan el cero? ¿Cuál es el efecto más grande?
# Mi respuesta:


# 2.7  Una interacción. ¿El efecto de las horas es el mismo en doctorado y en
#      magíster? Permite que la pendiente cambie según el nivel.
mi <- lm(meses_titulacion ~ horas_investigacion_sem ___ nivel, data = egresados)
tidy(mi)

# Pregunta: ¿cuál es la pendiente de las horas en Doctorado y cuál en Magíster?
#           (Pista: la de Magíster es la suma del coeficiente de las horas y el
#           de la interacción.)
# Mi respuesta:


# Bloque 3 · Cuidados y reporte (10 min) ---------------------------------------


# 3.1  Un vistazo a los residuos. Con `augment()`, dibuja los residuos contra
#      los valores ajustados de `m2` y un gráfico cuantil-cuantil.
a <- augment(m2)

ggplot(a, aes(.fitted, ___)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = 2)

ggplot(a, aes(sample = .std.resid)) +
  stat_qq() +
  stat_qq_line()

# Pregunta: ¿ves algún patrón en la nube de residuos? ¿Los puntos del gráfico
#           cuantil-cuantil siguen la línea? (En la sesión 5 aprenderemos a
#           diagnosticar esto en detalle.)
# Mi respuesta:


# 3.2  No extrapolar. Ajusta un modelo solo con doctorandos y pídele una
#      predicción para una cifra imposible.
doctores <- filter(egresados, nivel == "Doctorado")
range(doctores$horas_investigacion_sem)

m_doc <- lm(meses_titulacion ~ horas_investigacion_sem, data = doctores)
predict(m_doc, tibble(horas_investigacion_sem = ___))

# Pregunta: ¿qué resultado obtienes y por qué no debes creerle? ¿Cuál es el
#           rango de horas observado?
# Mi respuesta:


# 3.3  Una tabla de resultados. Arma una tabla lista para pegar en un informe.
tidy(m3, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>
  transmute(Término = term,
            Estimación = round(estimate, 2),
            `IC 95%` = str_c("[", round(conf.low, 2), "; ", round(___, 2), "]"),
            `Valor p` = scales::pvalue(p.value)) |>
  knitr::kable()

# Pregunta: escribe una frase de reporte para el efecto de la beca (estimación,
#           intervalo, lenguaje de asociación y no de causa).
# Mi respuesta:


# 3.4  Guardar tu trabajo. Guarda el script (Ctrl + S). En la TERMINAL:
#   git add R/sesion4.R
#   git commit -m "Agrega regresion multiple, diagnostico basico y reporte"


# Retos (si te sobra tiempo) ---------------------------------------------------


# Reto 1 · Coeficientes estandarizados. Ajusta `m3` con las variables numéricas
#          estandarizadas (`scale()`), de modo que los coeficientes sean
#          comparables entre sí. ¿Qué variable numérica tiene el mayor efecto,
#          en desviaciones estándar?
# (Escribe tu código aquí.)


# Reto 2 · Un modelo por área. Con `nest()`, `map()` y `tidy()` (sesión 2),
#          ajusta el modelo `meses_titulacion ~ horas_investigacion_sem +
#          nivel` por separado para cada área y reúne en una tabla el
#          coeficiente de las horas de cada una, con su intervalo.
# (Escribe tu código aquí.)


# Reto 3 · Una función para reportar. Escribe `formatear_modelo(m)`, que reciba
#          un modelo y devuelva la tabla de resultados del ejercicio 3.3.
#          Aplícala a `m2` y a `m3`.
# (Escribe tu código aquí.)


# Cierre (5 min) ---------------------------------------------------------------

# Revisa tu lista (cambia `[ ]` por `[x]`):

#   [ ] Escribí mi código en `R/sesion4.R` y guardé cada modelo en un objeto.
#   [ ] Interpreto la pendiente con unidades, «en promedio» y «manteniendo
#       constantes las demás variables».
#   [ ] Sé leer un coeficiente de una variable categórica (contra qué
#       referencia se compara).
#   [ ] `git log --oneline` muestra mis commits de hoy.


# TAREA PARA LA PRÓXIMA SESIÓN (Diagnóstico de modelos):
#   1. Termina los retos que te falten.

#   2. Deja el modelo completo (`m3`) guardado en tu script: es el que
#      diagnosticaremos en la sesión 5 (normalidad de los residuos,
#      homocedasticidad y multicolinealidad).

#   3. Trae una variable de tu propia investigación con la que te gustaría
#      ajustar una regresión lineal y anota qué supuestos crees que podrían
#      fallar.
