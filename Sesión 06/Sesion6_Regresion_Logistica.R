# ==============================================================================
# Práctica — Sesión 6: Regresión logística
# Análisis de Datos en R · Nivel Básico-Intermedio · Duración: 60 minutos
# ==============================================================================
#
# CÓMO TRABAJAR ESTE ARCHIVO
#
# Este archivo es TU SCRIPT DE TRABAJO: lo vas a completar directamente aquí.
# En el paso 0.4 lo guardas como R/sesion6.R dentro de tu proyecto.
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
#   * Regla de la sesión: antes de interpretar un odds ratio, revisa cuál es el
#     evento (titulo_a_tiempo = 1) y cuál es la referencia de cada factor.
#   * Tip RStudio: abre el panel "Outline" (Ctrl + Shift + O) para navegar entre bloques.
#   * Si ves caracteres raros en las tildes: File > Reopen with Encoding > UTF-8.
#
# BLOQUES DE LA PRÁCTICA
#
#   Bloque 0 · Preparación .................................... 5 min
#   Bloque 1 · De lm a glm .................................... 15 min
#   Bloque 2 · Modelo múltiple, odds ratios y predicciones .... 20 min
#   Bloque 3 · Evaluar el modelo y cuidados ................... 15 min
#   Cierre y dudas ............................................ 5 min
#
# DATOS (todos simulados, no corresponden a personas reales)
#
#   titulacion.csv  (NUEVO) 320 egresados de posgrado. Respuesta: titulo_a_tiempo
#                   (1 = se tituló dentro del plazo, 0 = no). Predictores: nivel,
#                   area, horas_investigacion_sem, sat_supervision (1 a 5), beca y
#                   edad_ingreso.
#   programas.csv   el de las sesiones anteriores (hoy no se usa).
#
# REQUISITOS
#
#   R >= 4.1, RStudio, el proyecto "curso-r-intermedio" (con su repositorio de Git)
#   y los paquetes tidyverse, here, broom, patchwork, car y pROC.
# ==============================================================================


# Bloque 0 · Preparación (5 min) -----------------------------------------------


# 0.1  Abre el proyecto `curso-r-intermedio` en RStudio. En la terminal, `git
#      status` debería indicar que no hay cambios pendientes (tu `R/sesion5.R`
#      ya tiene commit).


# 0.2  Copia `titulacion.csv` a `data/raw/`.


# 0.3  Instala el paquete nuevo (solo una vez), en la consola:
install.packages("pROC")


# 0.4  Crea un script nuevo, guárdalo como `R/sesion6.R` y escribe al comienzo:
library(tidyverse)
library(here)
library(broom)
library(patchwork)
library(car)
library(pROC)

titulacion <- read_csv(here("data", "raw", "titulacion.csv")) |>
  mutate(nivel = factor(nivel, levels = c("Doctorado", "Magíster")),
         beca  = factor(beca,  levels = c("Sin beca", "___")),
         area  = factor(area))

glimpse(titulacion)
levels(titulacion$beca)

# Pregunta: en la sesión 4 la referencia de `beca` era «Con beca». Hoy la
#           dejamos en «Sin beca»: ¿por qué conviene que el evento de interés
#           («con beca») sea el segundo nivel y no el primero?
# Mi respuesta:


# Regla de trabajo para toda la práctica: antes de interpretar un odds ratio,
# revisa siempre cuál es el evento (`titulo_a_tiempo = 1`) y cuál es la
# categoría de referencia de cada factor.


# Bloque 1 · De `lm` a `glm` (15 min) ------------------------------------------


# 1.1  Explorar antes de modelar. Calcula la proporción de egresados que se
#      tituló a tiempo según tengan beca o no, y cuenta los casos por nivel.
titulacion |>
  group_by(beca) |>
  summarise(n = n(), eventos = sum(titulo_a_tiempo), prop = mean(___))

titulacion |>
  count(nivel, titulo_a_tiempo)

# Pregunta: ¿qué proporción se tituló a tiempo con beca y sin beca? Con esas
#           dos proporciones, calcula a mano el odds ratio «crudo» de la beca
#           (odds con beca dividido por odds sin beca).
# Mi respuesta:


# 1.2  Probabilidad, odds y logit. Pasa de una escala a otra con la proporción
#      global de titulados a tiempo.
p <- mean(titulacion$titulo_a_tiempo)
odds <- p / (1 - ___)

p
odds
log(odds)
qlogis(p)
plogis(qlogis(p))

# Pregunta: ¿cuánto valen la probabilidad, la odds y el logit globales? ¿Qué te
#           dice que el logit sea negativo?
# Mi respuesta:


# 1.3  El problema de `lm` con una respuesta 0/1. Ajusta la recta de
#      probabilidad lineal y pídele predicciones con muy pocas y con muchas
#      horas.
m_lpm <- lm(titulo_a_tiempo ~ horas_investigacion_sem, data = titulacion)

extremos <- tibble(horas_investigacion_sem = c(0, ___))
predict(m_lpm, newdata = extremos)

# Pregunta: ¿qué «probabilidad» predice la recta con 65 horas semanales? ¿Qué
#           problema tiene?
# Mi respuesta:


# 1.4  El primer modelo logístico. Ajusta con `glm()` la relación entre las
#      horas de investigación y titularse a tiempo.
m1 <- glm(titulo_a_tiempo ~ horas_investigacion_sem,
          data = titulacion,
          family = ___)

summary(m1)

# Pregunta: ¿cuánto vale el coeficiente de las horas y en qué escala está?
#           Anota también la devianza nula, la devianza residual y el AIC.
# Mi respuesta:


# 1.5  Del coeficiente al odds ratio. Exponencia el coeficiente y calcula su
#      intervalo de confianza.
exp(coef(m1))
exp(confint(m1))

tidy(m1, exponentiate = ___, conf.int = TRUE)

# Odds ratio por 5 horas adicionales
exp(5 * coef(m1)["horas_investigacion_sem"])

# Pregunta: escribe una frase que interprete el odds ratio de las horas (por
#           hora y por cinco horas), con su intervalo. ¿El intervalo incluye al
#           1?
# Mi respuesta:


# 1.6  Probabilidades predichas. Predice la probabilidad de titularse a tiempo
#      con 15, 25 y 35 horas, y calcula con cuántas horas la probabilidad es
#      del 50% (cuando el logit vale cero: `-b0 / b1`).
nuevos <- tibble(horas_investigacion_sem = c(15, 25, 35))
predict(m1, newdata = nuevos, type = "___")

-coef(m1)[1] / coef(m1)[2]

# Pregunta: ¿qué probabilidad corresponde a cada valor? ¿Con cuántas horas
#           semanales la probabilidad llega a 50% en este modelo?
# Mi respuesta:


# 1.7  Guardar tu trabajo. Guarda el script (Ctrl + S). En la TERMINAL:
#   git add R/sesion6.R
#   git commit -m "Agrega modelo logistico simple y odds ratios"


# Bloque 2 · Modelo múltiple, odds ratios y predicciones (20 min) --------------


# 2.1  El modelo múltiple. Agrega las demás variables, igual que en la sesión
#      4, pero con `glm()`.
mf <- glm(titulo_a_tiempo ~ horas_investigacion_sem + nivel + sat_supervision + ___ +
            area + edad_ingreso,
          data = titulacion,
          family = binomial)

summary(mf)

# Pregunta: ¿qué variables tienen coeficiente positivo y cuáles negativo?
#           ¿Cuáles tienen valor p menor que 0,05? ¿Cuánto valen ahora la
#           devianza residual y el AIC, comparados con el modelo simple?
# Mi respuesta:


# 2.2  Odds ratios con intervalos. Arma una tabla ordenada con los odds ratios
#      de todos los términos, sin el intercepto.
or_mf <- tidy(mf, exponentiate = TRUE, conf.int = TRUE) |>
  filter(term != "___")

or_mf |>
  select(term, estimate, conf.low, conf.high, p.value)

# Pregunta: interpreta el odds ratio de la beca y el de la edad de ingreso (por
#           año). ¿Qué odds ratios tienen un intervalo que incluye el 1?
# Mi respuesta:


# 2.3  Gráfico de odds ratios. Dibuja los odds ratios con sus intervalos, en
#      escala logarítmica y con una línea de referencia.
ggplot(or_mf, aes(estimate, term)) +
  geom_vline(xintercept = ___, linetype = 2) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  scale_x_log10() +
  labs(x = "Odds ratio (escala log)", y = NULL)

# Pregunta: ¿qué intervalos cruzan la línea punteada? ¿Qué puedes decir (y qué
#           no) de esos términos?
# Mi respuesta:


# 2.4  Unidades. Expresa el efecto de las horas por cada 5 horas semanales
#      adicionales y el de la edad por cada 5 años.
exp(5 * coef(mf)["horas_investigacion_sem"])
exp(5 * confint(mf)["horas_investigacion_sem", ])

exp(___ * coef(mf)["edad_ingreso"])

# Pregunta: interpreta ambos resultados con una frase. ¿Por qué cinco horas es
#           una unidad más razonable que una hora?
# Mi respuesta:


# 2.5  Probabilidades para perfiles concretos. Predice la probabilidad de
#      titularse a tiempo de cuatro perfiles, en escala de logit y de
#      probabilidad.
perfiles <- tibble(
  perfil = c("A", "B", "C", "D"),
  nivel = factor(c("Magíster", "Magíster", "Doctorado", "Doctorado"),
                 levels = levels(titulacion$nivel)),
  horas_investigacion_sem = c(25, 25, 15, 35),
  sat_supervision = c(4, 4, 3, 5),
  beca = factor(c("Con beca", "Sin beca", "Sin beca", "Con beca"),
                levels = levels(titulacion$beca)),
  area = factor(c("Ciencias Naturales", "Ciencias Naturales", "Ingeniería", "Ciencias Sociales"),
                levels = levels(titulacion$area)),
  edad_ingreso = c(30, 30, 38, 28)
)

perfiles |>
  mutate(logit = predict(mf, newdata = perfiles),
         prob  = predict(mf, newdata = perfiles, type = "___"))

# Pregunta: los perfiles A y B solo difieren en la beca. ¿Cuántos puntos
#           porcentuales cambia la probabilidad? El odds ratio de la beca es el
#           mismo (2,5) para todos los perfiles: ¿por qué entonces la
#           diferencia en probabilidad depende de dónde se parta?
# Mi respuesta:


# 2.6  Comparar modelos. Ajusta un modelo intermedio y compara los tres con
#      devianza, AIC, BIC y pseudo-R² de McFadden. Después, contrasta los
#      modelos anidados con la razón de verosimilitud.
m3v <- glm(titulo_a_tiempo ~ horas_investigacion_sem + nivel + beca,
           data = titulacion, family = binomial)

modelos <- list(M1 = m1, M2 = m3v, M3 = mf)

tibble(modelo   = names(modelos),
       devianza = map_dbl(modelos, deviance),
       AIC      = map_dbl(modelos, AIC),
       BIC      = map_dbl(modelos, BIC),
       mcfadden = 1 - devianza / m1$null.deviance)

anova(m1, mf, test = "___")
anova(m3v, mf, test = "Chisq")

# Pregunta: ¿qué modelo elige el AIC y cuál el BIC? ¿Qué concluyen las pruebas
#           de razón de verosimilitud? ¿Cómo interpretas un McFadden de 0,15?
# Mi respuesta:


# 2.7  Guardar tu trabajo. Guarda el script (Ctrl + S). En la TERMINAL:
#   git add R/sesion6.R
#   git commit -m "Agrega modelo multiple, odds ratios y comparacion de modelos"


# Bloque 3 · Evaluar el modelo y cuidados (15 min) -----------------------------


# 3.1  Matriz de confusión. Calcula la probabilidad predicha para cada
#      egresado, clasifícalo con un umbral y cruza la predicción con lo que
#      ocurrió realmente.
titulacion <- titulacion |>
  mutate(p_hat = predict(mf, type = "response"),
         pred  = if_else(p_hat >= ___, 1, 0))

tabla <- table(real = titulacion$titulo_a_tiempo,
               prediccion = titulacion$pred)
tabla

# Pregunta: ¿cuántos verdaderos positivos, falsos positivos, falsos negativos y
#           verdaderos negativos hay?
# Mi respuesta:


# 3.2  Sensibilidad, especificidad y exactitud. Calcúlalas a partir de la tabla
#      y compara la exactitud con la de un «modelo» que siempre dijera que
#      nadie se titula a tiempo.
VN <- tabla["0", "0"]; FP <- tabla["0", "1"]
FN <- tabla["1", "0"]; VP <- tabla["1", "1"]

sensibilidad  <- VP / (VP + ___)
especificidad <- VN / (VN + FP)
exactitud     <- (VP + VN) / sum(tabla)
tasa_base     <- 1 - mean(titulacion$titulo_a_tiempo)

c(sensibilidad = sensibilidad, especificidad = especificidad,
  exactitud = exactitud, tasa_base = tasa_base)

# Pregunta: ¿cuánto valen las tres medidas? ¿Es muy buena la exactitud al
#           compararla con la tasa base?
# Mi respuesta:


# 3.3  El umbral es una decisión. Calcula las tres medidas para varios
#      umbrales.
real <- titulacion$titulo_a_tiempo == 1

map(c(0.3, 0.4, 0.5, 0.6), \(u) {
  pred <- titulacion$p_hat >= u
  tibble(umbral        = u,
         sensibilidad  = mean(pred[real]),
         especificidad = mean(!pred[!___]),
         exactitud     = mean(pred == real))
}) |>
  list_rbind()

# Pregunta: ¿qué pasa con la sensibilidad y la especificidad al subir el
#           umbral? ¿Qué umbral elegirías si fuera más grave dejar pasar a
#           alguien que sí se titularía a tiempo que alertar de más?
# Mi respuesta:


# 3.4  Curva ROC y AUC. Evalúa el modelo completo sin depender de un umbral y
#      compáralo con el de solo horas.
roc_f <- roc(titulacion$titulo_a_tiempo, titulacion$p_hat)
roc_h <- roc(titulacion$titulo_a_tiempo, fitted(m1))

auc(roc_f)
ci.auc(roc_f)
auc(___)

ggroc(list(completo = roc_f, solo_horas = roc_h), legacy.axes = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  labs(x = "1 - especificidad", y = "Sensibilidad", colour = "Modelo")

# Pregunta: ¿cuánto vale el AUC de cada modelo y cuál es el intervalo del
#           completo? ¿Por qué ese AUC es optimista?
# Mi respuesta:


# 3.5  Cuidados. Revisa la multicolinealidad, los datos influyentes y los
#      eventos por variable, igual que en la sesión 5.
vif(___)

a <- augment(mf)
max(a$.cooksd)
which.max(a$.cooksd)
sum(a$.cooksd > 4 / nrow(a))

sum(titulacion$titulo_a_tiempo) / (length(coef(mf)) - 1)

# Pregunta: ¿cuánto vale el VIF máximo (mira la columna adecuada para `area`)?
#           ¿Qué dice la distancia de Cook? ¿Cuántos eventos hay por
#           coeficiente y es suficiente?
# Mi respuesta:


# 3.6  Calibración (opcional). Agrupa a los egresados en diez grupos según su
#      probabilidad predicha y compara la probabilidad promedio predicha con la
#      proporción observada.
titulacion |>
  mutate(decil = ntile(p_hat, 10)) |>
  group_by(decil) |>
  summarise(predicha = mean(p_hat), observada = mean(___))

# Pregunta: ¿las probabilidades predichas se parecen a las observadas? ¿Qué tan
#           lejos queda el peor grupo?
# Mi respuesta:


# 3.7  Guardar tu trabajo. Guarda el script (Ctrl + S). En la TERMINAL:
#   git add R/sesion6.R
#   git commit -m "Agrega evaluacion del modelo logistico: matriz, ROC y cuidados"


# Retos (si te sobra tiempo) ---------------------------------------------------


# Reto 1 · ¿El efecto de las horas depende del nivel?. Agrega la interacción
#          `horas_investigacion_sem * nivel` al modelo completo, contrástala
#          con la razón de verosimilitud y, si te parece justificada, dibuja
#          las probabilidades predichas según horas para cada nivel.
# (Escribe tu código aquí.)


# Reto 2 · Comparar dos curvas ROC. Con `roc.test()` (prueba de DeLong),
#          contrasta formalmente el AUC del modelo de solo horas con el del
#          modelo completo. ¿La diferencia es significativa?
# (Escribe tu código aquí.)


# Reto 3 · Una función de evaluación. Escribe `evaluar_logit(modelo, datos,
#          umbral = 0.5)`, que devuelva en una fila la sensibilidad, la
#          especificidad, la exactitud y el AUC del modelo sobre `datos`.
#          Aplícala a `m1` y a `mf`.
# (Escribe tu código aquí.)


# Cierre (5 min) ---------------------------------------------------------------

# Revisa tu lista (cambia `[ ]` por `[x]`):

#   [ ] Escribí mi código en `R/sesion6.R` y revisé evento y referencia antes
#       de interpretar.
#   [ ] Sé pasar de un coeficiente a un odds ratio con su intervalo, y a una
#       frase.
#   [ ] Distingo un odds ratio de un riesgo relativo y de una diferencia de
#       probabilidades.
#   [ ] Obtengo probabilidades predichas con `predict(type = "response")` para
#       perfiles concretos.
#   [ ] Evalúo un modelo logístico con matriz de confusión, ROC y la tasa base,
#       y conozco sus cuidados.
#   [ ] `git log --oneline` muestra mis commits de hoy.


# TAREA PARA LA PRÓXIMA SESIÓN (Reportes reproducibles avanzados):
#   1. Termina los retos que te falten.

#   2. Deja `R/sesion6.R` guardado y con commit.

#   3. Verifica que Quarto está instalado (en la terminal: `quarto --version`)
#      y que tienes una cuenta de GitHub con la que puedas iniciar sesión.

#   4. Trae una variable de tu propia investigación con dos categorías (sí/no)
#      que te gustaría explicar con otras variables, y anota qué variables
#      crees que podrían influir.
