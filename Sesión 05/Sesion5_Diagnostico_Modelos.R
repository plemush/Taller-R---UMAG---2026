# ==============================================================================
# Práctica — Sesión 5: Diagnóstico de modelos
# Análisis de Datos en R · Nivel Básico-Intermedio · Duración: 60 minutos
# ==============================================================================
#
# CÓMO TRABAJAR ESTE ARCHIVO
#
# Este archivo es TU SCRIPT DE TRABAJO: lo vas a completar directamente aquí.
# En el paso 0.4 lo guardas como R/sesion5.R dentro de tu proyecto.
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
#   * Regla de la sesión: primero el gráfico, después la prueba. Una prueba sin
#     gráfico puede engañarte en cualquier dirección.
#   * Tip RStudio: abre el panel "Outline" (Ctrl + Shift + O) para navegar entre bloques.
#   * Si ves caracteres raros en las tildes: File > Reopen with Encoding > UTF-8.
#
# BLOQUES DE LA PRÁCTICA
#
#   Bloque 0 · Preparación .................................... 5 min
#   Bloque 1 · Residuos, linealidad y normalidad .............. 15 min
#   Bloque 2 · Varianza constante y transformación logarítmica  15 min
#   Bloque 3 · Multicolinealidad e influencia ................. 20 min
#   Cierre y dudas ............................................ 5 min
#
# DATOS (todos simulados, no corresponden a personas ni proyectos reales)
#
#   egresados.csv   el de la sesión 4: diagnosticaremos su modelo completo (m3).
#   proyectos.csv   (NUEVO) 178 proyectos de investigación: monto_mm (millones de
#                   pesos), n_investigadores, n_estudiantes, horas_equipo_sem,
#                   publicaciones_previas, duracion_meses, area y otras columnas.
#   proyectos_atipicos.csv  (NUEVO) dos proyectos extra para el Bloque 3.
#   programas.csv   los 6 programas con su área (de sesiones anteriores).
#
# REQUISITOS
#
#   R >= 4.1, RStudio, el proyecto "curso-r-intermedio" (con su repositorio de Git)
#   y los paquetes tidyverse, here, broom, patchwork, car, lmtest y sandwich.
# ==============================================================================


# Bloque 0 · Preparación (5 min) -----------------------------------------------


# 0.1  Abre el proyecto `curso-r-intermedio` en RStudio. En la terminal, `git
#      status` debería indicar que no hay cambios pendientes.


# 0.2  Copia `proyectos.csv` y `proyectos_atipicos.csv` a `data/raw/`
#      (`egresados.csv` ya debería estar).


# 0.3  Instala los paquetes nuevos (solo una vez), en la consola:
install.packages(c("car", "lmtest", "sandwich"))


# 0.4  Crea un script nuevo, guárdalo como `R/sesion5.R` y escribe al comienzo:
library(tidyverse)
library(here)
library(broom)
library(patchwork)
library(car)
library(lmtest)
library(sandwich)

egresados <- read_csv(here("data", "raw", "egresados.csv")) |>
  mutate(nivel = factor(nivel, levels = c("Doctorado", "Magíster")),
         beca  = factor(beca,  levels = c("Con beca", "Sin beca")),
         area  = factor(area))

# El modelo completo de la sesión 4
m3 <- lm(meses_titulacion ~ horas_investigacion_sem + nivel + sat_supervision + beca + area + edad_ingreso,
         data = egresados)

proyectos <- read_csv(here("data", "raw", "proyectos.csv")) |>
  mutate(area = factor(area))

glimpse(proyectos)
nobs(m3)

# Regla de trabajo para toda la práctica: primero el gráfico, después la
# prueba. Una prueba sin gráfico puede engañarte en cualquier dirección.


# Bloque 1 · Residuos, linealidad y normalidad (15 min) ------------------------


# 1.1  Los residuos con `augment()`. Obtén una tabla con los residuos del
#      modelo completo de egresados y cuenta cuántos caen fuera de ±2
#      desviaciones estándar.
a <- augment(m3)
glimpse(a)

sum(abs(a$.std.resid) > ___)

# Pregunta: ¿cuántos residuos estandarizados superan 2 en valor absoluto? En
#           una muestra de 194 casos, ¿cuántos esperarías por azar (alrededor
#           del 5%)? ¿Es motivo de alarma?
# Mi respuesta:


# 1.2  Linealidad y varianza constante. Dibuja los residuos contra los valores
#      ajustados, con una línea suave.
ggplot(a, aes(___, .resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_smooth(se = FALSE)

# Pregunta: ¿la línea suave es casi plana sobre cero? ¿Ves una curva o un
#           embudo (más dispersión a la derecha que a la izquierda)?
# Mi respuesta:


# 1.3  Normalidad: gráficos. Dibuja el gráfico cuantil-cuantil de los residuos
#      estandarizados y un histograma de los residuos.
p_qq <- ggplot(a, aes(sample = ___)) +
  stat_qq(alpha = 0.6) +
  stat_qq_line()

p_hist <- ggplot(a, aes(.resid)) +
  geom_histogram(bins = ___)

p_qq + p_hist

# Pregunta: ¿los puntos siguen la diagonal? ¿Alguna cola se separa de ella?
# Mi respuesta:


# 1.4  Normalidad: prueba. Aplica la prueba de Shapiro-Wilk a los residuos.
shapiro.test(___(m3))

# Pregunta: ¿cuánto valen W y el valor p? ¿Qué concluyes? ¿Por qué esta prueba,
#           sola, es mala consejera con muestras muy grandes o muy pequeñas?
# Mi respuesta:


# 1.5  Los cuatro gráficos de R base. El diagnóstico clásico viene incluido:
#      `plot()` sobre un modelo.
par(mfrow = c(2, 2))
plot(___)
par(mfrow = c(1, 1))

# Pregunta: ¿qué supuesto revisa cada uno de los cuatro paneles? (Residuos vs
#           ajustados, cuantil-cuantil, escala-ubicación y residuos vs
#           palanca.)
# Mi respuesta:


# 1.6  Independencia (opcional). Aplica la prueba de Durbin-Watson.
durbinWatsonTest(m3)

# Pregunta: ¿el estadístico D-W está cerca de 2? ¿Qué significa eso? ¿Por qué
#           esta prueba, en estos datos, dice poco sobre la independencia real
#           (piensa en cómo se recogieron)?
# Mi respuesta:


# 1.7  Guardar tu trabajo. Guarda el script (Ctrl + S). En la TERMINAL:
#   git add R/sesion5.R
#   git commit -m "Agrega diagnostico de residuos del modelo de egresados"


# Bloque 2 · Varianza constante y transformación logarítmica (15 min) ----------


# 2.1  Explorar antes de modelar. Con los proyectos, dibuja el monto adjudicado
#      contra el número de investigadores y ajusta la regresión simple.
ggplot(proyectos, aes(n_investigadores, ___)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE)

m_simple <- lm(monto_mm ~ n_investigadores, data = proyectos)
glance(m_simple)$r.squared

# Pregunta: ¿cuánto vale R²? ¿Qué forma tiene la nube de puntos a medida que
#           crece el equipo?
# Mi respuesta:


# 2.2  Un modelo de montos. Ajusta el modelo con cuatro predictores y dibuja
#      sus residuos.
m_monto <- lm(monto_mm ~ n_investigadores + publicaciones_previas + duracion_meses + ___,
              data = proyectos)

a_m <- augment(m_monto)

p1 <- ggplot(a_m, aes(.fitted, .resid)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE)

p2 <- ggplot(a_m, aes(.fitted, sqrt(abs(.std.resid)))) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE)

p1 + p2

# Pregunta: ¿qué forma tienen los residuos? ¿Qué supuesto está en problemas?
# Mi respuesta:


# 2.3  Pruebas de varianza constante y de normalidad. Confirma con pruebas lo
#      que viste.
ncvTest(m_monto)
bptest(___)
shapiro.test(resid(m_monto))

# Pregunta: ¿cuál es la hipótesis nula de `ncvTest()`? ¿Qué decides con los
#           valores p? ¿Y qué dice Shapiro?
# Mi respuesta:


# 2.4  Transformar la respuesta. Ajusta el mismo modelo pero con el logaritmo
#      del monto como respuesta, y repite el diagnóstico.
m_log <- update(m_monto, log(___) ~ .)

a_l <- augment(m_log)

p3 <- ggplot(a_l, aes(.fitted, .resid)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE)

p4 <- ggplot(a_l, aes(sample = .std.resid)) +
  stat_qq(alpha = 0.6) +
  stat_qq_line()

p3 + p4

# Pregunta: ¿desapareció el embudo? ¿Se ve mejor el gráfico cuantil-cuantil?
# Mi respuesta:


# 2.5  Comparar antes y después. Arma una tabla con las pruebas y el R² de
#      ambos modelos.
tibble(modelo     = c("Monto crudo", "log(monto)"),
       ncv_p      = c(ncvTest(m_monto)$p, ncvTest(m_log)$p),
       shapiro_p  = c(shapiro.test(resid(m_monto))$p.value, shapiro.test(resid(m_log))$p.value),
       r2         = c(glance(m_monto)$r.squared, glance(___)$r.squared))

# Pregunta: ¿qué pruebas dejaron de rechazar tras la transformación? ¿Cambió el
#           R²? (Atención: el R² de ambos modelos no mide lo mismo, porque la
#           respuesta cambió de escala.)
# Mi respuesta:


# 2.6  Interpretar un modelo con logaritmo. El coeficiente de `log(monto)` es
#      un cambio porcentual aproximado. Conviértelo con `(exp(β) − 1) × 100`.
(exp(coef(m_log)["n_investigadores"]) - ___) * 100

(exp(confint(m_log)["n_investigadores", ]) - 1) * 100

# Pregunta: escribe una frase que interprete el coeficiente de
#           `n_investigadores`: ¿cuánto cambia el monto, en porcentaje, por
#           cada investigador adicional (con su intervalo)?
# Mi respuesta:


# 2.7  Guardar tu trabajo. Guarda el script (Ctrl + S). En la TERMINAL:
#   git add R/sesion5.R
#   git commit -m "Agrega homocedasticidad y transformacion logaritmica"


# Bloque 3 · Multicolinealidad e influencia (20 min) ---------------------------


# 3.1  Correlaciones entre variables. Mira cuáles variables del equipo miden
#      casi lo mismo.
proyectos |>
  select(monto_mm, n_investigadores, n_estudiantes, horas_equipo_sem, publicaciones_previas, duracion_meses) |>
  cor() |>
  round(___)

# Pregunta: ¿qué pares de predictores tienen correlación mayor a 0,9? ¿Tiene
#           sentido que sean tan parecidos?
# Mi respuesta:


# 3.2  Un modelo con las seis variables. Ajusta el modelo logarítmico
#      incluyendo las seis variables de tamaño y trabajo del equipo, y compara
#      el coeficiente de `n_investigadores` con el de `m_log`.
m_log6 <- lm(log(monto_mm) ~ n_investigadores + n_estudiantes + horas_equipo_sem +
               publicaciones_previas + duracion_meses + area,
             data = proyectos)

bind_rows(cuatro = tidy(m_log, conf.int = TRUE),
          seis   = tidy(m_log6, conf.int = TRUE),
          .id = "modelo") |>
  filter(term == "___") |>
  select(modelo, estimate, std.error, p.value, conf.low, conf.high)

# Pregunta: ¿qué pasó con el error estándar y con el intervalo de
#           `n_investigadores` al agregar las dos variables? ¿Cambió la
#           conclusión?
# Mi respuesta:


# 3.3  Diagnóstico con VIF. Calcula el factor de inflación de la varianza del
#      modelo de seis variables.
vif(___)

# Pregunta: ¿qué variables superan 10? Para `area`, que es categórica, ¿qué
#           columna debes mirar?
# Mi respuesta:


# 3.4  Reducir el modelo. Quita las dos variables redundantes (por teoría: el
#      número de estudiantes y las horas del equipo crecen con el tamaño del
#      equipo) y revisa el VIF.
m_red <- update(m_log6, . ~ . - n_estudiantes - ___)

vif(m_red)
all.equal(coef(m_red), coef(m_log))

# Pregunta: ¿cuánto vale ahora el VIF máximo? ¿Por qué eliminar variables por
#           teoría es mejor que eliminarlas solo porque tienen VIF alto?
# Mi respuesta:


# 3.5  Agregar dos proyectos y medir la influencia. Une los dos proyectos extra
#      a la base y ajusta de nuevo el modelo final.
atipicos <- read_csv(here("data", "raw", "proyectos_atipicos.csv"))

pa <- bind_rows(proyectos, atipicos) |>
  mutate(area = factor(area))

m_at <- update(m_red, data = ___)

a_at <- augment(m_at) |>
  mutate(fila = row_number())

a_at |>
  filter(.cooksd > 4 / nrow(a_at)) |>
  arrange(desc(.cooksd)) |>
  select(fila, .std.resid, .hat, .cooksd)

# Pregunta: ¿qué filas superan el umbral 4/n y cuáles destacan más? Revisa sus
#           valores: ¿qué tienen de particular (residuo grande, palanca alta o
#           ambas)?
# Mi respuesta:


# 3.6  Análisis de sensibilidad. Compara los coeficientes del modelo final con
#      y sin esos dos proyectos.
sens <- bind_rows(sin_2 = tidy(m_red, conf.int = TRUE),
                  con_2 = tidy(m_at, conf.int = TRUE),
                  .id = "datos") |>
  filter(term != "(Intercept)")

sens |>
  select(datos, term, estimate, p.value) |>
  pivot_wider(names_from = datos, values_from = c(estimate, ___))

shapiro.test(resid(m_red))$p.value
shapiro.test(resid(m_at))$p.value

# Pregunta: ¿qué conclusiones se mantienen aunque estén los dos proyectos y
#           cuáles cambian? ¿Qué harías antes de decidir excluirlos?
# Mi respuesta:


# 3.7  Guardar tu trabajo. Guarda el script (Ctrl + S). En la TERMINAL:
#   git add R/sesion5.R
#   git commit -m "Agrega multicolinealidad, influencia y sensibilidad"


# Retos (si te sobra tiempo) ---------------------------------------------------


# Reto 1 · Una función de diagnóstico. Escribe `diagnosticar(modelo)`, que
#          reciba un modelo y devuelva, con `patchwork`, un panel de cuatro
#          gráficos: residuos vs ajustados, cuantil-cuantil, escala-ubicación y
#          distancia de Cook por fila. Aplícala a `m3` y a `m_monto`.
# (Escribe tu código aquí.)


# Reto 2 · Errores estándar robustos. Para el modelo sin transformar
#          (`m_monto`), compara la tabla de coeficientes clásica con la que usa
#          errores estándar robustos (`coeftest()` y `vcovHC()` con `type =
#          "HC3"`). ¿Cambian los coeficientes? ¿Cambia alguna conclusión?
# (Escribe tu código aquí.)


# Reto 3 · Sensibilidad en un solo flujo. Con una lista de dos modelos (`m_red`
#          y `m_at`), `map()` y `tidy()`, arma en una sola tabla el coeficiente
#          de `n_investigadores` con su intervalo para cada modelo.
# (Escribe tu código aquí.)


# Cierre (5 min) ---------------------------------------------------------------

# Revisa tu lista (cambia `[ ]` por `[x]`):

#   [ ] Escribí mi código en `R/sesion5.R` y miré los gráficos antes de las
#       pruebas.
#   [ ] Sé qué supuesto revisa cada gráfico de diagnóstico y qué hacer si
#       falla.
#   [ ] Interpreto un coeficiente de un modelo con logaritmo como cambio
#       porcentual.
#   [ ] Distingo un dato atípico de uno influyente, y no elimino datos solo
#       porque «mejoran» el modelo.
#   [ ] `git log --oneline` muestra mis commits de hoy.


# TAREA PARA LA PRÓXIMA SESIÓN (Regresión logística):
#   1. Termina los retos que te falten.

#   2. Deja `R/sesion5.R` guardado y con commit: lo retomaremos como punto de
#      partida.

#   3. Trae una variable de tu propia investigación con dos categorías (sí/no,
#      aprueba/reprueba, ocurre/no ocurre) que te gustaría explicar con otras
#      variables, y anota qué variables crees que podrían influir.
