library(tidyverse)
library(here)

programas <- read_csv(here("data", "raw", "programas.csv"))

renderizar_informe <- function(programa_id, incluir_modelo = TRUE) {
  args <- c("render", shQuote(here("informes", "informe_programa.qmd")),
            "-P", str_glue("programa_id:{programa_id}"),
            "-P", str_glue("incluir_modelo:{tolower(incluir_modelo)}"),
            "-M", str_glue("output-file:informe_{programa_id}.html"))
  estado <- system2("quarto", args, stdout = FALSE, stderr = FALSE)
  if (estado != 0) warning("Falló el informe de ", programa_id, call. = FALSE)
  invisible(estado)
}

walk(programas$programa_id, renderizar_informe)

# Página de inicio con los enlaces a todos los informes
system2("quarto", c("render", shQuote(here("index.qmd"))), stdout = FALSE, stderr = FALSE)
