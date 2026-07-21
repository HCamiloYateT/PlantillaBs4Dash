# Activa el entorno reproducible cuando renv ya fue inicializado.
renv_activate <- file.path("renv", "activate.R")

if (file.exists(renv_activate)) {
  source(renv_activate)
} else if (interactive()) {
  message(
    "renv no esta inicializado. Ejecuta renv::init() y versiona renv.lock ",
    "y renv/activate.R."
  )
}

rm(renv_activate)
