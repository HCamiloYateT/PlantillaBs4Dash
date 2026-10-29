# Descargar una sola aplicación sin sobrescribir proyectos existentes ----
DescargarPlantillaBs4Dash <- function(tipo = c("dashboard", "single"), destino = getwd(),
                                     repo = "https://github.com/HCamiloYateT/PlantillaBs4Dash.git", ref = "main") {
  tipo <- match.arg(tipo)
  git <- Sys.which("git")
  if (!nzchar(git)) stop("Git no está instalado o no está disponible en PATH.", call. = FALSE)
  for (value in list(destino, repo, ref)) {
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
      stop("destino, repo y ref deben ser textos no vacíos.", call. = FALSE)
    }
  }
  if (!dir.exists(destino)) stop("El directorio de destino debe existir.", call. = FALSE)
  destino <- normalizePath(destino, winslash = "/", mustWork = TRUE)
  folder <- if (tipo == "dashboard") "APP" else "APP_SINGLE"
  target <- file.path(destino, folder)
  occupied <- function() file.exists(target) || (!is.na(Sys.readlink(target)) && nzchar(Sys.readlink(target)))
  if (occupied()) stop("El destino ya existe: ", target, call. = FALSE)
  tmp <- tempfile("plantilla-")
  stage <- tempfile(".plantilla-", tmpdir = destino)
  on.exit({ unlink(tmp, recursive = TRUE); unlink(stage, recursive = TRUE) }, add = TRUE)
  run_git <- function(args) {
    result <- suppressWarnings(system2(git, vapply(args, shQuote, character(1)), stdout = TRUE, stderr = TRUE))
    status <- attr(result, "status")
    if (!is.null(status) && status != 0L) stop("Falló Git: ", paste(result, collapse = "\n"), call. = FALSE)
    invisible(result)
  }
  run_git(c("clone", "--depth=1", "--filter=blob:none", "--sparse", "--no-checkout", "--single-branch",
            "--branch", ref, "--", repo, tmp))
  run_git(c("-C", tmp, "sparse-checkout", "set", "--no-cone", "--", paste0("/", folder, "/")))
  run_git(c("-C", tmp, "checkout"))
  source <- file.path(tmp, folder)
  if (!file.exists(file.path(source, "global.R")) || !file.exists(file.path(source, "server.R"))) {
    stop("La referencia no contiene una aplicación completa: ", folder, call. = FALSE)
  }
  if (!dir.create(stage)) stop("No se pudo preparar el destino temporal.", call. = FALSE)
  files <- list.files(source, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  if (!all(file.copy(files, stage, recursive = TRUE, copy.mode = TRUE))) {
    stop("No se pudo copiar la aplicación.", call. = FALSE)
  }
  # La carpeta temporal se publica solo cuando la copia termina; no borra un destino previo.
  if (occupied()) stop("El destino apareció durante la descarga: ", target, call. = FALSE)
  if (!file.rename(stage, target)) stop("No se pudo publicar la aplicación en: ", target, call. = FALSE)
  message("Plantilla disponible en: ", target)
  invisible(target)
}
