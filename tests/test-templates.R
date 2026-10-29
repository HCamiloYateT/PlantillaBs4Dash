library(testthat)
source("utils/descargar_plantilla.R")

test_that("ambas plantillas se analizan e identifican grupos correctamente", {
  for (app in c("APP", "APP_SINGLE")) {
    files <- list.files(app, pattern = "[.]R$", full.names = TRUE, recursive = TRUE)
    for (file in files) expect_silent(parse(file))
    env <- new.env()
    sys.source(file.path(app, "misc", "functions.R"), env)
    for (empty in list(NULL, character(), c(NA_character_, "", " "))) {
      expect_equal(env$.template_groups(empty), "ANALÍTICA")
    }
    expect_equal(env$.template_groups(c("analítica", " ventas ", "ANALÍTICA", NA)), "ANALÍTICA · VENTAS")
    expect_equal(env$.template_groups(123), "123")
    values <- c(1234, 1234.56, 1234567.89, -1234567.89, .125)
    shown <- vapply(values, function(value) {
      format(value, digits = 15, big.mark = ",", decimal.mark = ".", scientific = FALSE, trim = TRUE)
    }, character(1))
    expect_equal(shown, c("1,234", "1,234.56", "1,234,567.89", "-1,234,567.89", "0.125"))
    expect_true(is.numeric(values))
  }
})

test_that("la descarga preserva proyectos y copia solo la aplicación elegida", {
  fixture <- tempfile("repo-")
  dir.create(fixture)
  on.exit(unlink(fixture, recursive = TRUE), add = TRUE)
  for (app in c("APP", "APP_SINGLE")) {
    dir.create(file.path(fixture, app))
    for (file in c("global.R", "ui.R", "server.R")) writeLines("# fixture", file.path(fixture, app, file))
    writeLines("oculto", file.path(fixture, app, ".gitkeep"))
  }
  writeLines("no descargar", file.path(fixture, "otro.txt"))
  git <- function(args) {
    result <- system2("git", vapply(c("-C", fixture, args), shQuote, character(1)), stdout = TRUE, stderr = TRUE)
    status <- attr(result, "status")
    if (!is.null(status) && status != 0) stop(paste(result, collapse = "\n"))
  }
  git(c("init", "-b", "main"))
  git(c("add", "."))
  git(c("-c", "user.name=Pruebas", "-c", "user.email=tests@example.invalid", "commit", "-m", "fixture"))
  destination <- tempfile("destino con espacios-")
  dir.create(destination)
  on.exit(unlink(destination, recursive = TRUE), add = TRUE)
  for (type in c("dashboard", "single")) {
    app <- if (type == "dashboard") "APP" else "APP_SINGLE"
    path <- DescargarPlantillaBs4Dash(type, destination, repo = fixture)
    expect_equal(path, file.path(destination, app))
    expect_true(file.exists(file.path(path, ".gitkeep")))
    expect_true(all(file.exists(file.path(path, c("global.R", "ui.R", "server.R")))))
    expect_false(file.exists(file.path(path, ".git")))
    expect_false(file.exists(file.path(path, "otro.txt")))
    writeLines("conservar", file.path(path, "usuario.txt"))
    expect_error(DescargarPlantillaBs4Dash(type, destination, repo = fixture), "ya existe")
    expect_equal(readLines(file.path(path, "usuario.txt")), "conservar")
  }
  fresh <- tempfile(); dir.create(fresh)
  on.exit(unlink(fresh, recursive = TRUE), add = TRUE)
  expect_error(DescargarPlantillaBs4Dash("single", fresh, repo = file.path(fixture, "no-existe")), "Falló Git")
  expect_length(list.files(fresh, all.files = TRUE, no.. = TRUE), 0)
  expect_error(DescargarPlantillaBs4Dash(destino = file.path(fresh, "no-existe")), "debe existir")
})
