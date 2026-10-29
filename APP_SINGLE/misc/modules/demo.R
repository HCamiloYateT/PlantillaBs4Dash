# Módulo de demostración sin servicios externos ----
.demo_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("indicadores")),
    racafeForms::racafePanel("Información general",
      racafeForms::racafeInfoGrid(
        racafeForms::racafeInfoItem("Fuente", "Datos simulados en memoria"),
        racafeForms::racafeInfoItem("Formato", "1,234.56"))),
    racafeForms::racafePanel("Formulario de demostración",
      racafeForms::racafeForm(id = "form", ns = ns,
        racafeForms::racafeFormRow(
          racafeForms::racafeInputText("nombre", "Nombre", value = "Solicitud de prueba", ns = ns),
          racafeForms::racafeInputNumeric("importe", "Importe", value = 1234.56, format = "number", ns = ns)),
        racafeForms::racafeButton("consultar", "Consultar", ns = ns),
        racafeForms::racafeButton("guardar", "Guardar en memoria", ns = ns),
        racafeForms::racafeButton("fallar", "Probar error controlado", ns = ns)),
      shiny::textOutput(ns("resultado")),
      shiny::tags$div(id = ns("panel"), class = "template-programmatic-panel",
        shiny::p("El guardado usa un objeto loader limitado a este panel."))),
    racafeForms::racafePanel("Convención numérica", shiny::tableOutput(ns("numeros"))))
}
.demo_server <- function(id, actualizacion) {
  shiny::moduleServer(id, function(input, output, session) {
    resultado <- shiny::reactiveVal("Sin operaciones")
    guardados <- shiny::reactiveVal(0L)
    loader <- racafeShiny::racafeLoader(session, type = "guardar", text = "Guardando información…", id = "panel")
    # Los datos de cálculo permanecen numéricos; el formato se aplica solo en outputs.
    datos <- shiny::reactive({
      shiny::req(is.numeric(input$importe), length(input$importe) == 1L, is.finite(input$importe))
      list(nombre = input$nombre, importe = input$importe, actualizacion = actualizacion())
    })
    output$indicadores <- shiny::renderUI({
      racafeForms::racafeMetricGrid(
        racafeForms::racafeMetric(racafeShiny::ObtenerFormato("numero")(datos()$importe), "Importe"),
        racafeForms::racafeMetric(as.character(guardados()), "Guardados en esta sesión"),
        racafeForms::racafeMetric(as.character(actualizacion()), "Actualizaciones"))
    })
    output$resultado <- shiny::renderText(resultado())
    output$numeros <- shiny::renderTable({
      valores <- c(1234, 1234.56, 1234567.89, -1234567.89, .125)
      data.frame(Presentación = vapply(valores, function(valor) {
        format(valor, digits = 15, big.mark = ",", decimal.mark = ".", scientific = FALSE, trim = TRUE)
      }, character(1)), check.names = FALSE)
    }, striped = TRUE, bordered = FALSE)
    shiny::observeEvent(input$consultar, {
      valor <- datos()$importe * 2
      resultado(paste("Resultado:", racafeShiny::ObtenerFormato("numero")(valor)))
      racafeForms::racafeNotify(session, "Consulta terminada", "Los valores reactivos siguen siendo numéricos.",
                                type = "success")
    })
    save_local <- function(fail = FALSE) {
      on.exit(loader$hide(), add = TRUE)
      if (fail) stop("Error de demostración")
      guardados(guardados() + 1L)
      resultado(paste("Guardado en memoria:", datos()$nombre))
    }
    schedule_save <- function(fail = FALSE) {
      loader$show()
      # La pausa de demo devuelve el control para que el navegador pinte el waiter.
      # No convierte cálculos pesados en trabajo paralelo ni simula progreso.
      later::later(function() {
        if (session$isClosed()) return(invisible(NULL))
        shiny::withReactiveDomain(session, shiny::isolate({
          tryCatch({
            save_local(fail)
            racafeForms::racafeNotify(session, "Registro guardado", type = "success")
          }, error = function(error) {
            resultado("Error controlado; aplicación disponible")
            racafeForms::racafeNotify(session, conditionMessage(error), type = "error")
          }, finally = loader$hide())
        }))
      }, delay = .8)
    }
    shiny::observeEvent(input$guardar, schedule_save())
    shiny::observeEvent(input$fallar, schedule_save(fail = TRUE))
  })
}
