function(input, output, session) {
  # Estado de cada sesión ----
  actualizacion <- shiny::reactiveVal(0L)
  usuario <- shiny::reactive({
    user <- session$user
    if (is.null(user) || !length(user) || is.na(user[[1]]) || !nzchar(trimws(user[[1]]))) return("HCYATE")
    stringr::str_to_upper(trimws(as.character(user[[1]])))
  })
  grupo <- shiny::reactive(.template_groups(session$groups))
  output$user <- shiny::renderUI(shiny::tags$strong(usuario()))
  output$group <- shiny::renderUI(shiny::tags$span(class = "badge badge-secondary", grupo()))
  output$ultima_actualizacion <- shiny::renderText(paste("Actualizaciones de esta sesión:", actualizacion()))
  shiny::observeEvent(input$BTN_Actualizar, {
    actualizacion(actualizacion() + 1L)
    racafeForms::racafeNotify(session, "Información actualizada", "Datos de demostración en memoria.",
                              type = "success")
  })
  .demo_server("demo", actualizacion)
}
