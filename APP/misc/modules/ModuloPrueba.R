ModuloPruebaUI <- function(id){
  ns <- NS(id)
  tagList(
    h4("Tabla de Prueba"),
    gt_output(ns("tabla_prueba"))
  )

}
ModuloPrueba <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$tabla_prueba <- render_gt({
      iris
    })

  })
}

### App de prueba ----
ui <- bs4DashPage(
  title = "Prueba ResumenTotal",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(),
  body = bs4DashBody(useShinyjs(),
                     ModuloPruebaUI("resumen")
  )
)

server <- function(input, output, session) {
  ModuloPrueba("resumen")
}

shinyApp(ui, server)

