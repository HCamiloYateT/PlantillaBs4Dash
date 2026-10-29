body <- bs4Dash::bs4DashBody(
  .assets,
  shiny::includeCSS("www/template.css"),
  racafeShiny::racafeLoaderUI(startup = FALSE, busy = TRUE,
    triggers = c(actualizar = "BTN_Actualizar", consultar = "demo-consultar",
                 guardar = "demo-guardar", validar = "demo-fallar")),
  bs4Dash::bs4TabItems(
    bs4Dash::bs4TabItem(tabName = "TablaGeneral", shiny::tags$main(class = "template-main",
      shiny::h1("Tabla de importaciones"), shiny::textOutput("ultima_actualizacion"), .demo_ui("demo"))),
    bs4Dash::bs4TabItem(tabName = "SerieTiempo", shiny::tags$main(class = "template-main",
      shiny::h1("Serie de tiempo"), racafeForms::racafePanel("Histórico",
        shiny::p("Añada aquí su módulo de series de tiempo; no se consultan servicios externos."))))))
