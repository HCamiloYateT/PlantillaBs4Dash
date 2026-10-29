body <- bs4Dash::bs4DashBody(
  .assets,
  shiny::includeCSS("www/template.css"),
  racafeShiny::racafeLoaderUI(startup = FALSE, busy = TRUE,
    triggers = c(actualizar = "BTN_Actualizar", consultar = "demo-consultar",
                 guardar = "demo-guardar", validar = "demo-fallar")),
  shiny::tags$main(class = "template-main",
    shiny::h1(tit_app), shiny::p("Formulario corporativo y resultados locales, sin navegación lateral."),
    shiny::textOutput("ultima_actualizacion"), .demo_ui("demo")))
