# Layout bs4Dash; recursos externos opcionales desactivados ----
ui <- bs4Dash::bs4DashPage(title = tit_app, dark = FALSE, scrollToTop = TRUE,
  preloader = racafeShiny::racafePreloader(type = "inicio", text = "Cargando aplicación…"),
  header = header, sidebar = sidebar, body = body, controlbar = controlbar, footer = footer)
shiny::tagList(htmltools::suppressDependencies("ionicons", "googlefonts"), ui)
