header <- bs4Dash::bs4DashNavbar(status = "white", border = FALSE,
  title = bs4Dash::dashboardBrand(title = tit_app, href = "#", image = .logo_url),
  leftUi = shiny::tags$li(class = "dropdown template-user",
    shiny::uiOutput("user", inline = TRUE), shiny::uiOutput("group", inline = TRUE),
    racafeShiny::Boton("BTN_Actualizar", label = NULL, icono = "sync", size = "xxs",
                       label_posicion = "below", titulo = "Actualizar")),
  rightUi = shiny::tags$li(class = "dropdown template-date", format_fecha_es(Sys.Date())))
# Solo se retiran los toggles internos; usuario y actualización permanecen en el navbar.
header <- htmltools::tagQuery(header)$find(".nav-item")$remove()$allTags()
