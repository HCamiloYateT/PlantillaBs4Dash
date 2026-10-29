sidebar <- bs4Dash::bs4DashSidebar(status = "danger", expandOnHover = FALSE,
  bs4Dash::bs4SidebarMenu(id = "menu_principal",
    bs4Dash::bs4SidebarMenuItem("Tabla Importaciones", icon = shiny::icon("truck-fast"),
      bs4Dash::bs4SidebarMenuSubItem("Importaciones", tabName = "TablaGeneral", selected = TRUE)),
    bs4Dash::bs4SidebarMenuItem("Serie de Tiempo", icon = shiny::icon("chart-line"),
      bs4Dash::bs4SidebarMenuSubItem("Histórico", tabName = "SerieTiempo"))))
