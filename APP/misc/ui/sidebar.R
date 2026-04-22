sidebar <- bs4DashSidebar(status = "danger", expandOnHover = FALSE,
                          bs4SidebarMenu(id = "menu_principal",
                                         bs4SidebarMenuItem("Tabla Importaciones", icon = icon("truck-fast"), selected = FALSE,
                                                            bs4SidebarMenuSubItem("Importaciones", tabName = "TablaGeneral", icon = icon("list"))
                                         ),
                                         bs4SidebarMenuItem("Serie de Tiempo", icon = icon("chart-line"),
                                                            bs4SidebarMenuSubItem("Histórico", tabName = "SerieTiempo", icon = icon("list"))
                                                            )
                                         )
                          )
