# Badge de entorno y pie corporativo ----
.badge_catalog <- c(prototipo = "PROTOTIPO", pruebas = "PRUEBAS", staging = "STAGING", produccion = "PRODUCCIÓN",
                    demo = "DEMO", mantenimiento = "MANT.", ninguno = "")
app_badge <- function(estado = badge_estado) {
  key <- tolower(trimws(estado))
  if (!key %in% names(.badge_catalog)) key <- "prototipo"
  shiny::tags$span(class = paste("app-badge", paste0("app-badge--", key)), .badge_catalog[[key]])
}
footer <- bs4Dash::bs4DashFooter(left = app_badge(),
  right = shiny::tags$img(src = .logo_url, alt = "Racafé", height = "30"), fixed = TRUE)
