ModuloTablaPaisUI <- function(id){
  ns <- NS(id)
  tagList(
    h4("Tabla de Prueba"),
    gt_output(ns("TablaPais"))
  )

}
ModuloTablaPais <- function(id, dat) {
  moduleServer(id, function(input, output, session) {

    output$TablaPais <- render_gt({

      t1 <- dat() %>%
        group_by(Importador, Periodo = year(as.Date(Fecha))) %>%
        summarise(Sacos70 = sum(Total) / 70 ) %>%
        arrange(Periodo) %>%
        pivot_wider(names_from = Periodo, values_from = Sacos70)

      t1 %>%
        gt() %>%
        gt_minimal_style()


    })



  })
}
