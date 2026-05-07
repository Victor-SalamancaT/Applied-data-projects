# ================================================================
# mod_home.R - Resumen general y descomposición de serie de tiempo
# ================================================================

mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bs4ValueBoxOutput(ns("total_registros")),
    bs4ValueBoxOutput(ns("promedio_pago")),
    bs4ValueBoxOutput(ns("outliers_detectados")),
    box(width = 12, title = "Descomposición de la serie de tiempo",
        color = "primary", solidHeader = TRUE,
        withSpinner(plotOutput(ns("plot_descomposicion"))))
  )
}

mod_home_server <- function(id, data_reactiva) {
  moduleServer(id, function(input, output, session) {
    
    output$total_registros <- renderValueBox({
      datos <- data_reactiva()
      req(datos)
      bs4ValueBox(
        value = nrow(datos$datos),
        subtitle = "Registros totales",
        icon = icon("database"),
        color = "primary"
      )
    })
    
    output$promedio_pago <- renderValueBox({
      datos <- data_reactiva()
      req(datos)
      bs4ValueBox(
        value = round(mean(datos$pagos_semana$Total_Pagado), 2),
        subtitle = "Pago promedio semanal",
        icon = icon("dollar-sign"),
        color = "info"
      )
    })
    
    output$plot_descomposicion <- renderPlot({
      datos <- data_reactiva()
      req(datos)
      ts_data <- ts(datos$pagos_semana$Total_Pagado, frequency = 52)
      autoplot(decompose(ts_data)) +
        labs(title = "Descomposición de la serie de tiempo") +
        theme_minimal()
    })
  })
}
