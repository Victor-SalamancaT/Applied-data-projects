# ================================================================
# mod_outliers.R - Detección de outliers coherente por grupo
# ================================================================

mod_outliers_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(width = 3, title = "Filtros", color = "primary", solidHeader = TRUE,
          uiOutput(ns("filtros_outliers"))
      ),
      box(width = 9, title = "Outliers detectados (IQR)", color = "danger",
          solidHeader = TRUE,
          withSpinner(DTOutput(ns("tabla_outliers")))
      )
    ),
    fluidRow(
      box(width = 12,
          title = "Distribución de valores por variable seleccionada",
          color = "info", solidHeader = TRUE,
          withSpinner(plotOutput(ns("plot_boxplot")))
      )
    ),
    fluidRow(
      bs4ValueBoxOutput(ns("total_outliers")),
      bs4ValueBoxOutput(ns("promedio_periodo")),
      bs4ValueBoxOutput(ns("total_pagado_periodo"))
    )
  )
}

mod_outliers_server <- function(id, data_reactiva) {
  moduleServer(id, function(input, output, session) {
    
    # ---------- Filtros ----------
    output$filtros_outliers <- renderUI({
      datos <- data_reactiva()
      req(datos)
      
      años  <- sort(unique(format(datos$datos$Fecha_Pago, "%Y")))
      meses <- sort(unique(format(datos$datos$Fecha_Pago, "%m")))
      
      tagList(
        selectInput(session$ns("anio"), "Selecciona el año:",
                    choices = años, selected = tail(años, 1)),
        selectInput(session$ns("mes"), "Selecciona el mes:",
                    choices = meses, selected = tail(meses, 1)),
        selectInput(session$ns("variable_agrupacion"),
                    "Variable de agrupación:",
                    choices = c("Sin agrupación",
                                "Usuario",
                                "Plataforma",
                                "Daño",
                                "Nombre_Concesionaria"),
                    selected = "Sin agrupación")
      )
    })
    
    # ---------- Datos filtrados ----------
    datos_filtrados <- reactive({
      req(data_reactiva(), input$anio, input$mes)
      
      data_reactiva()$datos %>%
        filter(format(Fecha_Pago, "%Y") == input$anio,
               format(Fecha_Pago, "%m") == input$mes)
    })
    
    # ---------- Outliers coherentes ----------
    outliers_detectados <- reactive({
      df <- datos_filtrados()
      req(df)
      
      if (input$variable_agrupacion == "Sin agrupación") {
        
        q1 <- quantile(df$TOTALES_PAGADOS_TOTAL, 0.25, na.rm = TRUE)
        q3 <- quantile(df$TOTALES_PAGADOS_TOTAL, 0.75, na.rm = TRUE)
        iqr <- q3 - q1
        
        df %>%
          filter(TOTALES_PAGADOS_TOTAL < (q1 - 1.5 * iqr) |
                   TOTALES_PAGADOS_TOTAL > (q3 + 1.5 * iqr))
        
      } else {
        
        df %>%
          group_by(.data[[input$variable_agrupacion]]) %>%
          mutate(
            q1 = quantile(TOTALES_PAGADOS_TOTAL, 0.25, na.rm = TRUE),
            q3 = quantile(TOTALES_PAGADOS_TOTAL, 0.75, na.rm = TRUE),
            iqr = q3 - q1
          ) %>%
          filter(TOTALES_PAGADOS_TOTAL < (q1 - 1.5 * iqr) |
                   TOTALES_PAGADOS_TOTAL > (q3 + 1.5 * iqr)) %>%
          ungroup()
      }
    })
    
    # ---------- Tabla ----------
    output$tabla_outliers <- renderDT({
      datatable(
        outliers_detectados() %>%
          select(`#`, Nombre_Concesionaria, Plataforma, Daño,
                 Pago, Usuario, PRES_UTI_TOTAL, TOTALES_PAGADOS_TOTAL),
        options = list(scrollX = TRUE, pageLength = 5),
        rownames = FALSE
      )
    })
    
    # ---------- Boxplot ----------
    output$plot_boxplot <- renderPlot({
      df <- datos_filtrados()
      req(df)
      
      if (input$variable_agrupacion == "Sin agrupación") {
        ggplot(df, aes(y = TOTALES_PAGADOS_TOTAL)) +
          geom_boxplot(fill = "#0b7890", outlier.color = "red") +
          theme_minimal() +
          labs(title = "Distribución general de pagos",
               y = "Total Pagado")
      } else {
        ggplot(df, aes_string(x = input$variable_agrupacion,
                              y = "TOTALES_PAGADOS_TOTAL")) +
          geom_boxplot(fill = "#0b7890", outlier.color = "red") +
          theme_minimal() +
          labs(title = paste("Distribución de pagos por",
                             input$variable_agrupacion),
               x = input$variable_agrupacion,
               y = "Total Pagado") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
    })
    
    # ---------- Indicadores ----------
    output$total_outliers <- renderValueBox({
      bs4ValueBox(
        value = nrow(outliers_detectados()),
        subtitle = "Outliers detectados",
        icon = icon("exclamation-triangle"),
        color = ifelse(nrow(outliers_detectados()) > 0, "danger", "success")
      )
    })
    
    output$promedio_periodo <- renderValueBox({
      df <- datos_filtrados()
      bs4ValueBox(
        round(mean(df$TOTALES_PAGADOS_TOTAL, na.rm = TRUE), 2),
        "Promedio del periodo",
        icon = icon("chart-line"),
        color = "info"
      )
    })
    
    output$total_pagado_periodo <- renderValueBox({
      df <- datos_filtrados()
      bs4ValueBox(
        round(sum(df$TOTALES_PAGADOS_TOTAL, na.rm = TRUE), 2),
        "Total pagado en el periodo",
        icon = icon("dollar-sign"),
        color = "primary"
      )
    })
  })
}
