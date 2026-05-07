# ================================================================
# mod_forecast.R - Pronóstico SARIMA + NNETAR
# ================================================================

# ===============================
# UI
# ===============================
mod_forecast_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 4,
        title = "Configuración del pronóstico",
        color = "primary",
        solidHeader = TRUE,
        
        sliderInput(
          ns("horizonte"),
          "Meses a predecir:",
          min = 1,
          max = 12,
          value = 3
        ),
        
        actionButton(
          ns("run"),
          "Generar predicción",
          class = "btn btn-primary btn-block"
        )
      ),
      
      box(
        width = 8,
        title = "Pronóstico de pagos",
        color = "primary",
        solidHeader = TRUE,
        withSpinner(plotOutput(ns("grafico_prediccion")))
      )
    ),
    
    fluidRow(
      bs4Dash::bs4ValueBoxOutput(ns("total_predicho")),
      bs4Dash::bs4ValueBoxOutput(ns("promedio_predicho")),
      bs4Dash::bs4ValueBoxOutput(ns("ultimo_real"))
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Valores de predicción",
        color = "info",
        solidHeader = TRUE,
        DT::DTOutput(ns("tabla_predicciones"))
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Interpretación y métricas del modelo",
        color = "success",
        solidHeader = TRUE,
        verbatimTextOutput(ns("metricas_explicadas"))
      )
    )
  )
}

# ===============================
# SERVER
# ===============================
mod_forecast_server <- function(id, data_reactiva) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$run, {
      req(data_reactiva())
      
      detalle <- data_reactiva()$datos
      req(detalle)
      
      # ===============================
      # Serie mensual
      # ===============================
      pagos_mes <- detalle %>%
        dplyr::filter(!is.na(Fecha_Pago)) %>%
        dplyr::mutate(Mes = as.Date(format(Fecha_Pago, "%Y-%m-01"))) %>%
        dplyr::group_by(Mes) %>%
        dplyr::summarise(
          Total_Pagado = sum(TOTALES_PAGADOS_TOTAL, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::arrange(Mes)
      
      validate(
        need(
          nrow(pagos_mes) >= 12,
          "Se requieren al menos 12 meses de datos para generar el pronóstico."
        )
      )
      
      ts_data <- ts(
        pagos_mes$Total_Pagado,
        start = c(
          as.numeric(format(min(pagos_mes$Mes), "%Y")),
          as.numeric(format(min(pagos_mes$Mes), "%m"))
        ),
        frequency = 12
      )
      
      ts_clean <- forecast::tsclean(ts_data)
      
      # ===============================
      # Modelos
      # ===============================
      set.seed(2631)
      modelo_sarima <- forecast::auto.arima(ts_clean, seasonal = TRUE)
      modelo_nnet <- forecast::nnetar(ts_clean, p = 6, size = 8, repeats = 50)
      
      fc_sarima <- forecast::forecast(modelo_sarima, h = input$horizonte)
      fc_nnet <- forecast::forecast(modelo_nnet, h = input$horizonte)
      
      ensamble <- fc_sarima$mean * 0.4 + fc_nnet$mean * 0.6
      
      # ===============================
      # Fechas futuras
      # ===============================
      fechas_futuras <- seq.Date(
        from = as.Date(format(max(pagos_mes$Mes) + 32, "%Y-%m-01")),
        by = "month",
        length.out = input$horizonte
      )
      
      tabla_pred <- data.frame(
        Mes = format(fechas_futuras, "%Y-%m"),
        SARIMA = round(as.numeric(fc_sarima$mean), 2),
        NNETAR = round(as.numeric(fc_nnet$mean), 2),
        Ensamble = round(as.numeric(ensamble), 2)
      )
      
      # ===============================
      # Gráfico
      # ===============================
      output$grafico_prediccion <- renderPlot({
        forecast::autoplot(ts_data) +
          forecast::autolayer(ensamble, series = "Ensamble", size = 1.2) +
          labs(
            title = "Pronóstico mensual de pagos",
            y = "Total pagado",
            x = "Tiempo"
          ) +
          theme_minimal()
      })
      
      # ===============================
      # Tabla
      # ===============================
      output$tabla_predicciones <- DT::renderDT({
        DT::datatable(tabla_pred, rownames = FALSE)
      })
      
      # ===============================
      # Indicadores
      # ===============================
      output$total_predicho <- renderValueBox({
        bs4Dash::bs4ValueBox(
          value = round(sum(tabla_pred$Ensamble), 2),
          subtitle = "Total proyectado",
          icon = icon("dollar-sign"),
          color = "primary"
        )
      })
      
      output$promedio_predicho <- renderValueBox({
        bs4Dash::bs4ValueBox(
          value = round(mean(tabla_pred$Ensamble), 2),
          subtitle = "Promedio mensual",
          icon = icon("chart-line"),
          color = "info"
        )
      })
      
      output$ultimo_real <- renderValueBox({
        bs4Dash::bs4ValueBox(
          value = round(tail(ts_data, 1), 2),
          subtitle = "Último valor observado",
          icon = icon("calendar-check"),
          color = "success"
        )
      })
      
      # ===============================
      # Métricas
      # ===============================
      acc_sarima <- forecast::accuracy(modelo_sarima)
      acc_nnet <- forecast::accuracy(modelo_nnet)
      
      output$metricas_explicadas <- renderText({
        paste(
          "Métricas in-sample:\n\n",
          "SARIMA → RMSE:", round(acc_sarima[1, "RMSE"], 2),
          " | MAPE:", round(acc_sarima[1, "MAPE"], 2), "%\n",
          "NNETAR → RMSE:", round(acc_nnet[1, "RMSE"], 2),
          " | MAPE:", round(acc_nnet[1, "MAPE"], 2), "%\n\n",
          "Se utiliza un ensamble para mejorar estabilidad.\n",
          "El horizonte está limitado para reducir incertidumbre."
        )
      })
    })
  })
}
