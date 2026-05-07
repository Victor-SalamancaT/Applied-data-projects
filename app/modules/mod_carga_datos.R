# ================================================================
# mod_carga_datos.R - Carga y limpieza de datos base
# ================================================================

mod_carga_datos_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      title = "Cargar base de datos histórica",
      width = 12,
      solidHeader = TRUE,
      color = "primary",
      
      fileInput(
        ns("archivo"),
        "Selecciona el archivo Excel (.xlsx)",
        accept = ".xlsx"
      ),
      
      p("Asegúrate de subir el archivo unificado con el formato esperado."),
      
      withSpinner(
        tableOutput(ns("vista_previa"))
      )
    )
  )
}

mod_carga_datos_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    datos <- reactiveVal(NULL)
    
    observeEvent(input$archivo, {
      req(input$archivo)
      
      showNotification("Cargando y limpiando datos...", type = "message")
      
      # --- Lectura del Excel ---
      detalle <- readxl::read_excel(
        input$archivo$datapath,
        sheet = "Unificado"
      )
      
      # --- Renombrado literal de columnas ---
      colnames(detalle) <- c(
        "#", "Marca", "Fabricante", "Dealer", "Nombre_Concesionaria",
        "Orden", "VIN", "TME", "Plataforma", "Entrega", "Recepcion",
        "Pago", "Km", "Gti", "AT", "Daño", "Usuario", "Nombre", "Área",
        "Comentario", "Comentario_Deferencia",
        "%_AUT_MO", "%_AUT_MAT", "%_AUT_MO_EXT", "%_AUT_MAT_EXT",
        "PRES_UTI_MO", "PRES_UTI_MAT", "PRES_UTI_MO_EXT",
        "PRES_UTI_MAT_EXT", "PRES_UTI_TOTAL",
        "FRE", "GES", "KUN",
        "TOTALES_PAGADOS_MO", "TOTALES_PAGADOS_MAT",
        "TOTALES_PAGADOS_MO_EXT", "TOTALES_PAGADOS_MAT_EXT",
        "TOTALES_PAGADOS_TOTAL"
      )
      
      # --- Limpieza de usuarios ---
      detalle$Usuario[detalle$Usuario == "DP16RZQ.3"] <- "USER 1"
      detalle$Usuario[detalle$Usuario == "DP81QXW.3"] <- "USER 2"
      detalle$Usuario[detalle$Usuario == "DPMY3CV.3"] <- "USER 3"
      detalle$Usuario[detalle$Usuario == "DPVPKFF.3"] <- "USER 4"
      
      # --- Limpieza básica de texto ---
      detalle$Nombre_Concesionaria <- gsub(
        "Ã¿|Â|¿|\\?|Â¿",
        "",
        detalle$Nombre_Concesionaria
      )
      detalle$Nombre_Concesionaria <- trimws(detalle$Nombre_Concesionaria)
      
      # --- Reemplazos estandarizados de concesionarias ---
      reemplazos_concesionarias <- c(
        #Reemplazos de nombres mal escritos
      )
      
      detalle$Nombre_Concesionaria <- dplyr::recode(
        detalle$Nombre_Concesionaria,
        !!!reemplazos_concesionarias
      )
      
      # --- Limpiezas puntuales ---
      detalle$Daño[detalle$Daño == "ReparaciÃ³n segun instrucciones"] <-
        "Reparación según instrucciones"
      
      detalle$Plataforma[detalle$Plataforma == "auto"] <- "Auto"
      
      # --- Fechas ---
      detalle$Fecha_Pago <- as.Date(detalle$Pago)
      
      # --- Agrupación semanal ---
      pagos_semana <- detalle %>%
        dplyr::mutate(Semana = format(Fecha_Pago, "%Y-%U")) %>%
        dplyr::group_by(Semana) %>%
        dplyr::summarise(
          Total_Pagado = sum(TOTALES_PAGADOS_TOTAL, na.rm = TRUE),
          .groups = "drop"
        )
      
      ultima_fecha <- max(detalle$Fecha_Pago, na.rm = TRUE)
      
      datos(list(
        datos = detalle,
        pagos_semana = pagos_semana,
        ultima_fecha = ultima_fecha
      ))
      
      showNotification("Datos cargados y limpiados correctamente ✅", type = "message")
    })
    
    # --- Vista previa ---
    output$vista_previa <- renderTable({
      req(datos())
      tail(
        datos()$datos[, c(
          "#", "Nombre_Concesionaria", "Usuario",
          "Plataforma", "Daño", "Pago",
          "TOTALES_PAGADOS_TOTAL"
        )]
      )
    })
    
    return(datos)
  })
}
