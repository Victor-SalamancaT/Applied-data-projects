library(shiny)
library(bs4Dash)
library(dplyr)
library(ggplot2)
library(forecast)
library(plotly)
library(DT)
library(shinycssloaders)
library(readxl)

source("modules/mod_carga_datos.R")
source("modules/mod_home.R")
source("modules/mod_outliers.R")
source("modules/mod_forecast.R")

ui <- bs4DashPage(
  title = "Dashboard de Pagos",
  
  # ===== NAVBAR SUPERIOR =====
  header = dashboardHeader(
    title = tags$span("Análisis de Pagos", style = "font-weight:bold; color:white;"),
    skin = "light"
  ),
  
  # ===== SIDEBAR (menú lateral) =====
  sidebar = dashboardSidebar(
    skin = "dark",
    status = "primary",
    brandColor = "#0b2545",
    sidebarMenu(
      id = "tabs",
      menuItem("Carga de Datos", tabName = "carga", icon = icon("file-upload")),
      menuItem("Inicio", tabName = "home", icon = icon("home")),
      menuItem("Outliers", tabName = "outliers", icon = icon("exclamation-triangle")),
      menuItem("Predicciones", tabName = "forecast", icon = icon("chart-line"))
    )
  ),
  
  # ===== CONTENIDO =====
  body = dashboardBody(
    tags$head(
      tags$style(HTML("
        body { background-color: #ffffff; }
        .main-header { background-color: #0b2545 !important; }
        .sidebar-dark-primary { background-color: #0b2545 !important; }
      "))
    ),
    tabItems(
      tabItem(tabName = "carga", mod_carga_datos_ui("carga")),
      tabItem(tabName = "home", mod_home_ui("home")),
      tabItem(tabName = "outliers", mod_outliers_ui("outliers")),
      tabItem(tabName = "forecast", mod_forecast_ui("forecast"))
    )
  ),
  
  # ===== PIE DE PÁGINA =====
  footer = dashboardFooter(
    left = a(href = "#", "Desarrollado por Alfredo Tostado", target = "_blank"),
    right = "Dashboard de análisis de pagos | 2025",
    fixed = TRUE
  )
)

server <- function(input, output, session) {
  data_reactiva <- mod_carga_datos_server("carga")
  mod_home_server("home", data_reactiva)
  mod_outliers_server("outliers", data_reactiva)
  mod_forecast_server("forecast", data_reactiva)
}

shinyApp(ui, server)
