library(shiny)
library(readxl)
library(dplyr)
library(janitor)
library(leaflet)
library(ggplot2)
library(shinythemes)
library(thematic)
library(DT)

# dos caminos:
# 1. descargar el archivo manualmente y
# 2. leer dede la ruta de internet :(
# url <- "https://datos.gob.cl/dataset/c2969d8a-df82-4a6c-a1f8-e5eba36af6cf/resource/cbd329c6-9fe6-4dc1-91e3-a99689fd0254/download/pcma_20211117_oficio-4770_2013.xlsx"
# datos <- readxl::read_xlsx("Clases/Shiny/pcma_20211117_oficio-4770_2013.xlsx", skip = 8)



if(!require(tidyverse)) install.packages("tidyverse")
if(!require(jsonlite))  install.packages("jsonlite") 

library(tidyverse)
library(jsonlite)




opciones_empresas <- c("NUEVAPOLAR", "SMU", "BESALCO", "COPEC", "FALABELLA", 
                    "BSANTANDER",  "CMPC", "CHILE", "SQM-B", "ENELAM", "CENCOSUD",
                    "BCI", "LTM",  "ENELCHILE", "SM-CHILE B", "CCU", "PARAUCO",
                    "ITAUCORP", "AGUAS-A",  "COLBUN", "ENTEL", "ECL", "CONCHATORO",
                    "RIPLEY", "AESGENER",  "ANDINA-B", "SONDA", "CAP", "ILC", 
                    "SALFACORP", "SECURITY", "VAPORES",  "ENELGXCH", "ANTARCHILE",
                    "BANMEDICA", "EMBONOR-B", "FORUS",  "IAM", "MASISA", "ORO BLANCO", 
                    "SK", "SMSAAM")


# datos <- datos %>% 
#   mutate(
#     LONGITUD = as.numeric(LONGITUD),
#     LATITUD = as.numeric(LATITUD)
#   )
# 
# datos <- clean_names(datos)
# 
# opciones_comuna  <- unique(datos$comuna)


# ui ----------------------------------------------------------------------


ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("Indicadores Economicos"),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectizeInput("empresa",
                     label = "Seleccione empresa",
                     choices = opciones_empresas,
                     multiple = FALSE,
                     selected = "FALABELLA"),
      
      
      sliderInput("anios", label = h3("Rango de años"), min = 2001, 
                  max = 2021, value = c(40, 60))
    ),
    mainPanel(
      width = 10,
      
      fluidRow(
        column(6, plotOutput("plot"))
      )
    )
  ),
  DTOutput("tabla")
)



# server ------------------------------------------------------------------


server <- function(input, output) {
  
  data_indicadores <- reactive( { 
    

    obtener_indicadores <- function(empresa = input$empresa) { 
      
      url <- stringr::str_c("https://www.elmercurio.com/inversiones/json/json.aspx?categoria=", 
                            input$empresa, "&time=10&indicador=2") 
      
      df <- jsonlite::read_json(url)$Data %>% 
        stringr::str_split(";") %>% 
        dplyr::first() %>%
        I() %>% 
        readr::read_delim(delim = ",", col_names = c("fecha", "precio", "vol")) 
      
      df <- df %>% 
        mutate(
          fecha = lubridate::ymd_hms(fecha),
          anio = lubridate::year(fecha)
        )  %>% filter(anio>=min(input$anios) & anio<=max(input$anios) )
      
    } 
    
    df <- obtener_indicadores(empresa) 
  
} )


output$plot <- renderPlot({
  
  df <- data_indicadores()
  
  ggplot(df) + 
    geom_line(aes(fecha, precio)) 
  
})
output$tabla <- renderDT({
  
  df <- data_indicadores() %>%
    select(fecha, anio, precio, vol )
  
datatable(df)
  
})


}

  # # definir expresión reactiva
  # data_comunas <- reactive({
  #   # Sys.sleep(5)
  #   dcomuna <- datos %>% 
  #     filter(comuna == input$comuna)
  #   dcomuna
  # })
  
  


thematic_shiny()

shinyApp(ui, server)

