#****************************************************************************
# AR/ vR 
#
# Data Visualization - ACT Road Crash Data  
#
# Juan Carlos Guarino - 3229246
# 
#****************************************************************************

#clean Console  as command (CTRL + L)
cat("\014") 

#clean all global variables
rm(list = ls())

#****************************************************************************
if (!require(tidyverse)) install.packages('tidyverse'); library (tidyverse)
if (!require(naniar)) install.packages('naniar'); library (naniar)
if (!require(ggthemes)) install.packages('ggthemes'); library (ggthemes)
if (!require(gridExtra)) install.packages('gridExtra'); library (gridExtra)
if (!require(corrplot)) install.packages('corrplot'); library (corrplot)
if (!require(shiny)) install.packages('shiny'); library (shiny)
if (!require(shiny)) install.packages('shiny'); library (shiny)
if (!require(leaflet)) install.packages('leaflet'); library (leaflet)
if (!require(leaflet.extras)) install.packages('leaflet.extras'); library (leaflet.extras)
if (!require(sf)) install.packages('sf'); library (sf)
if (!require(tigris)) install.packages('tigris'); library (tigris)




# Read data

act_crash <- read.csv('')

library(shiny)
library(leaflet)

ui <- fluidPage(
  sliderInput(inputId = "slider", 
              label = "values",
              min = 0,
              max = 100,
              value = 0,
              step = 1),
  leafletOutput("my_leaf")
)

server <- function(input, output, session){
  set.seed(123456)
  df <-  
    
  ## create static element
  output$my_leaf <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles('Hydda.Full') %>%
      setView(lat = -37.8, lng = 144.8, zoom = 8)
    
  })
  
  ## filter data
  df_filtered <- reactive({
    df[df$value >= input$slider, ]
  })
  
  ## respond to the filtered data
  observe({
    
    leafletProxy(mapId = "my_leaf", data = df_filtered()) %>%
      clearMarkers() %>%   ## clear previous markers
      addMarkers()
  })
  
}

shinyApp(ui, server)