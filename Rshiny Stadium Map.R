library(readxl)
library(sf)
library(tidyverse)
library(dplyr)
library(shiny)
library(shinydashboard)
library(leaflet)

Stadium <- read_excel("C:/Users/William/OneDrive/Bureau/Stadium.xlsx")
View(Stadium)

Stadium <- Stadium %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326)
View(Stadium)

Stadium <- Stadium %>%
  mutate(popup_ingo = paste('<b>', 'Stadium :', '</b>', Stadium$stade, "<br/>",
                            '<b>', 'Club :', '</b>', Stadium$tenant, "<br/>",
                            '<b>', 'Capacity :', '</b>', Stadium$capacity,"<br/>",
                            '<b>', 'Country :', '</b>', Stadium$country, "<br/>",
                            '<b>', 'Opened :', '</b>', Stadium$opened, "<br/>",
                            '<b>', 'Renovated :', '</b>', Stadium$renovated,"<br/>",
                            '<b>', 'id :', '</b>', Stadium$id, "<br/>")
  )

#Stadium <- Stadium %>%
#  filter(!is.na(Stadium$capacity)) # suppression des valeurs NA pour capacité



ui <- dashboardPage(
  dashboardHeader(title = 'Stadium'),
  dashboardSidebar(
    sliderInput('capacity', 'Capacity', min = 0, max = 120000, value = c(0, 80000), step = 1000),
    sliderInput('opened', 'Opening date', 1906, 2022, value = 1960, step = 1)
  ),
  dashboardBody(
    leafletOutput('map', width = '100%', height = 800)
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    Stadium %>%
      filter((capacity >= input$capacity[1] & capacity <= input$capacity[2]) & opened >= input$opened)
  })
  
  observe({
    leafletProxy("map", data = filtered_data()) %>%
      addProviderTiles(providers$OpenStreetMap.France) %>%
      clearMarkers() %>%
      addCircleMarkers(
        #data = filtered_data$geometry,
        #lng = as.numeric(William_Data$longitude),
        #lat = as.numeric(William_Data$latitude),
        radius = 7,
        color = "blue",
        stroke = FALSE,
        fillOpacity = 0.3,
        popup = ~popup_ingo #info pour la popup
        #labelOptions = labelOptions(direction = "bottom"),
        #popup = paste(
        #'<b>', 'Ville :', '</b>', William_Data_Coords$ville, "<br/>",
        #'<b>', 'Club :', '</b>', William_Data_Coords$club, "<br/>",
        #'<b>', 'Nom du stade :', '</b>', William_Data_Coords$nom,"<br/>",
        #'<b>', 'Capacité :', '</b>', William_Data_Coords$capacite, "<br/>"
        #),
        #popupOptions = popupOptions(direction = "bottom")
      )
  })
  
  output$map <- renderLeaflet({
    map <- leaflet() %>% setView(lng = 8.227512, lat = 46.818188, zoom = 5) #coords Europe
    
    #map %>%
    #addProviderTiles(providers$OpenStreetMap.France) %>%
    #addCircleMarkers(
    #data = filtered_data$geometry,
    #radius = 8,
    #color = "blue",
    #stroke = FALSE,
    #fillOpacity = 0.5,
    #label = datas_with_coords$club,
    #labelOptions = labelOptions(direction = "bottom")
    #)
  })
}
shinyApp(ui, server)
