#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#load libraries
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(leaflet.extras2)
library(shinydashboard)
library(sf)
library(geojsonsf)
library(tidyverse)
library(lubridate)

#import data
data <- read.csv('../Website/Daten_Output/Mobility_vaccine.csv')
data$month <- month(as.POSIXct(data$date,format="%Y-%m-%d"))
data$longitude <- ifelse(data$iso_code == 'DE-BW', 9.183333, 
                         ifelse(data$iso_code == 'DE-BE', 13.404954, 
                                ifelse(data$iso_code == 'DE-BB', 13.063561, 
                                       ifelse(data$iso_code == 'DE-HE', 8.239761, 
                                              ifelse(data$iso_code == 'DE-MV', 11.41316, 
                                                     ifelse(data$iso_code == 'DE-SL', 7.000000, 
                                                            ifelse(data$iso_code == 'DE-SH', 10.139444, 
                                                                   ifelse(data$iso_code == 'DE-BY', 11.581981, 
                                                                          ifelse(data$iso_code == 'DE-HH', 9.993682, 
                                                                                 ifelse(data$iso_code == 'DE-HB', 8.8016937, 
                                                                                        ifelse(data$iso_code == 'DE-NI', 9.7320104, 
                                                                                               ifelse(data$iso_code == 'DE-NW', 6.7734556, 
                                                                                                      ifelse(data$iso_code == 'DE-RP', 8.2472526, 
                                                                                                             ifelse(data$iso_code == 'DE-SN', 13.7372621, 
                                                                                                                    ifelse(data$iso_code == 'DE-ST', 11.6276237, 
                                                                                                                           ifelse(data$iso_code == 'DE-TH', 11.0298799, 0))))))))))))))))
data$latitude <- ifelse(data$iso_code == 'DE-BW', 48.783333, 
                        ifelse(data$iso_code == 'DE-BE', 52.520008, 
                               ifelse(data$iso_code == 'DE-BB', 52.391842, 
                                      ifelse(data$iso_code == 'DE-HE', 50.078217, 
                                             ifelse(data$iso_code == 'DE-MV', 53.62937, 
                                                    ifelse(data$iso_code == 'DE-SL', 	49.233334, 
                                                           ifelse(data$iso_code == 'DE-SH', 54.323334, 
                                                                  ifelse(data$iso_code == 'DE-BY', 48.135125, 
                                                                         ifelse(data$iso_code == 'DE-HH', 53.551085, 
                                                                                ifelse(data$iso_code == 'DE-HB', 53.0792962, 
                                                                                       ifelse(data$iso_code == 'DE-NI', 52.3758916, 
                                                                                              ifelse(data$iso_code == 'DE-NW', 51.2277411, 
                                                                                                     ifelse(data$iso_code == 'DE-RP', 49.9928617, 
                                                                                                            ifelse(data$iso_code == 'DE-SN', 51.0504088, 
                                                                                                                   ifelse(data$iso_code == 'DE-ST', 52.1205333, 
                                                                                                                          ifelse(data$iso_code == 'DE-TH', 50.9847679, 0))))))))))))))))




ui <- dashboardPage(dashboardHeader(title='Impfauswertung'),
                    dashboardSidebar(sliderInput(
                      "months",
                      "Month:",
                      min = 1,
                      max = 12,
                      value = 1
                    )),
                    dashboardBody(#sidebarLayout(
                      #sidebarPanel(),
                      
                      
                      
                      fluidPage(
                        mainPanel(
                          #this will create a space for us to display our map
                          leafletOutput(
                            outputId = "mymap",
                            height = 600,
                            width = 1500
                          ),
                          #this allows me to put the checkmarks ontop of the map to allow people to view earthquake depth or overlay a heatmap
                          absolutePanel(
                            top = 70,
                            left = 20,
                            checkboxInput("parks", "Parks", FALSE),
                            checkboxInput("retail", "Retail", FALSE),
                            checkboxInput("work", "Arbeitsplatz", FALSE),
                            checkboxInput("residential", "Wohnbezirke", FALSE),
                            checkboxInput("grocery", "Einkaufszentren", FALSE),
                            checkboxInput("transit", "Verkehr", FALSE),
                            
                          )
                        ),
                        tabsetPanel(
                          id = 'df',
                          br(),
                          tabPanel(dataTableOutput('table'))
                        )
                        
                      )))

server <- function(input, output, session) {
  #define the color pallate for the magnitidue of the earthquake
  pal <- colorNumeric(
    palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
    domain = data$parks)
  
  #define the color of for the depth of the earquakes
  pal2 <- colorFactor(
    palette = c('blue', 'yellow', 'red'),
    domain = data$parks
  )
  
  output$table <- renderDataTable({data %>% filter(month == input$months)})
  
  #create the map
  output$mymap <- renderLeaflet({
    leaflet(data) %>% 
      setView(lng = 10, lat = 51, zoom = 5)  %>% #setting the view over ~ center of North America
      addTiles() %>% 
      addCircles(data = data %>% filter(month == input$months), 
                 lat = ~ latitude, 
                 lng = ~ longitude, 
                 weight = 1,
                 radius = ~Anzahl, 
                 popup = ~as.character(Bundesland), 
                 label = ~as.character(paste0("Anzahl: ", sep = " ", Anzahl)), 
                 #color = ~pal(parks), 
                 fillOpacity = 0.5
                 )
  })

  
  #next we use the observe function to make the checkboxes dynamic. If you leave this part out you will see that the checkboxes, when clicked on the first time, display our filters...But if you then uncheck them they stay on. So we need to tell the server to update the map when the checkboxes are unchecked.
  observe({
    proxy <- leafletProxy("mymap", data = data %>% filter(month == input$months))
    proxy %>% clearMarkers()
    if (input$parks) {
      proxy %>% addCircleMarkers(stroke = FALSE, color = ~pal2(parks), fillOpacity = 0.1, label = ~as.character(paste0("Parks: ", sep = " ", parks)))}
    else {
      proxy %>% clearMarkers() %>% clearControls()
    }
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = data %>% filter(month == input$months))
    proxy %>% clearMarkers()
    if (input$retail) {
      proxy %>% addCircleMarkers(stroke = FALSE, color = ~pal2(retail_and_recreation), fillOpacity = 0.1, label = ~as.character(paste0("Retail: ", sep = " ", retail_and_recreation)))}
    else {
      proxy %>% clearMarkers() %>% clearControls()
    }
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = data %>% filter(month == input$months))
    proxy %>% clearMarkers()
    if (input$grocery) {
      proxy %>% addCircleMarkers(stroke = FALSE, color = ~pal2(grocery_and_pharmacy), fillOpacity = 0.1, label = ~as.character(paste0("Einkaufszentren: ", sep = " ", grocery_and_pharmacy)))}
    else {
      proxy %>% clearMarkers() %>% clearControls()
    }
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = data %>% filter(month == input$months))
    proxy %>% clearMarkers()
    if (input$transit) {
      proxy %>% addCircleMarkers(stroke = FALSE, color = ~pal2(transit_stations), fillOpacity = 0.1, label = ~as.character(paste0("Verkehr: ", sep = " ", transit_stations)))}
    else {
      proxy %>% clearMarkers() %>% clearControls()
    }
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = data %>% filter(month == input$months))
    proxy %>% clearMarkers()
    if (input$residential) {
      proxy %>% addCircleMarkers(stroke = FALSE, color = ~pal2(residential), fillOpacity = 0.1, label = ~as.character(paste0("Wohnbezirke: ", sep = " ", residential)))}
    else {
      proxy %>% clearMarkers() %>% clearControls()
    }
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = data %>% filter(month == input$months))
    proxy %>% clearMarkers()
    if (input$work) {
      proxy %>% addCircleMarkers(stroke = FALSE, color = ~pal2(workplaces), fillOpacity = 0.1, label = ~as.character(paste0("Arbeitsplatz: ", sep = " ", workplaces)))}
    else {
      proxy %>% clearMarkers() %>% clearControls()
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)