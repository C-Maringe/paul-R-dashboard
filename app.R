#### group 2 assignment R Programming DASHBOARD CODE

### 1. first setting Up the working directory for the project

setwd('C:/Users/Dell/Desktop/R projects/paul/dashboard')

## Install packages to be later used in this pr
### 2. Dashboard Creation

# load the required packages

library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)
library(tidyverse)
library(sf)
library(leaflet)
library(readr)

## this package is for deploying the project on the shiny.io cloud 

library(rsconnect)

# Auto Hide shiny sidebar

library(shinyjs)

### importing dataset this is from ! data set used from data processing stage from dataprocessing.R file

active <- read_csv("active.csv")
dfProvince <- read_csv("dfProvince.csv")
dfProvinceClass <- read_csv("dfProvinceClass.csv")
Inactive <- read_csv("Inactive.csv")

#### Constant paramaters for bubble map 

mybins <- seq(600, 6000, by=900)
mypalette <- colorBin( palette="YlOrBr", domain=active$CountProvince, na.color="transparent", bins=mybins)

## app.R ##

### UI.R or user interface for the dashboard

ui <- dashboardPage(
  dashboardHeader(
    title = "Assignment 2: Group Assignment",
    titleWidth = 500
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("SideBar Functionalists",icon = icon("dashboard", verify_fa = FALSE)),
      menuItem("You can Select", icon = icon("th")),
      menuItem("You can Explore",icon = icon("info-circle")),
      menuItem("You can Reconfigure",icon = icon("bolt")),
      menuItem("You can Encode",icon = icon("bus")),
      menuItem("You can Abstract/Elaborate",icon = icon("chart-area")),
      menuItem("You can Filter",icon = icon("filter")),
      menuItem("You can Connect",icon = icon("circle"))
    )
  ),
  dashboardBody(
    
    frow2 <- fluidRow( 
      box(
        title = "Cluster Map"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,leafletOutput(outputId = "clustermap", height = "300px")
      )
      
      ,box(
        title = "Dot Map"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,leafletOutput(outputId = "dotmap", height = "300px")
      )
      ,box(
        title = "Bubble Map"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        #this will create a space for us to display our map
        ,leafletOutput(outputId = "bubblemap", height = "300px")
        ),
      box(
        title = "Chloropleth Map"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,leafletOutput(outputId = "cloroplethmap", height = "300px")
      )
      
      ),
    useShinyjs()
    )
  )

## Server .R where logic is written or backend of the system for dashboard creation

server <- function(input, output, session) {
  addClass(selector = "body", class = "sidebar-collapse")
  
  #create the maps
  
  ###########################################################################################
  ###########################################################################################
  
  # Cloropleth Map
  
  output$cloroplethmap <- renderLeaflet({
    clotext <- paste(
      "Country: ", "Zimbbwe", "<br/>",
      "CountProvince: ", ave(rep(1L, nrow(dfProvince)), dfProvince$Province, FUN = length), "<br/>") %>%
      lapply(htmltools::HTML)
    cloroplethmap <- leaflet() %>%
      addTiles() %>%
      addPolygons(data= dfProvince, lng = ~zw.lng, lat = ~zw.lat, color = "grey", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  label = clotext,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  group = "Province Summary grey") %>%
      addPolygons(data= dfProvince, lng = ~zw.lng, lat = ~zw.lat, color = "yellow", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  label = clotext,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  group = "Province Summary Yellow") %>%
      addPolygons(data= dfProvince, lng = ~zw.lng, lat = ~zw.lat, color = "red", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  label = clotext,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  group = "Province Summary red") %>%
      addPolygons(data= dfProvinceClass, lng = ~zw.lng, lat = ~zw.lat, color = "pink", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  label = clotext,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  group = "Province Class") %>%
      addPolygons(data= active, lng = ~zw.lng, lat = ~zw.lat, color = "green", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  label = clotext,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  group = "Active status") %>%
    addLayersControl(
      baseGroups = c("Province Summary grey", "Province Summary yellow", "Province Summary red"),
      overlayGroups = c("Province Class", "Active status"),
      options = layersControlOptions(collapsed = TRUE)
    )
    })
  
  ###########################################################################################
  ###########################################################################################
  
  #### CLUSTER MAP 
  
  
  output$clustermap <- renderLeaflet({
    
    atext <- paste(
      "Province: ", active$Province, "<br/>",
      "CountProvince: ", ave(rep(1L, nrow(active)), active$Province, FUN = length), "<br/>",
      "Fellow Status: ", active$`Fellow Status`, "<br/>", 
      "Class: ", active$Class, sep="") %>%
      lapply(htmltools::HTML)
    Intext = paste("Province: ", Inactive$Province, 
                   "<br/>",
                   "CountProvince: ", ave(rep(1L, nrow(Inactive)),
                                          Inactive$Province,
                                          FUN = length), 
                   "<br/>",
                   "Fellow Status: ", Inactive$`Fellow Status`,
                   "<br/>", 
                   "Class: ", Inactive$Class, sep="") %>%
      lapply(htmltools::HTML)
    
    
    clustermap <- leaflet() %>%
      addTiles() %>%
      setView(lng = 30, lat = -18, zoom = 6) %>%
      addCircleMarkers(data = active,
                       lng = ~zw.lng,
                       lat = ~zw.lat,
                       fillOpacity = 0.8,
                       stroke = F,
                       radius = 6,
                       label = atext,
                       popup = paste0("Province: ", active$Province,
                                      "<br>",
                                      "ProvinceFellowStatus: ", active$ProvinceFellowStatus,
                                      "<br>",
                                      "LevelType: ", active$LevelType,
                                      "<br>",
                                      "Status: ", active$Status),
                       clusterOptions = markerClusterOptions(),
                       group = "active") %>%
      addCircleMarkers(data = Inactive,
                       lng = ~zw.lng,
                       lat = ~zw.lat,
                       color =  "#998ec3",
                       fillOpacity = 0.8,
                       stroke = F,
                       radius = 6,
                       label = Intext,
                       popup = paste0("Province: ", Inactive$Province,
                                      "<br>",
                                      "LevelType: ", Inactive$LevelType,
                                      "<br>",
                                      "Status: ", Inactive$Status),
                       clusterOptions = markerClusterOptions(color =  "#998ec3"),
                       group = "Inactive") %>%
    clearControls() %>%
      addLegend("topright",
                title = "Active or Inactive capenum",
                colors = c("#f1a340", "blue", "#998ec3"),
                labels = c("CLICK FOR MORE INFO:", "Active", "Inactve"),
                opacity = 0.8) %>%
      addLayersControl(
        baseGroups = c("active", "Inactive"),
        options = layersControlOptions(collapsed = TRUE)
      )
      
    })
  
  ###########################################################################################
  ###########################################################################################
  
  ## Dot Map
  
  output$dotmap <- renderLeaflet({
    dottext <- paste(
      "Province: ", dfProvince$Province, "<br/>",
      "CountProvince: ", dfProvince$CountProvince, "<br/>") %>%
      lapply(htmltools::HTML)
    dotmap <- leaflet(dfProvince) %>%
      addTiles() %>%
      addCircles(
        lng = ~zw.lng, lat = ~zw.lat,
        weight = 1,
        radius = ~12000,
        popup = ~dfProvince$Province,
        label = dottext,
        labelOptions = labelOptions( style = list("font-weight" = "normal",
                                                  padding = "3px 8px"),
                                     textsize = "13px", direction = "auto"),
        group = "Hide Dots")%>%
      addMarkers(lng = ~dfProvince$zw.lng, lat = ~dfProvince$zw.lat,
                 label = dottext, group = "All Provinces") %>%
      addMarkers(lng = ~30.2000, lat = ~-17.3500,
                 label = dottext, group = "mashonaland west") %>%
      addMarkers(lng = ~31.5500, lat = ~-18.1833,
                 label = dottext, group = "mashonaland east") %>%
      addMarkers(lng = ~31.3333, lat = ~-17.3000,
                 label = dottext, group = "mashonaland central") %>%
      addMarkers(lng = ~31.0522, lat = ~-17.8292,
                 label = dottext, group = "harare") %>%
      addMarkers(lng = ~32.6694, lat = ~-18.9728,
                 label = dottext, group = "manicaland") %>%
      addMarkers(lng = ~29.8200, lat = ~-19.4580,
                 label = dottext, group = "midlands") %>%
      addMarkers(lng = ~30.8328, lat = ~-20.0744,
                 label = dottext, group = "masvingo") %>%
      addMarkers(lng = ~28.5667, lat = ~-20.1667,
                 label = dottext, group = "bulawayo") %>%
      addLayersControl(overlayGroups = c("Hide Dots"),
        baseGroups = c("All Provinces", "mashonaland west", "mashonaland east", "mashonaland central",
                       "harare", "manicaland", "midlands", "masvingo", "bulawayo"),
        options = layersControlOptions(collapsed = TRUE)
      )
    
    })
  
  
  ###########################################################################################
  ###########################################################################################
  
  # Bubble Map
  
  output$bubblemap <- renderLeaflet({
    mytext <- paste(
      "Province: ", active$Province, "<br/>",
      "CountProvince: ", ave(rep(1L, nrow(active)), active$Province, FUN = length),"<br/>",
      "Fellow Status: ", active$`Fellow Status`, "<br/>", 
      "Class: ", active$Class, sep="") %>%
      lapply(htmltools::HTML)
    bubblemap <- leaflet(active) %>%
      addTiles() %>%
      
      
      addCircleMarkers(lng = ~zw.lng, lat = ~zw.lat, 
                       fillColor = ~mypalette((active$CountProvince)), fillOpacity = 0.7,
                       color="blue", radius=(active$CountProvince)/500, stroke=FALSE,
                       label = mytext,
                       labelOptions = labelOptions( style = list("font-weight" = "normal",
                                                                 padding = "3px 8px"),
                                                    textsize = "13px", direction = "auto")
      ) %>%
      addLegend( pal=mypalette, values=~(active$CountProvince), opacity=0.9,
                 title = "Magnitude", position = "bottomright" )
    
    })
  ############################################################################################
  ############################################################################################
  
  ## To check if there are any errors or warnings
  traceback()
}

## RUNNING THE APP TO CREATE THE DASHBOARD

shinyApp(ui, server)

