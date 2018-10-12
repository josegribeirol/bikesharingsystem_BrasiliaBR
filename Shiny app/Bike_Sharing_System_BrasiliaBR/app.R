library(shiny)
library(leaflet)
library(tidyverse)
library(lubridate)

df_rides <- read.csv("https://raw.githubusercontent.com/josegrlopes/bikesharingsystem_BrasiliaBR/master/data/df_rides.csv", encoding = "UTF-8")
df_stations <- read.csv("https://raw.githubusercontent.com/josegrlopes/bikesharingsystem_BrasiliaBR/master/data/df_stations.csv", encoding = "UTF-8")

ui <- fluidPage(
  titlePanel("Bike Sharing System - Brasília, Brazil"),
  sidebarLayout(
    sidebarPanel(
      selectInput("station", "Station:", df_stations$station)
    ), mainPanel(plotOutput("plot_gender"), leafletOutput("map"), plotOutput("plot_time_start"), plotOutput("plot_time_end"))
  )
  
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng=df_stations[df_stations$station == input$station,]$lon, lat=df_stations[df_stations$station == input$station,]$lat, popup=df_stations[df_stations$station == input$station,]$station)
  })
  
  output$plot_gender <- renderPlot({
    df_rides %>% filter(!is.na(user_gender), station_start == input$station) %>% 
      ggplot() +
      geom_bar(aes(x = user_gender)) +
      theme_light()+
      labs(x = "Gender of the User", y = "Rides")
  })
  
  output$plot_time_start <- renderPlot({
    df_rides %>% 
      mutate(hour = hour(time_start)) %>% 
      filter(station_start == input$station) %>% 
      ggplot()+
      geom_bar(aes(x = hour))+
      scale_x_continuous(breaks = seq(0,23))
  })
  
  output$plot_time_end <- renderPlot({
    df_rides %>% 
      mutate(hour = hour(time_end)) %>% 
      filter(station_end == input$station) %>% 
      ggplot()+
      geom_bar(aes(x = hour))+
      scale_x_continuous(breaks = seq(0,23))
  })
}

shinyApp(ui, server)