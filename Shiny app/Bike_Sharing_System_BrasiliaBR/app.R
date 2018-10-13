library(shiny)
library(leaflet)
library(tidyverse)
library(lubridate)
library(repmis)

df_rides <- readRDS("data/df_rides.rds")
df_stations <- readRDS("data/df_stations.rds")

ui <- fluidPage(
  titlePanel("Bike Sharing System - Brasília, Brazil"),
  sidebarLayout(
    sidebarPanel(
      selectInput("station", "Station:", df_stations$station),
      dateRangeInput('date',
                     label = 'Date range:',
                     start = "2018-01-01", end = "2018-08-31")
    ), mainPanel(leafletOutput("map"), plotOutput("plot_gender"), plotOutput("plot_time_start"), plotOutput("plot_time_end"), verbatimTextOutput("date"))
  )
  
)

server <- function(input, output, session) {

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng=df_stations[df_stations$station == input$station,]$lon, lat=df_stations[df_stations$station == input$station,]$lat, popup=df_stations[df_stations$station == input$station,]$station)
  })
  
  output$plot_gender <- renderPlot({
    df_rides %>% 
      filter(ride_date >= input$date[1], ride_date <= input$date[2]) %>% 
      filter(!is.na(user_gender), station_start == input$station) %>% 
      ggplot() +
      geom_bar(aes(x = user_gender)) +
      theme_minimal()+
      labs(x = "Gender of the User", y = "Rides")
  })
  
  output$plot_time_start <- renderPlot({
    df_rides %>% 
      filter(ride_date >= input$date[1], ride_date <= input$date[2]) %>% 
      mutate(hour = hour(time_start)) %>% 
      filter(station_start == input$station) %>% 
      ggplot()+
      geom_bar(aes(x = hour))+
      scale_x_continuous(breaks = seq(0,23))+
      theme_minimal()+
      labs(x = "Departures by hour", y = "Frequency")
    
  })
  
  output$plot_time_end <- renderPlot({
    df_rides %>% 
      filter(ride_date >= input$date[1], ride_date <= input$date[2]) %>% 
      mutate(hour = hour(time_end)) %>% 
      filter(station_end == input$station) %>% 
      ggplot()+
      geom_bar(aes(x = hour))+
      scale_x_continuous(breaks = seq(0,23))+
      theme_minimal()+
      labs(x = "Arrivals by hour", y = "Frequency")
  })
  
  output$date  <- renderText({
    paste("Start:", input$date[1], "\n End:", input$date[2])
  })
}

shinyApp(ui, server)