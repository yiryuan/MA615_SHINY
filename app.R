library(shiny)
library(leaflet)
library(shinydashboard)
library(magrittr)
library(dplyr)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title = "MA 615 Shiny Group 8",titleWidth=800),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Simple_Map",tabName ="simple_map",icon=icon("map","fa-solid fa-map")),
      menuItem("Table",tabName ="table",icon=icon("table","fa-solid fa-table")),
      menuItem("Map_with_Detail",tabName ="map_with_detail",icon=icon("map-location-dot","fa-solid fa-map-location-dot"))
      )
    ),
 dashboardBody(
    tabItems(
      tabItem(tabName = "simple_map",
              h5("Group 8 chose the food truck schedule data found 
              in Analyze Boston Web. The dataset includes the food trucks' 
              business schedules,locations, brand names, and business websites.
              The simple_map tab displays a map based on data that includes 
              exact locations. The table tab presents the data of the food truck
              schedule and you can search for exciting information in the table. 
              The map_with_detail tab shows a map that displays 
              locations that are areas that correspond to neighborhoods, 
              districts, or zones."),
              fluidRow(
               plotOutput("shape",brush = "shape_brush")
                )
              )
            ),
    tabItems(
      tabItem(tabName = "table",
              fluidRow(
                dataTableOutput("table")
              )
            )
      ),
    tabItems(
      tabItem(tabName = "map_with_detail",
              fluidRow(
                leafletOutput("locations")
              )
      )
    )
 )
)

server <- function(input, output, session) {
  food_truck<- read.csv(url("https://bostonopendata-boston.opendata.arcgis.com/datasets/boston::food-truck-schedule.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"))
  food_truck$group <- 1
  food_truck[(food_truck$Location =="Back Bay"),"group"] <- 2
  food_truck[(food_truck$Location =="Downtown"),"group"] <- 3
  food_truck[(food_truck$Location =="Charlestown"),"group"] <- 4
  food_truck[(food_truck$Location =="Kenmore"),"group"] <- 5
  food_truck[(food_truck$Location =="Seaport"),"group"] <- 6
  
  output$shape<- renderPlot(ggplot(data = food_truck) + 
                              geom_polygon(aes(x = x, y = y))+
                              geom_point(data = food_truck, aes(x=x, y=y), color ="dark blue", size=4)+ 
                              labs(x="Longitude",y="latitude"))
  
  
  output$table <- renderTable({
    brushedPoints(food_truck, input$shape_brush)
  })
  
  tab <-  food_truck %>% 
    select("Day","Time","Truck","Hours","Link")
  output$table <- renderDataTable(tab, options = list(pageLength = 5))
  
  output$locations<-renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -71.0589, lat = 42.3601, zoom = 10) %>%
      addMarkers(lng = food_truck$x, 
                 lat = food_truck$y, 
                 popup = food_truck$Truck)
  })
}
shinyApp(ui, server)