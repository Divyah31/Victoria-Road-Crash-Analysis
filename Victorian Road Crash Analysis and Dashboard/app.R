library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(lubridate)
library(leaflet)
library(shinythemes)


#Loading Data
myData <- read.csv("Victoria_Accident_Data_FIT5147S12024PE2v2.csv")

# Define UI for app
ui <- fixedPage(
  theme = shinytheme("sandstone"),
  div(
    style = "text-align: center; margin-top: 20px; margin-bottom: 20px;",
    h2("Exploring Victoria's Accident Data")
  ),
  
  
  tags$hr(style = "margin-top: 10px; margin-bottom: 20px;border-top: 1px solid black;"),
  
 
  
  #Vis1 and Vis2 and its Description
  fixedRow(
    column(6, plotOutput("vis1")),
    column(6, plotOutput("vis2"))
  ),
  fixedRow(
    column(6,
           style = "margin-top: 10px;",
           p("The stacked bar chart shows accidents across different speed zones 
under various light conditions. The tallest bar is in the highest speed zone (100), 
indicating it has the most accidents, predominantly during daylight.")),
    column(6, 
           style = "margin-top: 10px;",
           p("The stacked bar chart illustrates the number of accidents per hour of 
           the day for top 4 speed zones. There are peaks during certain hours,
           suggesting higher accident rates, possibly during peak hours, especially in the 100 speed zone."))
  ),
  
  tags$hr(style = "margin-top: 10px; margin-bottom: 20px;border-top: 1px solid black;"),
  
  fixedRow(
   column(12,
          h4("Map of Vehicle Road Crash in Victoria", style = "font-weight: bold;"),
          leafletOutput("map"))
  ),
  fixedRow(
    column(10,
           h4("Filter Options"),
           sliderInput("severity_slider", "Severity Rank Range",
                       min = min(myData$SEVERITY_RANK, na.rm = TRUE), 
                       max = max(myData$SEVERITY_RANK, na.rm = TRUE), 
                       value = c(min(myData$SEVERITY_RANK, na.rm = TRUE), max(myData$SEVERITY_RANK, na.rm = TRUE)),
                       step = 1),
           h5("Note: 1 - High Severity and 3 - Low Severity"),
           tags$style(HTML("
    .irs--shiny .irs-bar {
      background: #FF5733;
      border-top: 1px solid black;
      border-bottom: 1px solid black;
    }
    .irs--shiny .irs-to, .irs--shiny .irs-from {
      background-color: #FF5733;
    }
    .irs--shiny .irs-handle {
      border: 1px solid #D3D3D3;
      background-color: #D3D3D3;
    }"))
    )
  ),
  fixedRow(
   column(12,
          p("The map visualization above provides a representation of vehicle road crash in Victoria. 
             Each circle on the map is uniquely identified by its color and size. The color of the circle corresponds to the  
             light condition during the accident, while the size of the circle is indicative of its severity. Bigger circle represents high severity.
             Range sliders are incorporated, allowing to conveniently filter the accidents based on its severity.This interactive tool provides a user-friendly 
             way to investigate the distribution of accidents and the factors influencing them across Melbourne."))
  ),
  tags$hr(style = "margin-top: 10px; margin-bottom: 20px;border-top: 1px solid black;"),
  fixedRow(
    column(12,
           h4("Data Source", style = "font-weight: bold;"),
           p("The data used in this visualization is sourced from ",
             a("Vehicle Road Crash Data", href = "https://discover.data.vic.gov.au/dataset/victoria-road-crash-data"),
             " released by the Victorian Department of Transport and Planning in January 2024."))
  )
)

#Define server logic
server <- function(input, output){
  #Loading Data
  myData <- read.csv("Victoria_Accident_Data_FIT5147S12024PE2v2.csv")
  colnames(myData)
  
  #Visualisation1
  speedzone_max <- 110
  filtered_speedzone <- myData %>% filter(SPEED_ZONE<=speedzone_max)
  output$vis1 <- renderPlot({
    ggplot(filtered_speedzone, aes(x =SPEED_ZONE, fill =LIGHT_CONDITION_DESC)) +
      geom_bar(position = "stack") +
      scale_fill_brewer(palette = "Set1") +
      labs(title = "Number of Accidents by Light Condition and Speed Zone",
           x = "Speed Zone",
           y = "Count of Accidents",
           fill = "Light Condition") +
      theme_minimal()
  })
  
  # Calculating the top 4 speed zones with the most accidents
  top_speed_zones <- myData %>%
    group_by(SPEED_ZONE) %>%
    summarise(NumAccidents = n(), .groups = 'drop') %>%
    top_n(4, NumAccidents) %>%
    pull(SPEED_ZONE)
  
  hourly_accidents <- myData %>%
    filter(SPEED_ZONE %in% top_speed_zones) %>%
    mutate(
      Hour = hour(hms(ACCIDENT_TIME)),
      SPEED_ZONE = factor(SPEED_ZONE)
    ) %>%
    group_by(SPEED_ZONE, Hour) %>%
    summarise(NumAccidents = n(), .groups = 'drop')
  #print(hourly_accidents)
  
  # Define a vector of red shades
  red_shades <- c("#EEDC82", "#FFC300", "#FF5733", "#C70039")
  
  # Visualisation2: Plotting
  output$vis2 <- renderPlot({
    ggplot(hourly_accidents, aes(x = Hour, y = NumAccidents, fill = SPEED_ZONE)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = red_shades) +
      labs(title = "Number of Accidents by Hour for Top 4 Speed Zones",
           x = "Hour of the Day",
           y = "Count of Accidents",
           fill = "Speed Zone") +
      theme_minimal() +
      scale_x_continuous(breaks = 0:23)  # Assuming 24-hour format for ACCIDENT_TIME
  })
  
  #map 
 
  
  # Define processed data
  processedData <- reactive({
    myData %>%
      mutate(
        daynight = case_when(
          LIGHT_CONDITION_DESC == "Day" ~ "day",
          LIGHT_CONDITION_DESC == "Dusk/Dawn" ~ "dusk/dawn",
          TRUE ~ "night"
        ),
        # Reverse the severity scale
        scaled_severity = (max(SEVERITY_RANK) + 1) - SEVERITY_RANK
      )
  })
  
  
  filteredDataForMap <- reactive({
    processedData() %>%
      filter(SEVERITY_RANK >= input$severity_slider[1], SEVERITY_RANK <= input$severity_slider[2])
  })
  
  output$map <- renderLeaflet({
    leaflet(filteredDataForMap()) %>%
      addTiles(
        urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
      ) %>%
      addCircleMarkers(
        lng = ~LONGITUDE, lat = ~LATITUDE,
        color = ~case_when(
          daynight == "day" ~ "orange",
          daynight == "dusk/dawn" ~ "#FFEA00",
          daynight == "night" ~ "darkblue"
        ),
        radius = ~scaled_severity * 5,
        opacity = 1,
        fillOpacity = 0.5,
        label = ~paste("Date: ", ACCIDENT_DATE, 
                       "; Type: ", ACCIDENT_TYPE_DESC,
                       "; Light Condition: ", LIGHT_CONDITION_DESC,
                       "; Road Geometry: ", ROAD_GEOMETRY_DESC,
                       "; Speed Zone: ", SPEED_ZONE)
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("orange", "#FFEA00", "darkblue"),
        labels = c("Day", "Dusk/Dawn", "Night"),
        title = "Light Condition"
      )
  })
}

# Run the application
shinyApp(ui, server)
  

