library(shiny)
library(plotly)
library(leaflet)
library(shinycssloaders)

# Define UI for the application
ui <- navbarPage(
  title = "Park Utilisation and Pedestrian Activity",  # Title of your app and the navigation bar
  
  # First page: Visualisations
  tabPanel(
    "Data Visualisation",
    fluidPage(
      # Bar chart and filters + explanation
      fluidRow(
        # Bar chart
        column(
          width = 8,  
          div(
            style = "border: 1px solid #dcdcdc; padding: 15px; margin: 10px;",  
            h4("Average Pedestrian Count Across Suburbs or Development Site by Time Dimension", 
               style = "text-align: center;"),  
            withSpinner(
              plotlyOutput("interactiveBarPlot", height = "900px")  
            )
          )
        ),
        # Filters + explanation
        column(
          width = 4,  
          div(
            style = "border: 1px solid #dcdcdc; padding: 15px; margin: 10px;",  
            h4("Stacked Bar Graph Filters", style = "text-align: center;"),  
            selectInput("xAxisVar", "Choose X-Axis Variable:",
                        choices = c("Suburb" = "suburb", "Development Site" = "dev_site")),  
            selectInput("timeDim", "Choose the Time Dimension:",
                        choices = c(
                          "Month" = "month", 
                          "Day of the Week" = "dayofweek",
                          "Weekends vs. Weekdays" = "is_weekend"
                        )),  
            div(
              style = "border: 1px solid #dcdcdc; padding: 15px; margin: 10px;",
              h4("Stacked Bar Graph Insights", style = "text-align: center;"),
              p("Insights:"),
              tags$ul(
                tags$li("Narre Warren consistently shows the highest pedestrian counts, especially during the later months of the year, indicating higher foot traffic compared to Berwick and Cranbourne."),
                tags$li("Berwick and Cranbourne exhibit lower and more uniform pedestrian activity, with minor peaks occurring in different months."),
                tags$li("Bunjil Place's (Development Site) significantly higher pedestrian count suggests it serves as a major activity center, while Berwick Village and Cranbourne High Street experience more stable, lower traffic throughout the year.")
              ),
              p("Implementation:"),
              tags$ul(
                tags$li("Narre Warren sees peak pedestrian activity in August and September, making it essential to implement safety measures, such as improved lighting and crosswalks, before these months to handle the increased foot traffic effectively."),
                tags$li("The similar distribution between weekdays and weekends suggests that public spaces and infrastructure are utilised evenly throughout the week. This consistency is beneficial for planning regular maintenance or service enhancements without needing to focus specifically on weekends.")
              ) 
            )
          )
        )
      ),
      
      # Stacked area chart and filters + explanation
      fluidRow(
        # Filters + explanation
        column(
          width = 4,  
          div(
            style = "border: 1px solid #dcdcdc; padding: 15px; margin: 10px;",  
            h4("Stacked Area Graph Filters", style = "text-align: center;"),  
            verbatimTextOutput("selectedSuburbs"),  
            actionButton("clearSelection", "Clear Selection"),  
            selectInput("StackedGraphdim", "Choose the Time Dimension:",
                        choices = c(
                          "Month" = "month_num", 
                          "Day of the Week" = "day_of_week_index",
                          "Date of Month" = "day",
                          "Hourly" = "hour",
                          "Weekends vs. Weekdays" = "is_weekend_index"
                        )),  
            selectInput("StackedGraphfill", "Choose the Fill Dimension:",
                        choices = c(
                          "Day of the Week" = "day_of_week_index",
                          "Month" = "month_num", 
                          "Date of Month" = "day",
                          "Hourly" = "hour",
                          "Weekends vs. Weekdays" = "is_weekend_index"
                        )),  
            div(
              style = "border: 1px solid #dcdcdc; padding: 15px; margin: 10px;",
              h4("Stacked Area Graph Insights", style = "text-align: center;"),
              p("Insights:"),
              tags$ul(
                tags$li("Weekday vs. Weekend Activity: Pedestrian activity peaks later on weekends (11 AM to 3 PM), suggesting recreational use of public spaces, while weekdays see higher movement earlier (8 AM to 2 PM), likely due to work and school routines."),
                tags$li("Morning and Evening Trends: Pedestrian activity is minimal between 12 AM and 6 AM on both weekdays and weekends, but weekend evenings (after 8 PM) show slightly more activity, indicating social or leisure engagements."),
                tags$li("Daily Peak Activity for Narre Warren: Tuesday records the highest pedestrian count, peaking at 11 AM and declining through the afternoon. Thursday and Friday also see significant foot traffic from 11 AM to 3 PM, while Monday, Wednesday, and Sunday have more moderate activity during this time."),
                tags$li("Seasonal Trends in Melbourne: Pedestrian activity in Melbourne peaks in July and October, with the highest average count in July (237 on Thursdays). The increase in July is likely due to winter festivals, school holidays, and cooler weather, while October's high activity aligns with spring events and warmer temperatures.")
              ),
              p("Implementation:"),
              tags$ul(
                tags$li("Weekday Improvements: Enhance crosswalks and public amenities from 8 AM to 2 PM in high-traffic areas to support work and school commutes."),
                tags$li("Weekend Enhancements: Focus on improving parks and public spaces (e.g., seating, signage) between 11 AM and 3 PM for increased recreational use."),
                tags$li("Evening Safety: Strengthen street lighting and security after 8 PM, especially on weekends. Although early morning activity is low, adding patrols or emergency services can improve safety during off-peak times."),
                tags$li("Seasonal Focus: Enhance park amenities, lighting, and event preparation during July and October to support increased foot traffic from winter and spring events, with particular attention to winter festivals and holiday crowds.")
              )  
            )
          )
        ),
        column(
          # Stacked area graph
          width = 8,  
          div(
            style = "border: 1px solid #dcdcdc; padding: 15px; margin: 10px;",  
            h4("Average Pedestrian Count by Selected Time Dimension", style = "text-align: center;"),  
            withSpinner(
              plotlyOutput("stackedGraphPlot", height = "700px")
            )
          )
        )
      ),
      
      # Map section
      fluidRow(
        column(
          width = 12,  
          div(
            style = "border: 1px solid #dcdcdc; padding: 15px; margin: 10px;",  
            h4("Park and Reserves Sensor Location and Utilisation", style = "text-align: center;"),  
            withSpinner(
              leafletOutput("InteractiveMap", height = "600px")
            ) 
          )
        )
      ),
      # Map filter and legend on the same row
      fluidRow(
        # Pedestrian count legend (uiOutput)
        column(
          width = 6,   
          div(
            style = "border: 1px solid #dcdcdc; padding: 15px; margin: 10px;",  
            h4("Pedestrian Count Legend", style = "text-align: center;"),  
            uiOutput("map_legend")  
          )
        ),
        # Map filters next to the legend
        column(
          width = 6,  
          div(
            style = "border: 1px solid #dcdcdc; padding: 15px; margin: 10px; text-align: center;",  # Center the content
            h4("Map Filters", style = "text-align: center;"),  
            div(
              style = "display: flex; justify-content: center;",  # Centers the select input
              selectInput("AreaRadius", "Choose the Radius:",
                          choices = c("0.75 km" = 750, "1 km" = 1000, "1.25 km" = 1250))  
            )
          )
        )
      ),
      # Map filter and explanation
      fluidRow(
        column(
          width = 12,  
          div(
            style = "border: 1px solid #dcdcdc; padding: 15px; margin: 10px;",  
            h4("Map Explanation", style = "text-align: center;"),
            p("Insights:"),
            tags$ul(
              tags$li("Park Proximity: Narre Warren (Bunjil Place) experiences consistently high pedestrian activity, especially in mid-morning and during later months, due to its dual role as a cultural hub and key transit route. With 74 parks within 1km, the area's blend of transit infrastructure and park access drives significant foot traffic, serving both commuters and leisure seekers."),
              tags$li("Park and Pedestrian Count: Cranbourne (High Street) and Berwick (Village) have moderate pedestrian counts (41 and 35, respectively), despite similar park access within a 1km radius. Lower foot traffic may be due to fewer high-density public facilities or events compared to Narre Warren."),
              tags$li("Utilisation Pattern: Proximity to parks alone doesn't necessarily increase foot traffic. Narre Warren's higher pedestrian count suggests that urban development, community activities, and commercial centers play a larger role in driving pedestrian movement."),
              tags$li("Weekday vs. Weekend Patterns: Narre Warren maintains high pedestrian activity on both weekdays and weekends, especially on Tuesdays and in July and October, serving as a major residential, commercial, and transit hub. In contrast, Cranbourne and Berwick show more uniform, localised foot traffic with less variation between weekdays and weekends.")
            ),
            p("Improvements:"),
            tags$ul(
              tags$li("Narre Warren: City planners should focus on enhancing park amenities, improving walkways, safety, and event support around high-traffic zones like Bunjil Place to sustain pedestrian activity and keep the area attractive."),
              tags$li("Cranbourne and Berwick: To increase pedestrian engagement, planners should host community events and improve park accessibility with better lighting, signage, and transportation links to encourage foot traffic to local parks."),
              tags$li("Lower Traffic Areas: In areas like Berwick and Cranbourne with lower pedestrian counts, developing community hubs or commercial spaces near parks and organising events can stimulate park use and attract more foot traffic.")
            )
          )
        )
      )
    )
  ),
  
  # Second Page: References
  tabPanel(
    "About",
    fluidPage(
      fluidRow(
        column(
          width = 12,
          div(
            style = "padding: 20px;",
            h2("Data Visualisation Project (DVP)"),
            hr(),
            h3("By: Yehezkiel Efraim Darmadi"),
            p("26 October 2024"),
            hr(),
            h3("Brief Explanation"),
            p("This application provides a comprehensive exploration of park utilisation and pedestrian activity within the City of Casey. It integrates data from multiple sources, including IoT sensors tracking pedestrian movement and GIS data for park locations, suburbs, and wards. The goal of this project is to provide urban planners, policymakers, and community stakeholders with an interactive tool to analyse pedestrian patterns. Users can explore trends in foot traffic across different regions and time periods, helping identify peak activity times and areas that might require infrastructure improvements or additional amenities."),
            p("Through this interactive visualisation, users can examine the relationships between pedestrian activity, park accessibility, and urban spaces. By uncovering insights from the data, the application enables decision-makers to enhance public safety, optimise resource allocation, and foster more vibrant, community-centered urban environments. The ability to filter and drill down into specific regions and time dimensions provides a deeper understanding of how the public utilises parks and open spaces."),
            p("The visualisation allows users to dynamically interact with the data by selecting different time dimensions (such as month, day of the week) and locations (suburb or development site). The project emphasises ease of use and engagement through intuitive controls and real-time updates, making it a valuable tool for ongoing urban development efforts in Casey."),
            h3("Data Sources"),
            tags$ul(
              tags$li("Pedestrian Count Data: Sourced from the City of Casey's open data portal. It consists of two datasets, one in Excel format with 360,401 rows and 27 columns, covering from August 12, 2021, to October 5, 2022, and another in GeoJSON format with 204,814 rows and 25 columns, covering from September 19, 2023, to October 20, 2024."), 
              tags$li("Park and Reserve Locations: Contains the locations and classifications of parks and reserves in the City of Casey, sourced from local government GIS data."), 
              tags$li("Suburb and Ward Shapefiles: Provides suburb and ward boundaries, sourced from the City of Casey and Geoscape administrative boundaries.")
            ),
            h3("Data References"),
            tags$ul(
              tags$li(tags$a(href = "https://data.casey.vic.gov.au/explore/dataset/pedestrian/export/?disjunctive.location_suburb&disjunctive.dev_site&disjunctive.location_desc&disjunctive.sub_location&disjunctive.devicename&sort=date", "City of Casey Open Data Portal")),
              tags$li(tags$a(href = "https://discover.data.vic.gov.au/dataset/parks-and-reserves-locations", "Parks and Reserves Locations Dataset")),
              tags$li(tags$a(href = "https://data.gov.au/dataset/ds-dga-bdcf5b09-89bc-47ec-9281-6b8e9ee147aa/details?q=", "Geoscape Administrative Boundaries")),
              tags$li(tags$a(href = "https://data.casey.vic.gov.au/explore/dataset/suburbs-w-postcodes/export/?disjunctive.suburb&disjunctive.postcode", "City of Casey Suburbs Boundaries"))
            ),
            h3("Other References"),
            tags$ul(
              tags$li(tags$a(href = "https://www.youtube.com/watch?v=fVFAI1CYudw&t=132s", "Agrawal, A. (2018, October 11). R Shiny App Tutorial | shinycssloaders package | Add animated spinner/loader to Shiny App [Video]. YouTube.")),
              tags$li(tags$a(href = "https://r-graph-gallery.com/stacked-area-graph", "R Graph Gallery. (n.d.). Stacked area chart. R Graph Gallery."))
              )
          )
        )
      )
    )
  )
)
