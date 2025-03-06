# Importing libraries
library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(rlang)
library(tidyverse)
library(leaflet)
library(sf)
library(shinycssloaders)

server <- function(input, output, session) {
  
  # Import data
  pd_df_clean <- read.csv("pd_df_clean.csv")
  
  # Making sure the data is cleaned
  na.omit(pd_df_clean)
  
  # Reorder months and days of the week in a specific order
  pd_df_clean$month <- factor(pd_df_clean$month, 
                              levels = c("January", "February", "March", "April", 
                                         "May", "June", "July", "August", 
                                         "September", "October", "November", "December"), 
                              ordered = TRUE)
  
  pd_df_clean$dayofweek <- factor(pd_df_clean$dayofweek, 
                                  levels = c(
                                    "Monday", "Tuesday", "Wednesday", "Thursday",
                                    "Friday", "Saturday", "Sunday"
                                  ), 
                                  ordered = TRUE)
  
  pd_df_clean$hour <- factor(pd_df_clean$hour, levels = 0:23, labels = as.character(1:24), ordered = TRUE)
  
  # Add new binary and indexed columns for weekend and day of the week
  pd_df_clean <- pd_df_clean %>%
    mutate(
      # Create a binary column for is_weekend (1 for Weekday, 2 for Weekend)
      is_weekend_index = ifelse(is_weekend == "Weekend", 2, 1),
      
      # Create an index for day of the week (1 for Monday, 7 for Sunday)
      day_of_week_index = case_when(
        dayofweek == "Monday"    ~ 1,
        dayofweek == "Tuesday"   ~ 2,
        dayofweek == "Wednesday" ~ 3,
        dayofweek == "Thursday"  ~ 4,
        dayofweek == "Friday"    ~ 5,
        dayofweek == "Saturday"  ~ 6,
        dayofweek == "Sunday"    ~ 7
      )
    )
  
  # VIS 1
  # Prepare the data for the stacked bar plot
  avg_data <- reactive({
    x_var <- input$xAxisVar 
    time_dim <- input$timeDim
    
    # Create mappings for labels
    time_dict <- c(
      "Month" = "month", 
      "Day of the Week" = "dayofweek",
      "Weekends vs. Weekdays" = "is_weekend"
    )
    x_dict <- c("Suburb" = "suburb", "Development Site" = "dev_site")
    
    # Get the labels
    time_label <- names(time_dict)[which(time_dict == time_dim)]
    x_label <- names(x_dict)[which(x_dict == x_var)]
    
    # Group by the data by categories
    avg_df <- pd_df_clean %>%
      group_by(.data[[x_var]], .data[[time_dim]]) %>%
      summarise(avg_val = mean(val_calibrated, na.rm = TRUE)) %>%
      mutate(custom_text = paste0(
        x_label, ": ", .data[[x_var]], "<br>",
        time_label, ": ", .data[[time_dim]], "<br>",
        "Average Value: ", round(avg_val, 0)
      ))
    
    return(avg_df)
  })
  
  # Create a reactive list of unique values from suburb or dev_site
  unique_values <- reactive({
    x_var <- sym(input$xAxisVar)  
    avg_data() %>% 
      distinct(!!x_var) %>%       
      pull(!!x_var)               
  })
  
  # Reactive value to store selected suburbs or dev_sites
  selected_items <- reactiveVal(character())
  
  # Render the interactive stacked bar plot with Plotly
  output$interactiveBarPlot <- renderPlotly({
    
    # x_var will be a string ("suburb" or "dev_site")
    x_var <- input$xAxisVar  
    
    # time_dim will be a string of time dimension
    time_dim <- input$timeDim
    
    # Create a dictionaries to extract the label
    time_dict <- c(
      "Month" = "month", 
      "Day of the Week" = "dayofweek",
      "Weekends vs. Weekdays" = "is_weekend"
    )
    
    x_dict <- c("Suburb" = "suburb", "Development Site" = "dev_site")
    
    # Get the labels
    time_label <- names(time_dict)[which(time_dict == time_dim)]
    x_label <- names(x_dict)[which(x_dict == x_var)]
    
    # Dynamically select x-axis variable (either suburb or dev_site)
    p <- ggplot(avg_data(), aes_string(x = x_var, y = "avg_val", fill = time_dim, text = "custom_text")) +
      geom_bar(stat = "identity", position = "stack") +   
      geom_text(aes(label = round(avg_val, 0)), 
                position = position_stack(vjust = 0.5), 
                color = "white", size = 2.5) +   
      labs(x = x_label, y = "Average Pedestrian Count", fill = time_label) +
      theme_minimal()
    
    
    # Convert ggplot to a Plotly interactive plot, specifying the source and tooltip
    ggplotly(p, source = "bar", tooltip = "text") %>% 
      layout(hovermode = "closest")  # Enable hover mode
  })
  
  # Update selected suburbs or dev_sites when bars are clicked
  observeEvent(event_data("plotly_click", source = "bar"), {
    click_data <- event_data("plotly_click", source = "bar")
    
    if (!is.null(click_data)) {
      new_selection <- click_data$x
      current_selection <- selected_items()
      
      # Add or remove the selection
      if (new_selection %in% current_selection) {
        selected_items(setdiff(current_selection, new_selection))
      } else {
        selected_items(c(current_selection, new_selection))
      }
    }
  })
  
  # Show the selected suburbs or dev_sites
  output$selectedSuburbs <- renderText({
    selected <- selected_items()
    category_values <- unique_values()
    
    selected_category <- category_values[as.integer(selected)]
    
    if (length(selected) > 0) {
      paste("Selected:", paste(selected_category, collapse = ", "))
    } else {
      "Select a filter to explore data from the Bar Graph."
    }
  })
  
  # Clear the selection when the button is clicked
  observeEvent(input$clearSelection, {
    selected_items(character())
  })
  
  
  # VIS2
  # Observe changes in vis 2 and update vis2 options dynamically in the UI page
  observeEvent(input$StackedGraphdim, {
    # Get the current value of vis 2
    current_dim <- input$StackedGraphdim
    
    # Define the choices for vis 2, excluding the selected vis 2
    available_choices <- c(
      "Month" = "month_num", 
      "Day of the Week" = "day_of_week_index",
      "Date of Month" = "day",
      "Hourly" = "hour",
      "Weekends vs. Weekdays" = "is_weekend_index"
    )
    
    # Remove the current dimension from the choices
    updated_choices <- available_choices[available_choices != current_dim]
    
    # Update the selectInput for StackedGraphfill with the new choices
    updateSelectInput(session, "StackedGraphfill", choices = updated_choices)
  })
  
  # Render the stackedgraph with Plotly
  output$stackedGraphPlot <- renderPlotly({
    selected <- selected_items()
    category_values <- unique_values()
    
    time_dim <- input$StackedGraphdim
    fill_var <- input$StackedGraphfill
    
    # Create mappings for labels
    label_mappings <- list(
      "month_num" = list(
        labels = c("January", "February", "March", "April", 
                   "May", "June", "July", "August", 
                   "September", "October", "November", "December"),
        levels = 1:12
      ),
      "day_of_week_index" = list(
        labels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                   "Friday", "Saturday", "Sunday"),
        levels = 1:7
      ),
      "day" = list(
        labels = as.character(1:31),
        levels = 1:31
      ),
      "hour" = list(
        labels = as.character(1:24),
        levels = 1:24
      ),
      "is_weekend_index" = list(
        labels = c("Weekday", "Weekend"),
        levels = 1:2
      )
    )
    
    # Retrieve labels for time and fill variables
    time_label <- names(which(c(
      "Month" = "month_num", 
      "Day of the Week" = "day_of_week_index",
      "Date of Month" = "day",
      "Hourly" = "hour",
      "Weekends vs. Weekdays" = "is_weekend_index"
    ) == time_dim))
    
    fill_label <- names(which(c(
      "Month" = "month_num", 
      "Day of the Week" = "day_of_week_index",
      "Date of Month" = "day",
      "Hourly" = "hour",
      "Weekends vs. Weekdays" = "is_weekend_index"
    ) == fill_var))
    
    # Show full data if there is no selected categories
    if (length(selected) == 0){
      stream_data <- pd_df_clean %>%
        group_by(.data[[time_dim]], .data[[fill_var]]) %>%
        summarise(avg_val_calibrated = mean(val_calibrated, na.rm = TRUE), .groups = 'drop')
    } else {
      selected_category <- category_values[as.integer(selected)]
      x_var <- input$xAxisVar
      
      stream_data <- pd_df_clean %>%
        filter(.data[[x_var]] %in% selected_category) %>%
        group_by(.data[[time_dim]], .data[[fill_var]]) %>%
        summarise(avg_val_calibrated = mean(val_calibrated, na.rm = TRUE), .groups = 'drop')
    }
    
    # Map x values to labels
    x_labels <- label_mappings[[time_dim]]$labels
    x_levels <- label_mappings[[time_dim]]$levels
    
    # Map fill variable to labels
    fill_labels <- label_mappings[[fill_var]]$labels
    fill_levels <- label_mappings[[fill_var]]$levels
    
    # Ensure x and fill variables are factors with proper levels and labels
    stream_data[[time_dim]] <- factor(stream_data[[time_dim]], levels = x_levels, labels = x_labels)
    stream_data[[fill_var]] <- factor(stream_data[[fill_var]], levels = fill_levels, labels = fill_labels)
    
    # Create the stackedgraph using plotly
    p <- plot_ly()
    
    for (fill_level in levels(stream_data[[fill_var]])) {
      df_subset <- stream_data %>% filter(.data[[fill_var]] == fill_level)
      
      p <- add_trace(p,
                     x = df_subset[[time_dim]],
                     y = df_subset$avg_val_calibrated,
                     name = fill_level,
                     type = 'scatter',
                     mode = 'markers+text',  
                     textposition = 'top center',  
                     stackgroup = 'one',
                     text = round(df_subset$avg_val_calibrated, 0),  
                     hoverinfo = 'text',  
                     hovertemplate = paste0(
                       time_label, ": ", df_subset[[time_dim]], "<br>",
                       fill_label, ": ", fill_level, "<br>",
                       "Average Pedestrian Count: ", round(df_subset$avg_val_calibrated, 0), 
                       "<extra></extra>"
                     )  
      )
    }
    
    p <- p %>% layout(
      xaxis = list(
        title = time_label,
        range = c(-1, length(unique(df_subset[[time_dim]]))) 
      ),
      yaxis = list(title = "Average Pedestrian Count"),
      legend = list(title = list(text = fill_label)),
      hovermode = 'closest',
      transition = list(
        duration = 500,
        easing = "cubic-in-out"
      )
    )
    p
  })
  
  # VIS 3
  # Read in the shapefiles
  casey_ward_shp <- st_read("casey_ward/casey_wards_final.shp")
  casey_suburb_shp <- st_read("casey_ward/casey_suburb_final.shp")
  park_reserve_shp <- st_read("parks-and-reserves1/parks-and-reserves1.shp")
  park_reserve_shp <- na.omit(park_reserve_shp)
  
  # Transform CRS to WGS84 if needed
  casey_ward_shp <- st_transform(casey_ward_shp, crs = 4326)
  casey_suburb_shp <- st_transform(casey_suburb_shp, crs = 4326)
  park_reserve_shp <- st_transform(park_reserve_shp, crs = 4326)
  
  # Get the location of each sensor
  pd_df_sub_dev_site_loc <- pd_df_clean %>%
    group_by(suburb, dev_site) %>%
    summarise(
      ped_count = mean(val_calibrated),
      lat = mean(loc_lat),
      long = mean(loc_lng)
    )
  
  # Convert sensor locations to sf object for spatial operations
  sensor_sf <- st_as_sf(pd_df_sub_dev_site_loc, coords = c("long", "lat"), crs = 4326)
  
  # Calculate the nearest neighbor and distance for each point in park_reserve_shp
  distance_matrix <- st_distance(park_reserve_shp, sensor_sf)
  
  # Get the index of the nearest point in parks_within_buffer for each location in park_reserve_shp
  nearest_index <- apply(distance_matrix, 1, which.min)
  
  # Extract the corresponding minimum distance
  min_distance <- apply(distance_matrix, 1, min)
  
  # Palette function for color assignment based on pedestrian count
  palette <- function(x) {
    # Custom palette with distinguishable shades of blue from light to dark
    color_numeric <- colorNumeric(palette = c("#87CEFA", "#4682B4", "#1E90FF", "#00008B"), domain = as.numeric(x))
    ifelse(x == 0, "gray", color_numeric(x))
  }
  
  # Render the interactive Leaflet map
  output$InteractiveMap <- renderLeaflet({
    radius <- input$AreaRadius
    
    # Create buffers (radius in meters, e.g., 1000m = 1km)
    sensor_buffers <- st_buffer(sensor_sf, dist = as.integer(radius))  # Adjust buffer size as needed
    
    # Spatial join to count parks within the sensor buffer
    parks_within_buffer <- st_join(sensor_buffers, park_reserve_shp, join = st_contains) %>%
      group_by(dev_site, suburb.x) %>%
      summarise(parks_count = n()) %>%
      right_join(pd_df_sub_dev_site_loc %>% select(-suburb), by = "dev_site") %>%
      select(-suburb.x)
    
    # Add the nearest suburb and ped_count to park_reserve_shp if distance is <= 1km (1000 meters)
    park_reserve_shp <- park_reserve_shp %>%
      mutate(
        closest_suburb = ifelse(min_distance <= as.integer(radius), parks_within_buffer$suburb[nearest_index], "No nearby Sensor"),
        closest_ped_count = ifelse(min_distance <= as.integer(radius), parks_within_buffer$ped_count[nearest_index], 0)
      )
    
    # Plotting starts here
    leaflet() %>%
      addTiles() %>%
      
      # Add Casey suburbs shapefile with the number of parks in each suburb
      addPolygons(data = casey_suburb_shp,
                  color = "green", weight = 2, opacity = 0.5, fillOpacity = 0.1,
                  highlightOptions = highlightOptions(
                    color = "yellow", weight = 3, bringToFront = TRUE, fillOpacity = 0.3),
                  popup = ~paste("Suburb:", suburb, "<br>Number of Parks:", park_count),  
                  group = "Casey Suburbs") %>%
      
      # Add Casey wards shapefile with the number of parks in each ward
      addPolygons(data = casey_ward_shp,
                  color = "brown", weight = 2, opacity = 0.5, fillOpacity = 0.1,
                  highlightOptions = highlightOptions(
                    color = "yellow", weight = 3, bringToFront = TRUE, fillOpacity = 0.3),  
                  popup = ~paste("Ward Name:", WARD_NAME, "<br>Number of Parks:", park_count),  
                  group = "Casey Wards") %>%
      
      # Add circle markers for Park and Reserve points with fixed radius
      addCircleMarkers(
        data = as.data.frame(park_reserve_shp %>% select(name, latitude, longitude, closest_suburb, closest_ped_count)),
        lat = ~latitude, lng = ~longitude, 
        radius = 1.5,  
        stroke = FALSE,
        fillColor = ~palette(as.numeric(closest_ped_count)), 
        fillOpacity = 4, 
        popup = ~paste("Park and Reserve Name:", name,
                       ifelse(closest_ped_count == "No nearby Sensor", "", 
                              paste("<br>Pedestrian Count:", round(as.numeric(closest_ped_count), 0)))
        ), 
        group = "Park and Reserve Location"  
      ) %>%
      
      # Add Park and Reserve shapefile with hover and click interactions
      addPolygons(data = park_reserve_shp,
                  color = "gray", weight = 2, opacity = 1, fillOpacity = 0.1,
                  highlightOptions = highlightOptions(
                    color = "yellow", weight = 3, bringToFront = TRUE, fillOpacity = 0.7),  
                  popup = ~paste("Park and Reserve Name:", name,
                                 ifelse(closest_ped_count == "No nearby Sensor", "", 
                                        paste("<br>Pedestrian Count:", round(as.numeric(closest_ped_count), 0)))
                  ),
                  group = "Park and Reserve Map") %>%
      
      # Add circle markers for sensors with parks count in popup
      addCircleMarkers(
        data = parks_within_buffer,
        lat = ~lat, lng = ~long,  
        radius = 3,  
        color = "red",
        fillColor = "red",  
        fillOpacity = 2,  
        popup = ~paste(
          "<br>Suburb:", suburb,
          "<b>Development Site:</b>", dev_site, 
          "<br>Number of Parks within 1km:", parks_count,
          "<br>Number of Average Pedestrian Count Daily:", round(ped_count, 0)
        ),  
        group = "Sensor"  
      ) %>%
      
      addPolygons(data = parks_within_buffer,
                  color = "red", weight = 2, opacity = 0.5, fillOpacity = 0.1,
                  highlightOptions = highlightOptions(
                    color = "yellow", weight = 2, bringToFront = TRUE, fillOpacity = 0.1), 
                  popup = ~paste(
                    "<br>Suburb:", suburb,
                    "<b>Development Site:</b>", dev_site, 
                    "<br>Number of Parks within 1km:", parks_count,
                    "<br>Number of Average Pedestrian Count Daily:", round(ped_count, 0)
                  ),
                  group = "Sensor Radius") %>%
      
      # Add layer control to toggle layers
      addLayersControl(
        overlayGroups = c("Park and Reserve Location", "Park and Reserve Map", "Casey Wards", "Casey Suburbs", "Sensor", "Sensor Radius"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      showGroup(c("Park and Reserve Location", "Sensor")) %>%
      hideGroup(c("Casey Wards", "Park and Reserve Map", "Casey Suburbs", "Sensor Radius"))
  })
  
  # Color legend
  output$map_legend <- renderUI({
    tagList(
      div(style = "margin-top: 10px; display: flex; justify-content: center;", 
          div(style = "text-align: left;",  
              div(style = "display: flex; align-items: center; margin-bottom: 5px;",
                  div(style = "background-color: #87CEFA; width: 50px; height: 20px; margin-right: 10px;"),
                  span("Low Count")
              ),
              div(style = "display: flex; align-items: center; margin-bottom: 5px;",
                  div(style = "background-color: #4682B4; width: 50px; height: 20px; margin-right: 10px;"),
                  span("Medium-Low Count")
              ),
              div(style = "display: flex; align-items: center; margin-bottom: 5px;",
                  div(style = "background-color: #1E90FF; width: 50px; height: 20px; margin-right: 10px;"),
                  span("Medium-High Count")
              ),
              div(style = "display: flex; align-items: center; margin-bottom: 5px;",
                  div(style = "background-color: #00008B; width: 50px; height: 20px; margin-right: 10px;"),
                  span("High Count")
              ),
              div(style = "display: flex; align-items: center; margin-bottom: 5px;",
                  div(style = "background-color: gray; width: 50px; height: 20px; margin-right: 10px;"),
                  span("No Count")
              )
          )
      )
    )
  })
  
}
