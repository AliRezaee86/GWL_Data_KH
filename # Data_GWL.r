# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(openxlsx)
library(tidyr)
library(leaflet)
library(sf)
library(mapview)  # For mapshot

# Load data
Pre_Data <- read_excel("rsconnect/shinyapps.io/pcyejp-ali-rezaee/Pre_Data.xlsx")
Well_Info <- read_excel("rsconnect/shinyapps.io/pcyejp-ali-rezaee/Well_Point2.xlsx")
aquifer_shapefile <- st_read("rsconnect/shinyapps.io/pcyejp-ali-rezaee/Aquifer_Clip_Union.shp")

# Validate and transform aquifer shapefile
if (any(!st_is_valid(aquifer_shapefile))) {
  aquifer_shapefile <- st_make_valid(aquifer_shapefile)
}
aquifer_shapefile <- st_transform(aquifer_shapefile, crs = 4326)

# Define UI
ui <- fluidPage(
  titlePanel("Piezometric Well Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("aquifer", "Select an Aquifer:", choices = c("All Aquifers", unique(Well_Info$AQ))),
      selectizeInput("wells", "Select Wells:", choices = NULL, multiple = TRUE, options = list(placeholder = 'Type to search wells...')),
      actionButton("update", "Update"),
      downloadButton("downloadData", "Download Excel"),
      downloadButton("downloadFigures", "Download Figures")
    ),
    
    mainPanel(
      verbatimTextOutput("well_info"),
      uiOutput("well_tables"),  
      plotOutput("scatter_plot"),
      plotOutput("histogram_plot"),
      leafletOutput("map")  
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Update well choices based on selected aquifer
  observeEvent(input$aquifer, {
    if (input$aquifer == "All Aquifers") {
      # Include all wells if "All Aquifers" is selected
      wells_in_aquifer <- Well_Info %>% pull(Name_Well)
    } else {
      # Filter wells based on the selected aquifer
      wells_in_aquifer <- Well_Info %>% filter(AQ == input$aquifer) %>% pull(Name_Well)
    }
    updateSelectizeInput(session, "wells", choices = wells_in_aquifer, server = TRUE)
  })
  
  # Function to clean data (remove zero and outliers using IQR)
  clean_data <- function(data) {
    data <- data %>% filter(WaterDepth != 0)
    
    Q1 <- quantile(data$WaterDepth, 0.25, na.rm = TRUE)
    Q3 <- quantile(data$WaterDepth, 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1
    data <- data %>%
      filter(WaterDepth >= (Q1 - 1.5 * IQR_value) & WaterDepth <= (Q3 + 1.5 * IQR_value))
    return(data)
  }
  
  well_data <- eventReactive(input$update, {
    req(input$wells)
    selected_data <- Pre_Data %>% 
      select(Year, Month, all_of(input$wells)) %>%
      pivot_longer(cols = -c(Year, Month), names_to = "Well", values_to = "WaterDepth") %>%
      filter(!is.na(WaterDepth))  
    
    cleaned_data <- clean_data(selected_data)
    return(cleaned_data)
  })
  
  well_info_details <- eventReactive(input$update, {
    req(input$wells)
    well_details <- Well_Info %>% filter(Name_Well %in% input$wells)
    well_details <- well_details %>% filter(!is.na(Lat) & !is.na(Lon) & !is.na(AQ))
    return(well_details)
  })
  
  output$well_tables <- renderUI({
    req(well_data(), well_info_details())
    
    well_tables <- lapply(input$wells, function(well) {
      well_data_subset <- well_data() %>% filter(Well == well)
      stats <- data.frame(
        "Mean" = mean(well_data_subset$WaterDepth, na.rm = TRUE),
        "Standard Deviation" = sd(well_data_subset$WaterDepth, na.rm = TRUE),
        "Median" = median(well_data_subset$WaterDepth, na.rm = TRUE),
        "Data Points" = nrow(well_data_subset)
      )
      
      well_info <- well_info_details() %>% filter(Name_Well == well) %>% select(Name_Well, Lat, Lon, AQ)
      
      final_table <- cbind(well_info, stats)
      
      tagList(
        h4(paste("Well: ", well)),
        tableOutput(outputId = paste0("table_", well))
      )
    })
    
    do.call(tagList, well_tables)
  })
  
  # Render tables for each well
  observe({
    req(input$wells)
    lapply(input$wells, function(well) {
      output[[paste0("table_", well)]] <- renderTable({
        well_data_subset <- well_data() %>% filter(Well == well)
        stats <- data.frame(
          "Mean" = mean(well_data_subset$WaterDepth, na.rm = TRUE),
          "Standard Deviation" = sd(well_data_subset$WaterDepth, na.rm = TRUE),
          "Median" = median(well_data_subset$WaterDepth, na.rm = TRUE),
          "Data Points" = nrow(well_data_subset)
        )
        
        well_info <- well_info_details() %>% filter(Name_Well == well) %>% select(Name_Well, Lat, Lon, AQ)
        
        final_table <- cbind(well_info, stats)
        final_table
      })
    })
  })
  
  output$scatter_plot <- renderPlot({
    req(well_data())
    
    scatter_plot <- ggplot(well_data(), aes(x = as.Date(paste(Year, Month, "01", sep = "-")), y = WaterDepth)) +
      geom_point(aes(color = Well)) +
      facet_wrap(~Well, scales = "free_y") +
      labs(title = "Scatter Plot of Water Depth for Selected Wells", x = "Date", y = "Water Depth") +
      theme_minimal()
    
    ggsave("scatter_plot.png", scatter_plot, width = 10, height = 8, dpi = 300)
    print(scatter_plot)
  })
  
  output$histogram_plot <- renderPlot({
    req(well_data())
    
    histogram_plot <- ggplot(well_data(), aes(x = WaterDepth)) +
      geom_histogram(aes(fill = after_stat(count)), binwidth = 1, color = "black") +
      facet_wrap(~Well, scales = "free_y") +
      labs(title = "Histogram of Water Depth for Selected Wells", x = "Water Depth", y = "Frequency") +
      theme_minimal()
    
    ggsave("histogram_plot.png", histogram_plot, width = 10, height = 8, dpi = 300)
    print(histogram_plot)
  })
  
  output$map <- renderLeaflet({
    req(input$wells)
    
    selected_wells <- well_info_details()
    
    map_plot <- leaflet() %>%
      addTiles() %>%
      addPolygons(data = aquifer_shapefile, color = "blue", weight = 2, opacity = 0.5, fillOpacity = 0.2, popup = "Aquifer Range") %>%
      addMarkers(data = selected_wells, lat = ~Lat, lng = ~Lon, popup = ~Name_Well)
    
    mapshot(map_plot, file = "map_plot.png", zoom = 2)  # Save map as image with high resolution
    map_plot
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { "Selected_Well_Data.xlsx" },
    content = function(file) {
      well_data_df <- well_data()
      well_info_df <- well_info_details()
      
      final_data <- left_join(well_data_df, well_info_df, by = c("Well" = "Name_Well"))
      write.xlsx(final_data, file)
    }
  )
  
  output$downloadFigures <- downloadHandler(
    filename = function() { "Figures.zip" },
    content = function(file) {
      zip(file, files = c("scatter_plot.png", "histogram_plot.png", "map_plot.png"))
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)