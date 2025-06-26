# Piezometric Well Data Analysis Tool
# This Shiny application provides visualization and initial evaluation of water depth data from piezometric wells.


library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(openxlsx)
library(tidyr)
library(sf)

# Load data
Pre_Data <- read_excel("data/Pre_Data.xlsx")
Well_Info <- read_excel("data/Well_Point2.xlsx")
aquifer_shapefile <- st_read("data/Aquifer_Clip_Union.shp")

# Ensure shapefile validity
if (any(!st_is_valid(aquifer_shapefile))) {
  cat("Fixing invalid geometries in shapefile...\n")
  aquifer_shapefile <- st_make_valid(aquifer_shapefile)
}

# Transform to WGS 1984 (EPSG:4326)
aquifer_shapefile <- st_transform(aquifer_shapefile, crs = 4326)

# Define UI
ui <- fluidPage(
  titlePanel("Piezometric Well Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("aquifer", "Select an Aquifer:", choices = unique(Well_Info$AQ)),
      selectInput("wells", "Select Wells:", choices = NULL, multiple = TRUE),
      actionButton("update", "Update"),
      downloadButton("downloadData", "Download Excel")
    ),
    
    mainPanel(
      verbatimTextOutput("well_info"),
      uiOutput("well_tables"),
      plotOutput("scatter_plot"),
      plotOutput("histogram_plot"),
      plotOutput("map")  # Updated map output using ggplot2
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Update well list based on selected aquifer
  observeEvent(input$aquifer, {
    wells_in_aquifer <- Well_Info %>% filter(AQ == input$aquifer) %>% pull(Name_Well)
    updateSelectInput(session, "wells", choices = wells_in_aquifer)
  })
  
  # Function to clean data (remove zero and outliers using IQR)
  clean_data <- function(data) {
    data <- data %>% filter(WaterDepth != 0)  # Remove zero values
    Q1 <- quantile(data$WaterDepth, 0.25, na.rm = TRUE)
    Q3 <- quantile(data$WaterDepth, 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1
    data <- data %>% 
      filter(WaterDepth >= (Q1 - 1.5 * IQR_value) & WaterDepth <= (Q3 + 1.5 * IQR_value))  # Remove outliers
    return(data)
  }
  
  # Get well data when update button is clicked
  well_data <- eventReactive(input$update, {
    req(input$wells)
    selected_data <- Pre_Data %>% 
      select(Year, Month, all_of(input$wells)) %>%
      pivot_longer(cols = -c(Year, Month), names_to = "Well", values_to = "WaterDepth") %>%
      filter(!is.na(WaterDepth))
    
    # Clean the data
    cleaned_data <- clean_data(selected_data)
    return(cleaned_data)
  })
  
  # Get well information for selected wells
  well_info_details <- eventReactive(input$update, {
    req(input$wells)
    Well_Info %>% filter(Name_Well %in% input$wells) %>% filter(!is.na(Lat) & !is.na(Lon) & !is.na(AQ))
  })
  
  # Display summary statistics for each well
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
      
      output[[paste0("table_", well)]] <- renderTable({ final_table })
      
      tagList(
        h4(paste("Well: ", well)),
        tableOutput(paste0("table_", well))
      )
    })
    
    do.call(tagList, well_tables)
  })
  
  # Scatter plot
  output$scatter_plot <- renderPlot({
    req(well_data())
    ggplot(well_data(), aes(x = as.Date(paste(Year, Month, "01", sep = "-")), y = WaterDepth, color = Well)) +
      geom_point() + 
      facet_wrap(~Well, scales = "free_y") +
      labs(title = "Scatter Plot of Water Depth", x = "Date", y = "Water Depth") +
      theme_minimal()
  })
  
  # Histogram
  output$histogram_plot <- renderPlot({
    req(well_data())
    ggplot(well_data(), aes(x = WaterDepth, fill = Well)) +
      geom_histogram(binwidth = 1, color = "black") +
      facet_wrap(~Well, scales = "free_y") +
      labs(title = "Histogram of Water Depth", x = "Water Depth", y = "Frequency") +
      theme_minimal()
  })
  
  # Render aquifer map with well locations
  output$map <- renderPlot({
    req(input$wells)
    
    selected_wells <- well_info_details()
    
    if (nrow(selected_wells) > 0) {
      ggplot(data = aquifer_shapefile) +
        geom_sf(fill = "lightblue", color = "blue", alpha = 0.5) +
        geom_point(data = selected_wells, aes(x = Lon, y = Lat, color = AQ), size = 3) +
        geom_text(data = selected_wells, aes(x = Lon, y = Lat, label = Name_Well), 
                  hjust = -0.1, vjust = -0.5, size = 3, color = "black") +
        theme_minimal() +
        labs(title = "Aquifer and Well Locations")
    } else {
      ggplot(data = aquifer_shapefile) +
        geom_sf(fill = "lightblue", color = "blue", alpha = 0.5) +
        labs(title = "Aquifer Map", subtitle = "No valid wells selected") +
        theme_minimal()
    }
  })
  
  # Download selected well data as Excel file
  output$downloadData <- downloadHandler(
    filename = function() { "Selected_Well_Data.xlsx" },
    content = function(file) {
      well_data_df <- well_data()
      well_info_df <- well_info_details()
      final_data <- left_join(well_data_df, well_info_df, by = c("Well" = "Name_Well"))
      write.xlsx(final_data, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)




