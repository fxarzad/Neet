source("global.R")
source("ui.R")


server <- function(input, output) {
  filtered_data <- reactive({
    data <- neet_data
    if (input$gender != "All") {
      data <- data[data$Gender == input$gender, ]
      neet_map_data = neet_map_data[neet_map_data$Gender == input$gender,]
    }
    if (input$territory != "All") {
      data <- data[data$Territory == input$territory, ]
    }
    data <- data[data$Year == input$year & data$Age_class == input$age_group, ]
    
    neet_map_data <- neet_map_data %>%
      filter(Year == input$year & Age_class == input$age_group)
    
    
    gadm_sf_merged <- left_join(gadm_shapes@data,neet_map_data,by = c('NAME_1'='Standard_Territory'))
    
    return(list(table_data = data, gadm_sf_merged = gadm_sf_merged))
  })
  
  
  
  
  output$neet_table <- renderTable({
    if ("Table" %in% input$data_display) {
      filtered_data()$table_data
    } else {
      NULL
    }
  })
  
  output$neet_chart <- renderPlot({
    if ("Bar Chart" %in% input$data_display || "Line Chart" %in% input$data_display) {
      chart_type <- ifelse("Bar Chart" %in% input$data_display, "bar", "line")
      ggplot(filtered_data()$table_data, aes(x = Gender, y = Value, group = Gender, fill = Gender)) +
        geom_col() +
        scale_fill_manual(values = c("males" = "#3498db", "females" = "#e74c3c")) +
        labs(title = "NEET Index in Italy", x = "Gender", y = "NEET Percentage") +
        theme_classic() +
        theme(plot.title = element_text(color = "#ecf0f1"),
              axis.title = element_text(color = "#ecf0f1"),
              panel.background = element_rect(fill = "#2c3e50"), # Change panel background color
              plot.background = element_rect(fill = "#2c3e50"),
              legend.background = element_blank(),
              legend.text = element_text(color = "#ecf0f1"),
              legend.title = element_text(color = "#ecf0f1", face = "bold"))
    } else {
      NULL
    }
  })
  
  output$neet_line_chart <- renderPlot({
    if ("Line Chart" %in% input$data_display) {
      line_data <- neet_data %>%
        filter(Age_class == input$age_group) %>%
        group_by(Year, Gender) %>%
        summarize(Value = mean(Value, na.rm = TRUE))
      
      ggplot(line_data, aes(x = Year, y = Value, color = Gender, group = Gender)) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        scale_color_manual(values = c("males" = "#3498db", "females" = "#e74c3c")) + 
        labs(title = "NEET Index Trend in Italy", x = "Year", y = "NEET Percentage") +
        theme_classic() +
        theme(plot.title = element_text(color = "#ecf0f1"),
              axis.title = element_text(color = "#ecf0f1"),
              panel.background = element_rect(fill = "#2c3e50"),
              plot.background = element_rect(fill = "#2c3e50"),
              legend.background = element_rect(fill = "#2c3e50"),
              legend.text = element_text(color = "#ecf0f1"),
              legend.title = element_text(color = "#ecf0f1", face = "bold"))
    } else {
      NULL
    }
  })
  
  
  output$neet_map <- renderPlot({
    gadm_sf_merged <- gadm_shapes
    gadm_sf_merged@data <- filtered_data()$gadm_sf_merged
    
    
    if (all(is.na(gadm_sf_merged@data$Value))) {
      plot.new()
      title("NEET Map - No Data Available")
      return()
    }
    
    
    pal <- colorRampPalette(c("blue", "yellow", "red"))
    
    
    breaks <- seq(min(gadm_sf_merged@data$Value, na.rm = TRUE), max(gadm_sf_merged@data$Value, na.rm = TRUE), length.out = 5)
    
    
    col_codes <- findInterval(gadm_sf_merged@data$Value, breaks, all.inside = TRUE)
    col_codes <- pal(5)[col_codes]
    
    
    col_codes[is.na(gadm_sf_merged@data$Value)] <- "gray"
      
    spplot(gadm_sf_merged, "Value",
           col.regions = col_codes,
           main = "NEET Map",
           scales = list(draw = TRUE),
           colorkey = list(space = "right", width = 1, height = 0.5, labels = list(at = breaks, labels = round(breaks, 1)), col = pal(5)),
           par.settings = black_bg_theme)
  })
  
  
  
  
  
}