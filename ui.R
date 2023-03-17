source("global.R")

ui <- fluidPage(
  theme = shinytheme("slate"),
  titlePanel("Descriptive Analysis of NEET Index in Italy (2010-2020)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Year:", choices = unique(neet_data$Year), selected = 2020),
      selectInput("territory", "Territory:", choices = c(unique(neet_data$Territory)), selected = "Italy"),
      selectInput("gender", "Gender:", choices = c("All" = "All", unique(neet_data$Gender))),
      selectInput("age_group", "Age Group:", choices = unique(neet_data$Age_class), selected = '15-29 years'),
      checkboxGroupInput("data_display", "Data Display:", choices = c("Table", "Bar Chart", "Line Chart"), selected = c("Table", "Bar Chart", "Line Chart"))
    ),
    mainPanel(
      tableOutput("neet_table") %>% withSpinner(),
      plotOutput("neet_chart") %>% withSpinner(),
      plotOutput("neet_line_chart") %>% withSpinner(),
      plotOutput("neet_map", height = 500) %>% withSpinner()
    )
  )
)
