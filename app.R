# Put the tasks in .qmd into a shiny app. 
library(shiny)
library(dplyr)

# Load Melbourne housing dataset
mh_data <- read.csv("C:/Users/tangw1/Desktop/ST558_repo/proj2/MELBOURNE_HOUSE_PRICES_LESS.csv")

# Define the UI function. There are two widgets for categorical variables "type" and "region"; also included is a dynamic slider for the numeric variable "price"
ui <- fluidPage(
  titlePanel("Melbourne Housing Data App"),

  sidebarLayout(
    sidebarPanel(
      # 1. add checkbox widgets for two categorical variables for users to subset
      checkboxGroupInput("type", "Select Property Type:", choices = unique(mh_data$Type), selected = unique(mh_data$Type)),
      checkboxGroupInput("region", "Select Region:", choices = unique(mh_data$Regionname), selected = unique(mh_data$Regionname)),

      # 2. add dynamic slider for first numeric variable for users to subset on the variable
      selectInput("numeric1", "Select Numeric Variable 1 for filtering:", choices = c("Price", "Propertycount", "Distance")),
      uiOutput("slider1"),

      # 3. add dynamic slider for second numeric variable for users to subset 
      selectInput("numeric2", "Select Numeric Variable 2 for filtering:", choices = c("Price", "Propertycount", "Distance")),
      uiOutput("slider2"),


      #  add an action button
      actionButton("apply_filters", "Apply Filters")
    ),

    mainPanel(
      # Main panel to display the filtered data or subsequent outputs
      textOutput("subset_message")
    )
  )
)

server <- function(input, output, session) {
  # Reactive values to store the filtered data
  filtered_data <- reactiveValues(data = mh_data)

  # Dynamically update the first slider based on the selected numeric variable
  output$slider1 <- renderUI({
    req(input$numeric1)
    min_val <- min(mh_data[[input$numeric1]], na.rm = TRUE)
    max_val <- max(mh_data[[input$numeric1]], na.rm = TRUE)
    sliderInput("slider1", label = paste("Range for", input$numeric1), min = min_val, max = max_val,
                value = c(min_val, max_val))
  })

  # Dynamically update the second slider based on the selected numeric variable
  output$slider2 <- renderUI({
    req(input$numeric2)
    min_val <- min(mh_data[[input$numeric2]], na.rm = TRUE)
    max_val <- max(mh_data[[input$numeric2]], na.rm = TRUE)
    sliderInput("slider2", label = paste("Range for", input$numeric2), min = min_val, max = max_val,
                value = c(min_val, max_val))
  })

  # Observe the action button to filter the data when clicked
  observeEvent(input$apply_filters, {
    # Apply filters based on sidebar inputs
    filtered_data$data <- mh_data %>%
      filter(
        Type %in% input$type,
        Regionname %in% input$region,
        between(get(input$numeric1), input$slider1[1], input$slider1[2]),
        between(get(input$numeric2), input$slider2[1], input$slider2[2])
      )

    output$subset_message <- renderText({
      paste("Data subset applied. Rows in subset:", nrow(filtered_data$data))
    })
  })
}


# Run the app
shinyApp(ui = ui, server = server)
















