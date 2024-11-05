# Put the tasks in .qmd into a shiny app. 
library(shiny)
library(dplyr)
library(DT)

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
      tabsetPanel(
        # Add About tab 
        tabPanel("About",
                 h3("About the Melbourne Housing Data Subset App"),
                 p("This app allows users to explore and analyze housing market data in Melbourne from 2016-18."),
                 p("The dataset provides insights into property types, prices, distances to the central business district (CBD), and other characteristics of the Melbourne housing market."),
                 p("Source: [Melbourne Housing Market Data](https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market)"),
                 
                 h4("Sidebar Overview"),
                 p("The sidebar contains filtering options to subset the data by property type and region, as well as range sliders for numeric variables price and distance."),
                 
                 h4("Tabs Overview"),
                 p("Each tab in the app offers different functions. This 'About' tab describes the app, the dataset, and its purpose. The 'Data Download' tab allows users to download the subsetted data."),
                 
                 # Add an image related to Melbourne housing (image is saved in 'www' folder under working directory)
                 img(src = "melbourneh.png", height = "300px", alt = "Melbourne Housing Market")
        ),
        
        # Data Download tab
        tabPanel("Data Download",
                 h3("Download Subsetted Data"),
                 DT::dataTableOutput("table"),
                 downloadButton("downloadData", "Download Subsetted Data as CSV")
#                p("This tab will allow users to download the subsetted data based on their selected filters.")
                 
        )
      )
    )
  )
)

# Define server
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

# the following output originally coded is removed based on the second part of "App Requirements" where the subsetted data shall be displayed, not just the number of obs.
  # show number of obs after user subsetting
    # output$subset_message <- renderText({
    #   paste("Data subset applied. Rows in subset:", nrow(filtered_data$data))
    # })
  })
  
  # Display the subsetted dataset
  output$table <- DT::renderDataTable({
    DT::datatable(filtered_data$data)
  })
  # add a download button
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("subsetted_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data$data, file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
















