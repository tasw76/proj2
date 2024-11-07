# This R script puts the tasks completed in the Melbourne.qmd file into a shiny app. 
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(shinycssloaders)

# Load Melbourne housing dataset
mh_data <- read.csv("MELBOURNE_HOUSE_PRICES_LESS.csv") 

# Define the UI function. I set up two widgets for categorical variables "type" and "region"; also included is a dynamic slider for the numeric variable "price"
ui <- fluidPage(
  titlePanel("Melbourne Housing Data App"),

  sidebarLayout(
    sidebarPanel(
      # 1. add checkbox widgets for two categorical variables for users to subset from. Users can select one or more levels
      checkboxGroupInput("type", "Select Property Type:", choices = unique(mh_data$Type), selected = unique(mh_data$Type)),
      checkboxGroupInput("region", "Select Region:", choices = unique(mh_data$Regionname), selected = unique(mh_data$Regionname)),

      # 2. add dynamic slider for first numeric variable for users to subset on the variable
      selectInput("numeric1", "Select Numeric Variable 1 for filtering:", choices = c("Price", "Propertycount", "Distance")),
      uiOutput("slider1"),

      # 3. add dynamic slider for second numeric variable for users to subset from
      selectInput("numeric2", "Select Numeric Variable 2 for filtering:", choices = c("Price", "Propertycount", "Distance")),
      uiOutput("slider2"),

      #  add an action button
      actionButton("apply_filters", "Apply Filters")
    ),

    mainPanel(
      tabsetPanel(
        # Add About tab. On this tab, I describe the purpose of the app, briefly discuss the data and source. I also tell the user the purpose of the slider and each tab in the app.
        tabPanel("About",
                 h3("About the Melbourne Housing Data Subset App"),
                 p("This app allows users to explore and analyze housing market data in Melbourne for years 2016-18."),
                 p("The dataset provides insights into property types, prices, distances to the central business district (CBD), and other characteristics of the Melbourne housing market."),
                 p("Source: [Melbourne Housing Market Data](https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market)"),
                 
                 h4("Sidebar Overview"),
                 p("The sidebar contains filtering options to subset the data by property type and region, as well as range sliders for numeric variables Price, Propertycount, and Distance."),
                 
                 h4("Tabs Overview"),
                 p("Each tab in the app offers different functions. This 'About' tab describes the app, the dataset, and its purpose. The 'Data Download' tab allows users to download the subsetted data. The 'Data Exploration' tab allows users to perform several preliminary analyses and plotting of the data"),
                 
                 # Add an image related to Melbourne housing (I save the image in 'www' folder under working directory)
                 img(src = "melbourneh.png", height = "300px", alt = "Melbourne Housing Market")
        ),
        
        # add a Data Download tab, it lets the user download subsetted data to a .csv file.
        tabPanel("Data Download",
                 h3("Download Subsetted Data"),
                 
        # add a dynamic text message for row count. (Note.. try to show the number of missing values in Server code)
                 h4("Data Summary"),
                 textOutput("download_data_summary"),
                 
                 DT::dataTableOutput("table"),
                 downloadButton("downloadData", "Download Subsetted Data as CSV")
# removed:  p("This tab will allow users to download the subsetted data based on their selected filters.")
        ),

        # add a Data Exploration tab. I choose the subtabs approach to show users each functionality. Two subtabs are created: 'categorical summaries' lets the users to select categorical variable(s) and provides a one-way contingency table or two-way. 
        tabPanel("Data Exploration",
                h3("Explore Subsetted Data"),
         
            # Subtabs for categorical and numerical summaries
            tabsetPanel(
              # Categorical Summaries
              tabPanel("Categorical Summaries",
               selectInput("categorical_var", "Select Categorical Variable:", choices = names(mh_data)[sapply(mh_data, function(x) is.factor(x)||is.character(x))]),
                    
               selectInput("two_way_var", "Select Variable for Two-Way Table (Optional):", choices = c("None", names(mh_data)[sapply(mh_data, function(x) is.factor(x)||is.character(x))])),
                    
                    h4("One-Way Contingency Table"),
                    verbatimTextOutput("one_way_table"),
                    
                    h4("Two-Way Contingency Table"),
                    verbatimTextOutput("two_way_table")
           ),
           
            # Numerical Summaries
            tabPanel("Numerical Summaries",
                    selectInput("numeric_summary_var", "Select Numeric Variable:", choices = names(mh_data)[sapply(mh_data, is.numeric)]),
                    
                    h4("Summary Statistics"),
                    verbatimTextOutput("numeric_summary"),
                    
                    h4("Plots"),
                    plotOutput("numeric_plot")
            ),
           
           # First plot output. Added loading spinner for this plot to load. This plot take a bit of time to load
           tabPanel("Plot 1 - Numerical variables relationship",
                    h4("Select Variables for Graph"),
                    selectInput("x_var", "Select X-axis Variable:", choices = c("Distance", "Propertycount", "Price")),
                    selectInput("y_var", "Select Y-axis Variable:", choices = c("Price", "Propertycount", "Distance")),
                    
                    withSpinner(plotOutput("price_distance_plot"))
            ),
           
           # Added loading spinner for this plot to load. This plot is relatively quicker. As the dataset I'm working with here is small, loading spinner does not make quite bit of a difference. But it will be helpful if I deal with larger datasets. 
           tabPanel("Plot 2 - Housing price distribution",
                    h4("Boxplot of Price by Region and Property Type"),
                    withSpinner(plotOutput("price_region_boxplot"))
            ),  
           
           tabPanel("Plot 3 - Property Count Distribution",
                    h4("Histogram of Property Count by Suburb and Property Type"),
                    plotOutput("property_count_distribution")
            ),
           
           tabPanel("Plot 4 - Rooms by Region",
                    h4("Number of Rooms by Property Type Across Regions"),
                    plotOutput("rooms_by_region")
            ),
           
           tabPanel("Plot 5 - Price Heatmap",
                    h4("Average Property Price by Type and Region"),
                    plotOutput("price_heatmap")
            ), 
           
           tabPanel("Plot 6 - Price Over Time",
                    h4("Average Property Price Over Time by Type"),
                    plotOutput("price_over_time")
            )
          )
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

    # Dynamic text to display a summary of the filtered data in the Data Download tab
    output$download_data_summary <- renderText({
      # Calculate the number of rows in the subsetted data
      num_rows <- nrow(filtered_data$data)
      
      # Calculate the number of missing values for specific variables
      missing_price <- sum(is.na(filtered_data$data$Price))
      missing_rooms <- sum(is.na(filtered_data$data$Rooms))
      missing_distance <- sum(is.na(filtered_data$data$Distance))
      
      # Create a dynamic message with row count and missing values summary
      paste(
        "The current subset contains", num_rows, "properties.",
        "Missing values: Price =", missing_price, 
        ", Rooms =", missing_rooms, 
        ", Distance =", missing_distance
      )
    })
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
  
  # Display categorical summaries
  output$one_way_table <- renderPrint({
    req(input$categorical_var)
    table(filtered_data$data[[input$categorical_var]])
  })
  
  output$two_way_table <- renderPrint({
    req(input$categorical_var, input$two_way_var != "None")
    table(filtered_data$data[[input$categorical_var]], filtered_data$data[[input$two_way_var]])
  })
  
  # Display numerical summaries
  output$numeric_summary <- renderPrint({
    req(input$numeric_summary_var)
  #   summary(filtered_data$data[[input$numeric_summary_var]])
  # })

  # Calculate standard summary statistics and standard deviation
  summary_stats <- summary(filtered_data$data[[input$numeric_summary_var]])
  sd_value <- sd(filtered_data$data[[input$numeric_summary_var]], na.rm = TRUE)
  
  # Combine and print results
  cat("Summary Statistics:\n")
  print(summary_stats)
  cat("\nStandard Deviation:", sd_value)
})
  
  # Plot for numerical summaries
  output$numeric_plot <- renderPlot({
    req(input$numeric_summary_var)
    hist(filtered_data$data[[input$numeric_summary_var]], main = paste("Histogram of", input$numeric_summary_var),
         xlab = input$numeric_summary_var, col = "skyblue", border = "black")
  })

# Next six plots are on separate subtabs
  # First plot is a graph between two variables for Price vs. Distance by Property Type in Graphical Summaries
  output$price_distance_plot <- renderPlot({
    req(input$x_var, input$y_var)  # Ensure both variables are selected
    
    ggplot(filtered_data$data, aes_string(x = input$x_var, y = input$y_var, color = "Type")) +
      geom_point(alpha = 0.6) +
      theme_minimal() +
      labs(
        title = paste(input$y_var, "vs.", input$x_var, "by Property Type"),
        x = input$x_var,
        y = input$y_var
      ) +
      scale_y_continuous(labels = scales::comma)
  })
  
  # The second plot is a graph that displays the price distribution by region and property type
  output$price_region_boxplot <- renderPlot({
    ggplot(filtered_data$data, aes(x = Regionname, y = Price, fill = Type)) +
      geom_boxplot(outlier.alpha = 0.3) +
      theme_minimal() +
      labs(
        title = "Price Distribution by Region and Property Type",
        x = "Region",
        y = "Price"
      ) +
      scale_y_continuous(labels = scales::comma) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # The third plot is a graph that displays the distribution of property count by suburb and property type
  output$property_count_distribution <- renderPlot({
    ggplot(filtered_data$data, aes(x = Propertycount)) +
      geom_histogram(binwidth = 100, fill = "skyblue", color = "black", alpha = 0.7) +
      facet_wrap(~ Type) +
      theme_minimal() +
      labs(
        title = "Distribution of Property Count by Suburb and Property Type",
        x = "Property Count",
        y = "Frequency"
      )
  })
  # The fourth plot is a graph of the number of rooms by property type across regions. 
  output$rooms_by_region <- renderPlot({
    ggplot(filtered_data$data, aes(x = Rooms, fill = Type)) +
      geom_bar(position = "dodge") +
      facet_wrap(~ Regionname, scales = "free_y") +
      theme_minimal() +
      labs(
        title = "Number of Rooms by Property Type Across Regions",
        x = "Number of Rooms",
        y = "Count"
      ) +
      scale_fill_brewer(palette = "Set2")
  })
  # The fifth plot is a graph of heatmap. 
  output$price_heatmap <- renderPlot({
    # Calculate the average price for each combination of Type and Regionname
    heatmap_data <- filtered_data$data %>%
      group_by(Type, Regionname) %>%
      summarise(avg_price = mean(Price, na.rm = TRUE)) %>%
      ungroup()
    
    # Create the heatmap
    ggplot(heatmap_data, aes(x = Regionname, y = Type, fill = avg_price)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Avg Price") +
      theme_minimal() +
      labs(
        title = "Average Property Price by Type and Region",
        x = "Region",
        y = "Property Type"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # The last (sixth) plot shows average property price over time by type
  output$price_over_time <- renderPlot({
    
    # Ensure Date is in date format
    filtered_data$data$Date <- as.Date(filtered_data$data$Date, format = "%d/%m/%Y")
    
    # Calculate average price by Type and Date
    price_over_time <- filtered_data$data %>%
      group_by(Date, Type) %>%
      summarise(avg_price = mean(Price, na.rm = TRUE)) %>%
      ungroup()
    
    # Plot the average property price over time
    ggplot(price_over_time, aes(x = Date, y = avg_price, color = Type)) +
      geom_line(size = 1) +
      theme_minimal() +
      labs(
        title = "Average Property Price Over Time by Type",
        x = "Date",
        y = "Average Price"
      ) +
      scale_y_continuous(labels = scales::comma)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
















