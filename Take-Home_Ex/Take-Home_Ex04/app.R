pacman::p_load(shiny, tidyverse, plotly)

weatherdata <- read_rds("weatherdata.rds")

ui <- navbarPage("My Shiny App",
                 tabPanel("Exploratory",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "category1",
                                          label = "Category:",
                                          choices = c("Rainfall" = "Rainfall",
                                                      "Temperature" = "MeanTemp",
                                                      "Wind" = "MeanWind"),
                                          selected = "Temperature"),  
                              selectInput(inputId = "location1",
                                          label = "Location:",
                                          choices = c("Admiralty" = "Admiralty",
                                                      "Pulau Ubin" = "Pulau Ubin",
                                                      "Jurong (West)" = "Jurong (West)",
                                                      "Newton" = "Newton",
                                                      "Changi" = "Changi"),
                                          multiple = TRUE,
                                          selected = "Temperature"),
                              sliderInput(inputId = "year_range1",
                                          label = "Select a range of years:",
                                          min = 2013,
                                          max = 2023,
                                          value = c(2013, 2023),
                                          step = 1)
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                              plotOutput("Hist1"),
                              plotOutput("Hist2")
                            )
                          )),
                 tabPanel("Confirmatory",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "category2",
                                          label = "Category:",
                                          choices = c("Rainfall" = "Rainfall",
                                                      "Temperature" = "MeanTemp",
                                                      "Wind" = "MeanWind"),
                                          selected = "Temperature"),  
                              selectInput(inputId = "location2",
                                          label = "Location:",
                                          choices = c("Admiralty" = "Admiralty",
                                                      "Pulau Ubin" = "Pulau Ubin",
                                                      "Jurong (West)" = "Jurong (West)",
                                                      "Newton" = "Newton",
                                                      "Changi" = "Changi"),
                                          multiple = TRUE,
                                          selected = "Temperature"),
                              sliderInput(inputId = "year_range2",
                                          label = "Select a range of years:",
                                          min = 2013,
                                          max = 2023,
                                          value = c(2013, 2023),
                                          step = 1)
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                              plotOutput("Hist4")
                            )
                          )),
                 tabPanel("Prediction",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "category3",
                                          label = "Category:",
                                          choices = c("Rainfall" = "Rainfall",
                                                      "Temperature" = "MeanTemp",
                                                      "Wind" = "MeanWind"),
                                          selected = "Temperature"),  
                              selectInput(inputId = "location3",
                                          label = "Location:",
                                          choices = c("Admiralty" = "Admiralty",
                                                      "Pulau Ubin" = "Pulau Ubin",
                                                      "Jurong (West)" = "Jurong (West)",
                                                      "Newton" = "Newton",
                                                      "Changi" = "Changi"),
                                          multiple = TRUE,
                                          selected = "Temperature"),
                              sliderInput(inputId = "year_range3",
                                          label = "Select a range of years:",
                                          min = 2013,
                                          max = 2023,
                                          value = c(2013, 2023),
                                          step = 1)
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                              plotOutput("Hist3")
                            )
                          ))
)

server <- function(input, output) {
  output$Hist1 <- renderPlot({
    # Filter data based on the selected range of years and locations
    filtered_data <- weatherdata %>%
      filter(Year >= input$year_range1[1], 
             Year <= input$year_range1[2],
             Station %in% input$location1)
    
    # Plot the filtered data as a scatter plot
    ggplot(data = filtered_data, aes_string(x = input$category1, y = "Year")) +
      geom_point(color = "darkblue") +
      labs(x = input$category1, y = "Year", 
           title = paste("Scatter Plot of", input$category1, "over the years"))
  })
  
  output$Hist2 <- renderPlot({
    # Filter data based on the selected range of years and locations
    filtered_data <- weatherdata %>%
      filter(Year >= input$year_range1[1], 
             Year <= input$year_range1[2],
             Station %in% input$location1)
    
    # Plot the filtered data as a histogram
    ggplot(data = filtered_data, aes_string(x = input$category1)) +
      geom_histogram(color = "darkgreen", fill = "lightgreen", bins = 30) +
      labs(x = input$category1, y = "Frequency", 
           title = paste("Histogram of", input$category1))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)