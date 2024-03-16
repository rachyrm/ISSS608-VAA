pacman::p_load(shiny, tidyverse, ggplot2, plotly, forecast, lubridate)

weatherdata <- read_rds("weatherdata.rds")

ui <- navbarPage("Weather Data Shiny App",
                 tabPanel("Exploratory Data Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "category1",
                                          label = "Category:",
                                          choices = c("Temperature" = "MeanTemp",
                                                      "Rainfall" = "Rainfall",
                                                      "Wind" = "MeanWind"),
                                          selected = "Temperature"),  
                              selectInput(inputId = "location1",
                                          label = "Location:",
                                          choices = c("North Region" = "Admiralty",
                                                      "North-East Region" = "Pulau Ubin",
                                                      "West Region" = "Jurong (West)",
                                                      "Central Region" = "Newton",
                                                      "East Region" = "Changi"),
                                          multiple = TRUE,
                                          selected = "Temperature"),
                              sliderInput(inputId = "year_range1",
                                          label = "Select a range of years:",
                                          min = 2013,
                                          max = 2023,
                                          value = c(2013, 2023),
                                          step = 1)
                            ),
                            
                            mainPanel(
                              plotOutput("EDA_Plot1"),
                              plotlyOutput("EDA_Plot2"),
                              plotlyOutput("EDA_Plot3"),
                              plotlyOutput("EDA_Plot4"),
                              plotlyOutput("EDA_Plot5")
                            )
                          )),
                 tabPanel("Confirmatory Data Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "category1",
                                          label = "Category:",
                                          choices = c("Temperature" = "MeanTemp",
                                                      "Rainfall" = "Rainfall",
                                                      "Wind" = "MeanWind"),
                                          selected = "Temperature"),  
                              selectInput(inputId = "location1",
                                          label = "Location:",
                                          choices = c("North Region" = "Admiralty",
                                                      "North-East Region" = "Pulau Ubin",
                                                      "West Region" = "Jurong (West)",
                                                      "Central Region" = "Newton",
                                                      "East Region" = "Changi"),
                                          multiple = TRUE,
                                          selected = "Temperature"),
                              sliderInput(inputId = "year_range1",
                                          label = "Select a range of years:",
                                          min = 2013,
                                          max = 2023,
                                          value = c(2013, 2023),
                                          step = 1)
                            ),
                            
                            mainPanel(
                              plotOutput("CDA_Plot1"),
                              plotOutput("CDA_Plot2")
                            )
                          )),
                 tabPanel("Predictive Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "category20",
                                          label = "Category:",
                                          choices = c("Temperature" = "MeanTemp",
                                                      "Rainfall" = "Rainfall",
                                                      "Wind" = "MeanWind"),
                                          selected = "Temperature"),  
                              selectInput(inputId = "location20",
                                          label = "Location:",
                                          choices = c("North Region" = "Admiralty",
                                                      "North-East Region" = "Pulau Ubin",
                                                      "West Region" = "Jurong (West)",
                                                      "Central Region" = "Newton",
                                                      "East Region" = "Changi"),
                                          multiple = TRUE,
                                          selected = "Temperature")
                            ),
                            
                            mainPanel(
                              plotlyOutput("Predictive")
                            )
                          ))
)

server <- function(input, output) {
  #EDA Plot 1
  output$EDA_Plot1 <- renderPlot({
    filtered_data <- weatherdata %>%
      filter(Year >= input$year_range1[1], 
             Year <= input$year_range1[2],
             Station %in% input$location1)
    ggplot(data = filtered_data, aes_string(x = input$category1, y = "Year")) +
      geom_point(color = "darkblue") +
      labs(x = input$category1, y = "Year", 
           title = paste("Scatter Plot of", input$category1, "over the years"))
  })
  
  #EDA Plot 2
  output$EDA_Plot2 <- renderPlotly({
    filtered_data2 <- weatherdata %>%
      filter(Year >= input$year_range1[1], 
             Year <= input$year_range1[2],
             Station %in% input$location1) %>%
      group_by(Year, Station) %>%
      summarise(MeanTemp = mean(MeanTemp, na.rm = TRUE),
                Rainfall = mean(Rainfall, na.rm = TRUE),
                MeanWind = mean(MeanWind, na.rm = TRUE), .groups = 'drop')
    
    input_category1 <- filtered_data2[[input$category1]]
    
    plot_ly(data = filtered_data2, x = ~Year, y = input_category1,
            color = ~Station) %>%
      layout(title = paste("Annual Average of", input$category1, "by Station"),
             xaxis = list(title = "Year"),
             yaxis = list(title = input$category1))
  })
  
  #EDA Plot3
  output$EDA_Plot3 <- renderPlotly({
    filtered_data3 <- weatherdata %>%
      filter(Year >= input$year_range1[1], 
             Year <= input$year_range1[2],
             Station %in% input$location1) %>%
      group_by(Year, Month, Station) %>%
      summarise(MeanTemp = mean(MeanTemp, na.rm = TRUE),
                Rainfall = mean(Rainfall, na.rm = TRUE),
                MeanWind = mean(MeanWind, na.rm = TRUE), .groups = 'drop')
    
    filtered_data3$Month <- factor(filtered_data3$Month,
                                   levels = 1:12,
                                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"),
                                   ordered = TRUE)  
    
    
    y_aes <- sym(input$category1)
    
    p <- ggplot(filtered_data3, aes(x = Month, y = !!y_aes, color = as.factor(Station))) +
      geom_point(aes(size = !!y_aes), alpha = 0.7) +
      scale_size(range = c(2, 12)) +
      labs(x = 'Month', y = input$category1) + 
      theme(legend.position = 'bottom') +
      guides(color = guide_legend(title = "Station", override.aes = list(size = 3), ncol = 1))
    
    ggplotly(p)
  }) 
  
  #EDA Plot4
  output$EDA_Plot4 <- renderPlotly({
    filtered_data4 <- weatherdata %>%
      filter(Year >= input$year_range1[1], 
             Year <= input$year_range1[2],
             Station %in% input$location1) %>%
      group_by(Year, Month, Station) %>%
      summarise(MeanTemp = mean(MeanTemp, na.rm = TRUE),
                Rainfall = mean(Rainfall, na.rm = TRUE),
                MeanWind = mean(MeanWind, na.rm = TRUE), .groups = 'drop')
    
    filtered_data4$Month <- factor(filtered_data4$Month,
                                   levels = 1:12,
                                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"),
                                   ordered = TRUE)
    
    
    category_value <- sym(input$category1)
    
    gg <- ggplot(filtered_data4, aes(x = Year, y = Month)) + 
      geom_tile(aes(fill = !!category_value), color = "white", size = 0.1) + 
      scale_fill_gradient(low = "sky blue", high = "dark blue", name = input$category1) +
      labs(title = paste("Heat Map of", input$category1, "by Year & Month"), x = NULL, y = NULL) +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7),
            plot.title = element_text(hjust = 0.5),
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 6))
    
    gg  
  })
  
  #EDA Plot5
  output$EDA_Plot5 <- renderPlotly({
    filtered_data5 <- weatherdata %>%
      filter(Year >= input$year_range1[1], 
             Year <= input$year_range1[2],
             Station %in% input$location1) %>%
      group_by(Year, Month, Station) %>%
      summarise(MeanTemp = mean(MeanTemp, na.rm = TRUE),
                Rainfall = mean(Rainfall, na.rm = TRUE),
                MeanWind = mean(MeanWind, na.rm = TRUE), .groups = 'drop')
    
    filtered_data5$Month <- factor(filtered_data5$Month,
                                   levels = 1:12,
                                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"),
                                   ordered = TRUE)
    
    category_value2 <- sym(input$category1)

    ggplot(filtered_data5, aes(x = Month, y = !!category_value2, group = Year, color = factor(Year))) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(title = paste("Monthly Trends by Year:", input$category1), x = "Month", y = input$category1)
  }) 
  
  #CDA Plot1
  output$CDA_Plot1 <- renderPlot({
    filtered_data6 <- weatherdata %>%
      filter(Year >= input$year_range1[1], 
             Year <= input$year_range1[2],
             Station %in% input$location1) %>%
      group_by(Year, Station) %>%
      summarise(MeanTemp = mean(MeanTemp, na.rm = TRUE),
                Rainfall = mean(Rainfall, na.rm = TRUE),
                MeanWind = mean(MeanWind, na.rm = TRUE), .groups = 'drop')
    
    category_value3 <- sym(input$category1)

    ggplot(filtered_data6, aes(x = as.numeric(Year), y = !!category_value3)) +
      geom_point() +
      geom_smooth(method = "lm", color = "blue") +
      labs(title = paste("Long-term Trend of", input$category1), x = "Year", y = input$category1)
  })
  
  #CDA Plot2
  output$CDA_Plot2 <- renderPlot({
    filtered_data7 <- weatherdata %>%
      filter(Year >= input$year_range1[1], 
             Year <= input$year_range1[2],
             Station %in% input$location1) %>%
      group_by(Year, Month,Day,Station) %>%
      summarise(MeanTemp = mean(MeanTemp, na.rm = TRUE),
                Rainfall = mean(Rainfall, na.rm = TRUE),
                MeanWind = mean(MeanWind, na.rm = TRUE), .groups = 'drop')
    
    category_value4 <- sym(input$category1) 
    
    ggplot(filtered_data7, aes(x = Year, y = !!category_value4)) + 
      geom_boxplot(fill = "lightblue", notch = TRUE) +
      labs(title = paste("Annual", input$category1, "by Year"), x = "Year", y = paste("Annual", input$category1)) +
      theme_minimal()
  })
  
  #Predictive Plot1
  output$Predictive <- renderPlotly({
    filtered_data2 <- weatherdata %>%
      filter(Station %in% input$location20)
    
    filtered_data2$Date <- as.Date(paste(filtered_data2$Year, filtered_data2$Month, filtered_data2$Day, sep = "-"))
    
    AvgPerMonth <- filtered_data2 %>%
      group_by(Year, Month) %>%
      summarise(Average = mean(as.numeric(.data[[input$category20]]), na.rm = TRUE))
    
    AvgPerMonth$Month <- factor(AvgPerMonth$Month, levels = 1:12,
                                labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
    
    ts_data <- ts(AvgPerMonth$Average, start = c(2013, 1), frequency = 12)
    
    arima_model <- auto.arima(ts_data)
    forecast_result <- forecast(arima_model, h = 36)
    
    forecast_df <- as.data.frame(forecast_result)
    forecast_df$Date <- seq(as.Date(tail(filtered_data2$Date, 1)), by = "months", length.out = nrow(forecast_df))
    
    forecast_df$Year <- lubridate::year(forecast_df$Date)
    forecast_df$Month <- lubridate::month(forecast_df$Date, label = TRUE)
    
    p <- ggplot(forecast_df, aes(x = Date)) +
      geom_line(aes(y = `Point Forecast`), color = "blue") +
      geom_ribbon(aes(ymin = `Lo 95`, ymax = `Hi 95`), fill = "lightblue", alpha = 0.5) +
      labs(x = "Date", y = "Forecasted Value", title = "Forecasted Values with 95% Confidence Interval") +
      geom_point(aes(y = `Point Forecast`, 
                     text = paste("Year: ", Year, "<br>",
                                  "Month: ", Month, "<br>",
                                  "Forecasted Value: ", `Point Forecast`, "<br>",
                                  "Upper Bound: ", `Hi 95`, "<br>",
                                  "Lower Bound: ", `Lo 95`)), 
                 color = "blue", size = 0)  # Add tooltip text
    
    ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)