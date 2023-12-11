#library(shiny)
library(ggplot2)
library(tidyverse)

# Assuming df4 is your dataset
# Assuming lim is your linear regression model

# Define UI
ui <- fluidPage(
  titlePanel("Energy Consumption trends"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("county", "Select county", value = min(df4$df3.county_mode)),
      textInput("num_occupants", "Select num of occupants:", value = max(df4$df3.in.occupants_mode)),
      textInput("time","Input time like 05:30AM", value = ""),
      actionButton("predictButton", "Predict Energy Usage")
    ),
    
    mainPanel(
      plotOutput("buildingEnergyPlot"),
      plotOutput("buildingEnergyPlotFuture"),
      plotOutput("timeEnergyPlot"),
      plotOutput("timeEnergyPlotFuture")
      
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  observeEvent(input$predictButton, {
  
  classify_time_period <- function(time_string) {
    time_parts <- strptime(time_string, "%I:%M%p")
    hour <- as.numeric(format(time_parts, "%H"))
    
    time_period <- case_when(
      between(hour, 6, 11) ~ "Morning",
      between(hour, 12, 15) ~ "Afternoon",
      between(hour, 16, 19) ~ "Evening",
      TRUE ~ "Night"
    )
    
    return(time_period)
  }
  
  filtered_data <- reactive({
    df4 %>%
      filter(df3.county_mode == input$county, df3.in.occupants_mode <= input$num_occupants,
             time_period == classify_time_period(input$time))
  })
  
  filtered_data_df_future <-  reactive({
    df4_future %>% 
      filter(df3.county_mode == input$county, df3.in.occupants_mode <= input$num_occupants,
             time_period == classify_time_period(input$time))
  })
  
  output$buildingEnergyPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = as.character(df3.bldg_id))) +
      geom_line(aes(y = total_sum), color = "blue", size = 1) +
      #geom_line(aes(y = Parameter2), color = "red", size = 1) +
      labs(title = "Building IDs vs. Total Energy Usage",
           x = "Building IDs",
           y = "Energy Consumption") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$buildingEnergyPlotFuture <- renderPlot({
    ggplot(filtered_data_df_future(), aes(x = as.character(df3.bldg_id) , y = predicted_new_energy)) +
      geom_point(color = "red") +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Building IDs vs. Total Energy Usage in Future",
           x = "Building IDs",
           y = "Energy Consumption in Future")+
      theme(axis.text.x = element_text(angle = 65, hjust = 1))
  })
  
  output$timeEnergyPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = time_period , y = total_sum)) +
      geom_point(color = "blue") +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Time Period vs. Total Energy Usage",
           x = "Time Period",
           y = "Total Energy Usage")
      #theme(axis.text.x = element_text(angle = 65, hjust = 1))
  })
  output$timeEnergyPlotFuture <- renderPlot({
    ggplot(filtered_data_df_future(), aes(x = time_period , y = predicted_new_energy)) +
      geom_point(color = "red") +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Time Period vs. Total Energy Usage in Future",
           x = "Time Period",
           y = "Total Energy Usage in Future")
      #theme(axis.text.x = element_text(angle = 65, hjust = 1))
  })
 
  
  
  })
  
  }

# Run the Shiny app
shinyApp(ui, server)