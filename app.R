# INSERT Libraries HERE
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(plotly)
library(shinythemes)
library(DT)
library(ggplot2)

#PW: ESDA2022!
# INSERT Dataset HERE
windData <- read.csv('df_daily_final.csv')

histogramName <- c("wind_speed", "air_temperature", "relative_humidity", "air_pressure")

##########################
##### User interface #####
##########################

ui <- fluidPage(
  setBackgroundImage(
    src = "https://i.postimg.cc/BvNPSPD2/belgian-cows.jpg"),
  theme = shinytheme("yeti"),
  navbarPage(
    ##########
    ## Page 1
    ##########
    "Forecasting Onshore Wind Generation",
    tabPanel(
      "Home",
      sidebarLayout(
        sidebarPanel(
          h3(
            "Location:", 
            style = "padding-bottom: 20px"),
          h3(
            "Flanders, Belgium", 
            style = "padding-bottom: 20px")
        ),
        mainPanel(
          h2("About our research"),
          h5("Our goal is to understand the main weather variables that impact onshore wind power generation in Flanders, Belgium, and subsequently to select a suitable week-ahead forecasting methodology. Through our research we identified Neural Networks as the most suitable model, proved the seasonality of weather through data exploration and highlighted the degree to which each varialbe effects wind power generation. Readings obtained cover the 2018/11/01 - 2022/11/02 preiod."),
          h5("The value this research brings to future energy systems:"),
          tags$li("Onshore wind energy stakeholders may use our methodology to plan future projects in Flanders."),
          tags$li("Local Transmission System Operator (TSO), Elia, may forecast future wind energy supply."),
          tags$li("Consumers may use the available data to better understand the return on investment for installing domestic wind energy capacity."),
          tags$li("Identified the influence of wind direction as an area for future research."),
          h5("We believe in sharing our findings and data transparency, so that we may build on the collective body of knowledge. Contained in the following tabs is access to our raw data, insights into distributions of most the signficant variables and a historic view of wind energy generation at daily resolution."),
        )
      )
    ),
    ##########
    ## Page 2
    ##########
    tabPanel(
      "Weather Data Index",
      sidebarLayout(
        sidebarPanel(
          h6("Historic Weather Variables vs Output"),
          h6("2018-11-01 to 2022-11-02")
        ),
        mainPanel(
          div(DT::dataTableOutput(outputId = "dataTable"))
        )
      )
    ),
    ##########
    ## Page 3
    ##########
    tabPanel(
      "Weather Variable Distribution",
      sidebarLayout(
        sidebarPanel(
          dateRangeInput(
            'histDateRange',
            label = paste('Date Range'),
            start = as.Date(min(windData$date_col)), end = as.Date(max(windData$date_col)),
            min = as.Date(min(windData$date_col)), max = as.Date(max(windData$date_col)),
          ),
          selectInput(
            'histOptions', 
            'Select Variable', 
            histogramName, 
            selected = "wind_speed",
            multiple = TRUE, 
            selectize = TRUE
          )
        ),
        mainPanel(
          plotlyOutput(outputId = "histogram")
        )
      )
    ),
    ############
    ## Nav menu
    ############
    navbarMenu(
      "More",
      tabPanel(
        "Measured Output",
        sidebarLayout(
          sidebarPanel(
              h6("Measured Wind Power Output"),
              h6("2018-11-01 to 2022-11-02")
          ),
          mainPanel(
            plotlyOutput(outputId = "timeSeries")
          )
        )
      )
    )
  ))



###########################
##### Server function #####
###########################
server <- function(input, output, session) {
  # Filter data based on selections
  output$dataTable <- DT::renderDataTable(DT::datatable({
    windData #%>%
    #filter(filter(date >= input$histDateRange[1] & date <= input$histDateRange[2])
    
    #between(
    #date_col, 
    # as_datetime(as.character(input$histDateRange[1])), 
    # as_datetime(as.character(input$histDateRange[2]))
    #)
    #)
  }))
  
  # Histogram for each variables
  output$histogram <- renderPlotly({ 
    windData %>% 
      filter(
        between(
          date_col, 
          as_datetime(as.character(input$histDateRange[1])), 
          as_datetime(as.character(input$histDateRange[2]))
        )
      )
    
    histogramUnit <- c("m/s", "C", "%", "mbar")
    histogramVar <- c("wind_speed", "air_temperature", "relative_humidity", "air_pressure")
    figs <- list()
    
    for(i in 1:length(input$histOptions)) {
      index <- match(input$histOptions[i], histogramName)
      
      fig <- plot_ly(
        windData, 
        x = as.formula(paste0("~", histogramVar[index]))
      ) %>% 
        add_trace( 
          name = histogramName[index],
          type = "histogram",
          hovertemplate = paste(
            '<b>Range</b>: %{x} ',
            histogramUnit[index],
            '<br><b>Frequency</b>: %{y:.3f}',
            '<extra>', histogramName[index], '</extra>'
          ))
      figs[length(figs)+1] <- list(fig)
    }
    
    subplot(figs, nrows = length(input$histOptions))
  })
  
  # Time series for air temperatures
  output$timeSeries <- renderPlotly({
    windData %>% 
      plot_ly(
        x = windData$date_col, 
        y = windData$Measured_Upscaled, 
        type = 'scatter', 
        mode = 'lines',
        line = list(color = '#251d5a', width = 1.5)
      ) %>%
      layout(
        title = "Measured Output vs Time",
        xaxis = list(title = "Time"),
        yaxis = list(title = "Wind Speed (m/s)")
      )
  })
}

##################################
##### Call shinyApp function #####
##################################
shinyApp(ui = ui, server = server)
