# Libraries ----
library(tidyverse)
library(plotly)
library(DescTools)

# Data initialization ----
data <- read.csv("datasets/dataset.csv")
data <- data %>%
  select(c("Location", "Time", "AgeGrp", "PopMale", "PopFemale", "PopTotal")) %>%
  set_names(c("Country", "Year", "Group", "Males", "Females", "Total"))

# Plots layout ----
font <- list(
  family = "Tahoma",
  size = 16,
  color = "black"
)

x <- list(
  title = "Year",
  titlefont = font
)

y <- list(
  title = "Population",
  titlefont = font
)

# Server function ----
function(input, output) {
  
  ## Data to first plot ----
  firstPlotData <- reactive({
    plotData <- data %>%
      select(c("Country", "Year", "Total")) %>%
      filter(Country %in% input$countrySelect & Year>=as.numeric(Year(input$date[1])) & Year<=as.numeric(Year(input$date[2]))) %>%
      group_by(Country, Year) %>%
      summarise_all(sum) %>%
      arrange(Year)
  })
  
  ## Total view plot ----
  output$totalViewPlot <- renderPlotly({
    req(input$countrySelect)
    req(input$date)
    
    plt <- plot_ly(data=firstPlotData(), x=~Year, color=~Country, text=~Country)
    
    plt <- plt %>% add_trace(data=firstPlotData() %>% filter(Year<=2022), y=~Total, type="scatter", mode="lines+markers", hovertemplate="<extra></extra><b>%{text}</b>\nYear: %{x}\nPopulation: %{y}")
    
    plt <- plt %>% add_trace(data=firstPlotData() %>% filter(Year>2022), y=~Total, type="scatter", mode="lines+markers", hovertemplate="<extra></extra><b>%{text}</b>\nYear: %{x}\nPopulation: %{y}", showlegend=F, opacity=0.6)
    
    plt <- plt %>% layout(plt, title=list(text="Population by regions", font = font), xaxis=x, yaxis=y)
    
    highlight(plt)
  })
  ## UI sidebar elements ----
  
  ### Country select ----
  output$countrySelect <- renderUI({
    regions <- data %>% select("Country") %>% unique()
    selectInput(
      inputId = "countrySelect",
      label = "Select regions:",
      choices = regions,
      multiple = T,
      selected = "Poland"
    )
  })
  
  ### Date range ----
  output$date <- renderUI({
    minDate <- data %>% select("Year") %>% min()
    maxDate <- data %>% select("Year") %>% max()
    dateRangeInput(
      inputId = "date",
      label = "Select years:",
      startview = "year",
      start = as.Date(paste0(minDate,"/01/01/")),
      end = as.Date(paste0(maxDate,"/01/01/")),
      format = "yyyy"
    )
  })
  
  ## Sidebar render ----
  output$sidebar <- renderUI({
    if (input$panel == "Total view") {
      div(
        uiOutput("countrySelect"),
        uiOutput("date")
      )
    }
  })
}