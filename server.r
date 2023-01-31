# Libraries ----
library(tidyverse)

# Data initialization ----
data <- read.csv("datasets/dataset.csv")
data <- data %>%
  select(c("Location", "Time", "AgeGrp", "PopMale", "PopFemale", "PopTotal")) %>%
  set_names(c("Country", "Year", "Group", "Males", "Females", "Total"))

# Server function ----
function(input, output) {
  
  ## Data to first plot ----
  firstPlotData <- reactive({
    plotData <- data %>%
      select(c(Country, Year, Total)) %>%
      filter(Country %in% input$countrySelect & Year>=input$date[1] & Year<=input$date[2]) %>%
      group_by(Country) %>%
      summarise_all(sum) %>%
      arrange(Date)
  })
  
  ## Total view plot ----
  
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