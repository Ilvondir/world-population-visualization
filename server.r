# Libraries ----
library(tidyverse)
library(plotly)
library(DescTools)

# Data initialization ----
data <- read.csv("datasets/dataset.csv")
data <- data %>%
  select(c("Location", "Time", "AgeGrp", "PopMale", "PopFemale", "PopTotal")) %>%
  set_names(c("Country", "Year", "Group", "Males", "Females", "Total"))
data$Males <- as.numeric(data$Males * 1000)
data$Females <- as.numeric(data$Females * 1000)
data$Total <- as.numeric(data$Total * 1000)

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
  ## Data ----
  ### Total view data ----
  totalViewData <- reactive({
    plotData <- data %>%
      select(c("Country", "Year", "Total")) %>%
      filter(Country %in% input$countrySelect & Year>=years()[1] & Year<=years()[2]) %>%
      group_by(Country, Year) %>%
      summarise_all(sum) %>%
      arrange(Year)
  })
  
  ### Gender view data ----
  genderViewData <- reactive({
    secondPlotData <- data %>%
      select(c("Country", "Year", "Males", "Females")) %>%
      filter(Country %in% input$singleCountrySelect & Year>=years()[1] & Year<=years()[2]) %>%
      group_by(Country, Year) %>%
      summarise_all(sum) %>%
      arrange(Year)
  })
  
  ### Age group view data ----
  ageGroupViewData <- reactive({
    secondPlotData <- data %>%
      select(c("Country", "Year", "Group", "Total")) %>%
      filter(Country %in% input$singleCountrySelect & Year==input$singleDate)
  })
  
  ## Plots ----
  ### Total view plot ----
  output$totalViewPlot <- renderPlotly({
    req(input$countrySelect)
    req(input$date)
    
    plt <- plot_ly(data=totalViewData(), 
                   x=~Year, 
                   color=~Country, 
                   text=~Country)
    
    plt <- plt %>% add_trace(data=totalViewData() %>% 
                               filter(Year<=2022), 
                             y=~Total, 
                             type="scatter",
                             mode="lines+markers",
                             hovertemplate="<extra></extra><b>%{text}</b>\nYear: %{x}\nPopulation: %{y}", 
                             legendgroup=~Country)
    
    if (input$forecastCheckbox == T) {
      plt <- plt %>% add_trace(data=totalViewData() %>%
                                 filter(Year>=2022), 
                               y=~Total, type="scatter", 
                               mode="lines+markers", 
                               hovertemplate="<extra>Forecast</extra><b>%{text}</b>\nYear: %{x}\nPopulation: %{y}", 
                               showlegend=F, 
                               opacity=0.6, 
                               legendgroup=~Country)
    }
    
    plt <- plt %>% layout(plt, title="", xaxis=x, yaxis=y)
    
    highlight(plt)
  })
  
  ### Gender view plot ----
  output$genderViewPlot <- renderPlotly({
    req(input$singleCountrySelect)
    req(input$date)
    
    plt <- plot_ly(data=genderViewData(), x=~Year)
    
    plt <- plt %>% add_trace(data=genderViewData() %>% 
                               filter(Year<=2022), 
                             y=~Males, 
                             type="scatter",
                             mode="lines+markers",
                             hovertemplate="<extra></extra><b>Males</b>\nYear: %{x}\nPopulation: %{y}",
                             name="Males", 
                             line=list(color="steelblue"), 
                             marker=list(color="steelblue"), 
                             legendgroup="males")
    
    if (input$forecastCheckbox == T) {
      plt <- plt %>% add_trace(data=genderViewData() %>% 
                                 filter(Year>=2022), 
                               y=~Males, type="scatter",
                               mode="lines+markers", 
                               hovertemplate="<extra>Forecast</extra><b>Males</b>\nYear: %{x}\nPopulation: %{y}", 
                               showlegend=F,
                               opacity=0.6, 
                               line=list(color="steelblue"), 
                               marker=list(color="steelblue"), 
                               legendgroup="males")
    }
    
    plt <- plt %>% add_trace(data=genderViewData() %>%
                               filter(Year<=2022), 
                             y=~Females, 
                             type="scatter",
                             mode="lines+markers", 
                             hovertemplate="<extra></extra><b>Females</b>\nYear: %{x}\nPopulation: %{y}", name="Females", 
                             line=list(color="pink"), 
                             marker=list(color="pink"), 
                             legendgroup="females")
    
    if (input$forecastCheckbox == T) {
      plt <- plt %>% add_trace(data=genderViewData() %>% filter(Year>=2022), 
                               y=~Females, 
                               type="scatter", 
                               mode="lines+markers", 
                               hovertemplate="<extra>Forecast</extra><b>Females</b>\nYear: %{x}\nPopulation: %{y}",
                               showlegend=F, 
                               opacity=0.6, 
                               line=list(color="pink"), 
                               marker=list(color="pink"), 
                               name="Females", 
                               legendgroup="females")
    }
    
    plt <- plt %>% layout(plt, title="", xaxis=x, yaxis=y)
    
    highlight(plt)
  })
  
  ### Age group plot ----
  output$ageGroupPlot <- renderPlotly({
    req(input$singleCountrySelect)
    req(input$singleDate)
    
    xform <- list(
      categoryorder = "array",
      categoryarray = ageGroupViewData()[1:21, "Group"],
      title = "Age group",
      titlefont = font
    )
    
    plot <- plot_ly(data=ageGroupViewData(), 
                    x=~Group)
    
    plot <- plot %>% add_bars(y=~Total,
                              hovertemplate=paste0(
                                input$singleCountrySelect,
                                "- ",
                                input$singleDate,
                                "\nGroup: %{x}",
                                "\nPopulation: %{y}",
                                "<extra></extra>"
                              ))
    
    plot <- plot %>% layout(plot, 
                            title="",
                            xaxis=xform, 
                            yaxis=y)
      
    highlight(plot)
  })
  
  ## Functions ----
  
  ### Years to first & second plot ----
  years <- eventReactive(input$dateButton, {
    req(input$date)
    as.numeric(Year(input$date))
  }, ignoreNULL = F)
  
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
      format = "yyyy",
      separator = " - "
    )
  })
  
  ### Date button ----
  output$dateButton <- renderUI({
    actionButton(
      inputId = "dateButton",
      icon = icon("calendar", lib = "glyphicon"),
      label = "Update plot"
    )
  })
  
  ### Forecast checkbox ----
  output$forecastCheckbox <- renderUI({
    checkboxInput(
      inputId = "forecastCheckbox",
      label = "Show forecast (max to 2100)",
      value = F
    )
  })
  
  ### Single region select ----
  output$singleCountrySelect <- renderUI({
    regions <- data %>% select("Country") %>% unique()
    selectInput(
      inputId = "singleCountrySelect",
      label = "Select region:",
      choices = regions,
      multiple = F,
      selected = "Poland"
    )
  })
  
  ### Single date select ----
  output$singleDate <- renderUI({
    numericInput(
      inputId = "singleDate",
      label = "Select year:",
      min = 1950,
      max = 2050,
      step = 1,
      value = 2022
    )
  })
  
  ## Sidebar render ----
  output$sidebar <- renderUI({
    if (input$panel == "Total view") {
      div(
        uiOutput("countrySelect"),
        uiOutput("date"),
        uiOutput("dateButton"),
        uiOutput("forecastCheckbox")
      )
    } else if (input$panel == "Gender view") {
      div(
        uiOutput("singleCountrySelect"),
        uiOutput("date"),
        uiOutput("dateButton"),
        uiOutput("forecastCheckbox")
      )
    } else if (input$panel == "Age group view") {
      div(
        uiOutput("singleCountrySelect"),
        uiOutput("singleDate")
      )
    }
  })
}