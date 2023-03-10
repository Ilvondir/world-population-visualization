# Libraries ----
library(tidyverse)
library(plotly)
library(DescTools)

# Data initialization ----
data <- readRDS("datasets/dataset.rds")
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
    plotData <- data %>%
      select(c("Country", "Year", "Group", "Total")) %>%
      filter(Country %in% input$singleCountrySelect & Year==year())
  })
  
  ### Age pyramid data ----
  agePyramidData <- reactive({
    plotData <- data %>%
      select(c("Country", "Year", "Group", "Males", "Females")) %>%
      filter(Country %in% input$singleCountrySelect & Year==year())
  })
  
  ## Plots ----
  ### Total view plot ----
  output$totalViewPlot <- renderPlotly({
    req(input$countrySelect)
    req(input$date)
    
    plt <- plot_ly(data=totalViewData(), 
                   x=~Year, 
                   color=~Country, 
                   text=~Country,
                   colors="Set2")
    
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
                               y=~Total, 
                               type="scatter", 
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
                             hovertemplate="<extra></extra><b>Females</b>\nYear: %{x}\nPopulation: %{y}", 
                             name="Females", 
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
    req(year()>=1950 & year()<=2100)
    
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
                                year(),
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
  
  ###Age pyramid ----
  output$agePyramid <- renderPlotly({
    req(input$singleCountrySelect)
    req(year()>=1950 & year()<=2100)
    req(input$dataComparisonCheckbox==F | input$dataComparisonCheckbox==T)
    
    plot1 <- plot_ly(data=agePyramidData(), y=~Group)
    
    
    plot1 <- plot1 %>% add_bars(x=~Males,
                                name = "Males",
                                marker=list(color="steelblue"),
                                legendgroup="Males",
                                hovertemplate=paste0(
                                  "<extra></extra>",
                                  "<b>Males</b>\n",
                                  "Country: ",
                                  input$singleCountrySelect,
                                  "\nYear: ", year(),
                                  "\nGroup: %{y}",
                                  "\nPopulation: %{x}"
                                ))
    
    if (input$dataComparisonCheckbox==T) {
      
      dateF <- agePyramidData() %>%
        select(c("Group", "Males", "Females"))
      
      dateF$new <- dateF$Females-dateF$Males
      filter <- dateF$new<0
      dateF[filter,"new"]<-0
      
      plot1 <- plot1 %>% add_bars(data=dateF,
                                  x=~new,
                                  y=~Group,
                                  marker=list(
                                    color="pink"),
                                  showlegend=F,
                                  hoverinfo="none",
                                  legendgroup="Males",
                                  opacity=0.5)
    }
    
    plot1 <- plot1 %>% layout(
      xaxis=list(
        autorange = "reversed"),
      yaxis=list(
        showticklabels=F,
        categoryorder = "array",
        categoryarray = agePyramidData()[1:21, "Group"]),
      barmode="stack"
      )
    
    plot2 <- plot_ly(data=agePyramidData(), y=~Group)
    
    plot2 <- plot2 %>% add_bars(x=~Females,
                                name = "Females",
                                marker=list(color="pink"),
                                legendgroup="Females",
                                hovertemplate=paste0(
                                  "<extra></extra>",
                                  "<b>Females</b>\n",
                                  "Country: ",
                                  input$singleCountrySelect,
                                  "\nYear: ", year(),
                                  "\nGroup: %{y}",
                                  "\nPopulation: %{x}"
                                ))
    
    if (input$dataComparisonCheckbox==T) {
      
      dateF <- agePyramidData() %>%
        select(c("Group", "Males", "Females"))
      
      dateF$new <- dateF$Males-dateF$Females
      filter <- dateF$new<0
      dateF[filter,"new"]<-0
      
      plot2 <- plot2 %>% add_bars(data=dateF,
                                  x=~new,
                                  y=~Group,
                                  marker=list(
                                    color="steelblue"),
                                  showlegend=F,
                                  hoverinfo="none",
                                  opacity=0.5,
                                  legendgroup="Females")
    }
    
    plot2 <- plot2 %>% layout(yaxis=list(
      categoryorder = "array",
      categoryarray = agePyramidData()[1:21, "Group"],
      tickfont=list(size=9))
      )
    
    plot <- subplot(plot1, plot2)
    
    highlight(plot)
  })
  
  ## Functions ----
  
  ### Years to first & second plot ----
  years <- eventReactive(input$dateButton, {
    req(input$date)
    as.numeric(Year(input$date))
  }, ignoreNULL = F)
  
  ### Year to single date ----
  year <- eventReactive(input$singleDateButton, {
    req(input$singleDate)
    input$singleDate
  }, ignoreNULL=F)
  
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
    div(
      dateRangeInput(
        inputId = "date",
        label = "Select years:",
        startview = "year",
        start = as.Date(paste0(minDate,"/01/01/")),
        end = as.Date(paste0(maxDate,"/01/01/")),
        format = "yyyy",
        separator = " - "
      ),
      p("The chart supports the period from ", span(1950), " to ", span(2100), ".")
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
    div(
      checkboxInput(
        inputId = "forecastCheckbox",
        label = "Show forecast",
        value = F
      ),
      p("This checkbox enables an additional dataset that predicts the population distribution to", span(2100), ".")
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
    div(
      numericInput(
        inputId = "singleDate",
        label = "Select year:",
        min = 1950,
        max = 2050,
        step = 1,
        value = 2022
      ),
      p("The chart supports the period from ", span(1950), " to ", span(2100), ".")
    )
  })
  
  ### Single date button ----
  output$singleDateButton <- renderUI({
    actionButton(
      inputId = "singleDateButton",
      icon = icon("calendar", lib = "glyphicon"),
      label = "Update plot"
    )
  })
  
  ### Data comparison checkbox ----
  output$dataComparisonCheckbox <- renderUI({
    div(
      checkboxInput(
        inputId = "dataComparisonCheckbox",
        label = "Show data comparison",
        value = F
      ),
      p("This field includes data overlap so that you can clearly see, which gender was dominant in each age group.")
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
        uiOutput("singleDate"),
        uiOutput("singleDateButton")
      )
    } else if (input$panel == "Age pyramid") {
      div(
        uiOutput("singleCountrySelect"),
        uiOutput("singleDate"),
        uiOutput("singleDateButton"),
        uiOutput("dataComparisonCheckbox")
      )
    }
  })
}