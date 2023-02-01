# Libraries ----
library(shiny)
library(shinydashboard)
library(plotly)

# Frame ----
dashboardPage(
  
  skin = "purple",
  
  ## Header ----
  dashboardHeader(
    title = "World Population",
    titleWidth = 300
  ),
  
  ## Sidebar ----
  dashboardSidebar(
    width = 300,
    h2("Select your data"),
    
    uiOutput("sidebar")
  ),
  
  ## Content ----
  dashboardBody(
    ### CSS head tag ----
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css"),
      tags$link(rel="shortcut icon", href = "img/icon.png")
    ),
    
    ### Tabset panel ----
    tabsetPanel(
      id = "panel",
      type = "pills",
      
      tabPanel(
        title = "Total view",
        plotlyOutput("totalViewPlot")
      ),
      
      tabPanel(
        title = "Gender view",
        plotlyOutput("genderViewPlot")
      )
    )
  )
)