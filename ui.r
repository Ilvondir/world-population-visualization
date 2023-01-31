# Libraries ----
library(shiny)
library(shinydashboard)

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
    h2("Select your data")
  ),
  
  ## Content ----
  dashboardBody(
    ### CSS head tag ----
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    ### Tabset panel ----
    tabsetPanel(
      id = "panel",
      type = "pills",
      
      tabPanel(
        title = "Total view"
      )
    )
  )
)