# Libraries ----
library(shiny)
library(shinydashboard)

# Frame ----
dashboardPage(
  skin = "purple",
  
  ## Header ----
  dashboardHeader(
    title = "World Population",
    titleWidth = 200
  ),
  
  ## Sidebar ----
  dashboardSidebar(
    width = 200,
    h1("Select your data")
  ),
  
  ## Content ----
  dashboardBody(
    
    tabsetPanel(
      id = "panel",
      
      tabPanel(
        title = "Country view"
      )
    )
  )
)