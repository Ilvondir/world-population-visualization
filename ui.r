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
    h1("Select your data")
  ),
  
  ## Content ----
  dashboardBody(
    
    tabsetPanel(
      id = "panel",
      type = "pills",
      
      tabPanel(
        title = "Total view"
      )
    )
  )
)