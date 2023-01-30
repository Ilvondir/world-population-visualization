library(tidyverse)

data <- read.csv("datasets/dataset.csv")

data <- data %>%
  select(c("Location", "Time", "AgeGrp", "PopMale", "PopFemale", "PopTotal")) %>%
  set_names(c("Country", "Year", "Group", "Males", "Females", "Total"))

function(input, output) {
  
}