library(ggplot2)
library(heatmaply)
library(dplyr)
dataset_url <- "https://uwmadison.box.com/shared/static/g6uvupaaf8q4rn6mrfpowenh9o7f0baz.csv"
student_data <- read.csv(dataset_url)

data_selected <- data %>%
  select(Motivation_Level, Attendance, Sleep_Hours) %>%
  mutate(Motivation_Level = case_when(
    Motivation_Level == "Low" ~ 1,
    Motivation_Level == "Medium" ~ 2,
    Motivation_Level == "High" ~ 3
  ))

data_scaled <- scale(data_selected)
heatmaply(
  data_scaled,
  k_col = 3,  
  k_row = 3,  
  main = "Cluster heat map of students’ learning habits",
  xlab = "Study habits factor",
  ylab = "Student Group",
  colors = viridis::viridis(100), 
  row_dend_left = TRUE 
)
