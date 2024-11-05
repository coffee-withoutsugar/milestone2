library(shiny)
library(ggplot2)
library(dplyr)

# Load the dataset
dataset_url <- "https://uwmadison.box.com/shared/static/g6uvupaaf8q4rn6mrfpowenh9o7f0baz.csv"
student_data <- read.csv(dataset_url)

# Convert categorical variables to factors
student_data$Family_Income <- as.factor(student_data$Family_Income)
student_data$Parental_Education_Level <- as.factor(student_data$Parental_Education_Level)
student_data$Access_to_Resources <- as.factor(student_data$Access_to_Resources)
student_data$Teacher_Quality <- as.factor(student_data$Teacher_Quality)
student_data$Motivation_Level <- as.factor(student_data$Motivation_Level)  # Treat Motivation_Level as categorical

# Ensure numeric variables are correctly interpreted as numeric
student_data$Physical_Activity <- as.numeric(student_data$Physical_Activity)
student_data$Sleep_Hours <- as.numeric(student_data$Sleep_Hours)

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Analysis of Factors Influencing Test Performance"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select Variables"),
      
      # Dropdowns for categorical variables
      selectInput("family_income", "Family Income:", choices = levels(student_data$Family_Income)),
      selectInput("parental_education", "Parental Education Level:", choices = levels(student_data$Parental_Education_Level)),
      selectInput("access_to_resources", "Access to Resources:", choices = levels(student_data$Access_to_Resources)),
      selectInput("teacher_quality", "Teacher Quality:", choices = levels(student_data$Teacher_Quality)),
      selectInput("motivation_level", "Motivation Level:", choices = levels(student_data$Motivation_Level)),  # Use dropdown for Motivation_Level
      
      # Sliders for continuous numeric variables
      sliderInput("physical_activity", "Physical Activity:", min = min(student_data$Physical_Activity, na.rm = TRUE), max = max(student_data$Physical_Activity, na.rm = TRUE), value = range(student_data$Physical_Activity, na.rm = TRUE)),
      sliderInput("sleep_hours", "Sleep Hours:", min = min(student_data$Sleep_Hours, na.rm = TRUE), max = max(student_data$Sleep_Hours, na.rm = TRUE), value = range(student_data$Sleep_Hours, na.rm = TRUE)),
      
      # Interactive scatter plot selection
      selectInput("x_var", "X-Axis Variable (Scatter Plot):", choices = c("Hours_Studied", "Attendance")),
      selectInput("y_var", "Y-Axis Variable (Scatter Plot):", choices = c("Exam_Score")),
      checkboxInput("add_trend", "Add Trend Line", value = FALSE)
    ),
    
    mainPanel(
      # Scatter Plot Output
      plotOutput("scatter_plot", brush = brushOpts(id = "plot_brush", resetOnNew = TRUE)),
      
      # Display selected data points
      verbatimTextOutput("selected_points")
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  
  # Reactive filtered data based on sidebar inputs
  filtered_data <- reactive({
    student_data %>%
      filter(
        Family_Income == input$family_income,
        Parental_Education_Level == input$parental_education,
        Access_to_Resources == input$access_to_resources,
        Teacher_Quality == input$teacher_quality,
        Motivation_Level == input$motivation_level,  # Filter for selected Motivation_Level
        Physical_Activity >= input$physical_activity[1],
        Physical_Activity <= input$physical_activity[2],
        Sleep_Hours >= input$sleep_hours[1],
        Sleep_Hours <= input$sleep_hours[2]
      )
  })
  
  # Scatter Plot with interactive trend line
  output$scatter_plot <- renderPlot({
    data <- filtered_data()
    
    p <- ggplot(data, aes_string(x = input$x_var, y = input$y_var)) +
      geom_point() +
      labs(x = input$x_var, y = "Exam Score", title = paste("Exam Score vs", input$x_var))
    
    if (input$add_trend) {
      p <- p + geom_smooth(method = "lm", se = FALSE, color = "blue")
    }
    
    p
  })
  
  # Display brushed points from scatter plot
  output$selected_points <- renderPrint({
    brushed_points <- brushedPoints(filtered_data(), input$plot_brush)
    if (nrow(brushed_points) > 0) {
      brushed_points
    } else {
      "No points selected."
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
