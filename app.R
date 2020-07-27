library(shiny)
library(caret)
library(ggplot2)
library(dplyr)

# Load Dataset
diabetes <- read.csv("./diabetes.csv")

barplot <- function(parameter, filters) {
  filtered_diabetes <- diabetes %>% filter(parameter >= filters)
  samp <-sample(768, sample(300:700,1))
  
  filtered_diabetes = diabetes[samp, ]
  diab_count <- filtered_diabetes %>% count(Outcome)
  ggplot(data=diab_count, aes(x=Outcome, y=n)) +
    geom_bar(stat="identity")
}

barplot("Age", 50)
histoplot <- function(parameter) {
  # Basic histogram
  ggplot(diabetes, aes_string(x=parameter)) + geom_histogram()
}

histoplot("Age")

train <- function(parameters) {
  # Create Train and Test Sets
  size <- nrow(diabetes)
  train = 0.8
  test = 1 - train
  train_sample <- sample(size, trunc(train * size))
  
  train_set = diabetes[train_sample, ]
  test_set = diabetes[-train_sample, ]
  
  logistic_model <- glm(Outcome ~., data= subset(train_set, select=parameters), family= binomial)
  summary(logistic_model)
  print("TESTING ACCURACY")
  linear_score <- predict(logistic_model, newdata=test_set, type="response")
  rounded_pred <- ifelse(linear_score > 0.5, 1, 0)
  
  # Validation
  # confusionMatrix(rounded_pred, test_set$Outcome)
  results <-confusionMatrix(
    factor(rounded_pred),
    factor(test_set$Outcome))
  
  results$overall['Accuracy']
}



ui <- fluidPage(
  titlePanel("Pima Indians Diabetes Classification"),
  textOutput(outputId = "explanation"),
  selectInput(inputId = "view_var",
              label = "Select variable to visualize and filter",
              choices = names(diabetes[,-9])),
  textOutput(outputId = "hist_title"),
  plotOutput("hist"),
  numericInput(inputId = "num", label = "Select number to filter selected variable and see diabetes bar chart distribution", value = 0),
  plotOutput("bar"),
  checkboxGroupInput(inputId = "train_vars",
                     label = "Select variables to train Logistic Regression on (Keep Outcome (dependent variable))",
                     choices = names(diabetes), selected = names(diabetes)),
  textOutput(outputId = "accuraccy"),
  textOutput(outputId = "test")
)

server <- function(input, output) {
  # Explanation / Abstract
  output$explanation <- renderText({
    "Obesity related illnesses are on the rise around the world.
    As of 2019 there are 463 million adults with diabetes and the proportion of the population with diabetes is on the rise (Saeedi 2019). 
    This work leverages Logistic Regression Classification machine learning in order to predict if hospital patients
    have diabetes using a variety of data points collected from medical records and blood samples of the patients.
    This work could allow for earlier and more accurate detection of diabetes. Early detection would allow for preventative medication
    and lifestyle changes to be implemented before the disease progresses.
    Additionally this tool allows for researchers to explore and visualize trends with histograms and bar charts
    to see the distribution of diabetes patients across various input parameters.
    Such as Blood Pressure, Age, BMI ect."
  })
  
  # Explanation / Abstract
  output$hist_title <- renderText({
    paste("Histogram of ", input$view_var)
  })
  
  
  output$accuraccy <- renderText({ 
    log_acc <- train(input$train_vars) * 100
    acc_results <- sprintf("Prediction Accuracy: %f%%",log_acc)
    acc_results
  })
  
  output$test <- renderText({
    train_vars <- paste(input$train_vars, collapse = ", ")
    paste("Training Logistic Regression with: ", train_vars)
  })
  
  output$hist <- renderPlot({histoplot(input$view_var)})
  
  output$bar <- renderPlot({barplot(input$view_var, input$num)})
}


shinyApp(ui = ui, server = server)