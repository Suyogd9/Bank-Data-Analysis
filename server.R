#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)
library(dplyr)
library(ggpubr)
library(caret)
library(corrplot)
library(dplyr)

#Static function to calculate accuracy
calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}



shinyServer(function(input, output, session) {
  
  
#--------------------------------------------------------------------------------------------------------------------------#
#---------------------Functions-------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------# 
  
  #Reading the dataset
  bankDataUpdt <- reactive({
    
    bankData <- read_csv("data/bankData.csv")
    bankData <- bankData %>%
      mutate(education = gsub("\\.", " ",education)) %>%
      mutate(pdays = if_else(pdays == 999,0, pdays))
    bankData <- mutate_if(bankData, is.character, as.factor)
    return(bankData)
    
  })
  
  
  #Data frame with updated col names
  updtBankData <- reactive({
    
    updtBankData <- bankDataUpdt()
    colnames(updtBankData) <- c("Age","Job Type", "Marital status", "Education", "Has Credit", "Housing", "Loan", "Contact Type",
                                "Month", "Day_of_the_week","Last_contact_duration","Campaign", "pDays(Last contacted)","Previous(Previously contacted or not)",
                                "pOutcome(previous marketing campaign)", "Term deposit")
    updtBankData <- mutate_if(updtBankData, is.character, as.factor)
    
    return(updtBankData)
    
  })
  

  
#--------------------------------------------------------------------------------------------------------------------------#
#---------------------Data Page-------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------# 
  
  #Subsetting the data based on the values of the input
  theBankData <- reactive(updtBankData() %>%
                            select(all_of(input$selectVariables)) %>%
                            slice(1:input$numberOfRows))
  
  output$table <- renderDataTable({
    
    theBankData()
    
  },options = list(scrollX = TRUE, scrollY = TRUE, fixedHeader = TRUE))
  
  #Functionality to download the subsetted data
  output$download <- downloadHandler(
    filename = function(){"BankData.csv"}, 
    content = function(fname){
      write.csv(theBankData(), fname)
    }
  )
  

#-----------------------------------------------------------------------------------------------------------------------#
#---------------------Plots--------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------# 
  
  #Handling bar plots
  output$barPlot <- renderPlot({
    
    ggplot(bankDataUpdt() , aes_string(x = input$barPlotRadio)) +
      geom_bar(aes(fill = y)) + 
      scale_fill_discrete(name = "Type of Term deposit") + 
      xlab(paste("Type of ",input$barPlotRadio) ) +
      theme(axis.text.x = element_text(angle = input$twistAngle)) +
      ggtitle(paste("Bar plot for", input$barPlotRadio))
    
  })
  
  #Handling Histogram plot
  output$histPlot <- renderPlot({
    
    ggplot(data = bankDataUpdt(), aes_string(x = input$histPlotRadio)) + 
      geom_histogram(aes(y = after_stat(density)),binwidth = input$binWidth, colour="black", fill="white") +
      geom_density(alpha=.2, fill="#FF6666") +
      scale_fill_discrete(name = "Type of Term deposit") +
      xlab(input$histPlotRadio) + ggtitle(paste("Histogram for", input$histPlotRadio))
    
  })
  
  #Handling Box plot
  output$boxPlot <- renderPlot({
    y = 'y'
    ggplot(bankDataUpdt(), aes_string(x = input$boxPlotRadio, y = y)) + 
      geom_boxplot()  +
      xlab(paste("Type of ",input$boxPlotRadio)) + 
      ylab("Term Deposit") + 
      ggtitle( paste0("Boxplot for ",input$boxPlotRadio, " vs. Term deposit")) +
      geom_point(aes_string(color=input$boxPlotRadio), position="jitter", alpha = input$aplhaValue) 
    
  })
  
  #Handling Scatter plot
  output$scatterPlot <- renderPlot({
    
    g <- ggplot(bankDataUpdt(), aes_string(x = input$scatterPlotRadio1, y = input$scatterPlotRadio2))
    #scatter plot with linear regression line for carbs by protein
    g + geom_point(aes(color=y), position="jitter") +
      scale_color_discrete(name = "Type of Term deposit") +
      labs(title = paste(input$scatterPlotRadio1,  " vs. ", input$scatterPlotRadio2),
           x=input$scatterPlotRadio1, y=input$scatterPlotRadio2) + 
      geom_smooth(method = lm)  
    
  })
  
  #Handling correlation plot
  output$corrPlot <- renderPlot({
    
    corrplot.mixed(cor(bankDataUpdt()[, unlist(lapply(bankDataUpdt(), is.numeric))]),
                   lower = "number", 
                   upper = "circle",
                   tl.col = "black")
    
  })
  
  
#-----------------------------------------------------------------------------------------------------------------------#
#---------------------Summaries----------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------# 
  
  #subsetting the data and creating the summary dataTable
  summaryData <- reactive({
    
    summaryDf <- bankDataUpdt() %>%
      select(input$selectVariablesSumm) 
    summDf <- apply(summaryDf, MARGIN = 2, FUN = summary)
    rounded_summary <- round(summDf, input$roundValSumm)
    summaryDatFrame <- data.frame(rounded_summary)
    return(summaryDatFrame)
    
    
  })
  
  #Displaying numerical summaries of the data
  output$summTable <- renderDataTable({
    
    summaryData()
    
  })
  
  #Download functionality for summary table
  output$downloadSumm <- downloadHandler(
    filename = function(){"BankSummaryData.csv"}, 
    content = function(fname){
      write.csv(summaryData(), fname)
    }
  )
  
  #Contingency Table
  
  output$contTable <- renderDataTable({
    var <- input$contingencyRadio
    #print(get(var))
    as.data.frame.matrix(table(bankDataUpdt()[var][[1]],bankDataUpdt()['y'][[1]]))
    
  })

#-----------------------------------------------------------------------------------------------------------------------#
#---------------------Splitting the data--------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------# 
  
  dataset <- reactiveValues(train_data= NULL,test_data=NULL)
  
  observeEvent(input$goButton, {
    # Use the createDataPartition function to split the data
    # Set the seed to ensure that the split is reproducible
    set.seed(123)
    
    #dataset()$data <- bankDataUpdt()
    #df <- dataset()$data
    df <- bankDataUpdt()
    splitVar = input$splitRatio
    
    train_size <- sample(nrow(df),nrow(df)*splitVar)
    
    # Create a training set and a testing set
    train_data <- df[train_size, ]
    test_data <- df[-train_size, ]
    
  
    # Store the train and test sets in the reactive values object
    dataset$train_data <- train_data
    dataset$test_data <- test_data
    
  })
  
  #create split status info
  output$splitStatus <- renderText({
   
    paste("Data split was successful!!!")
    
  })
  
#-----------------------------------------------------------------------------------------------------------------------#
#---------------------Model Fitting-------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------#  
  
  modelFitValues <- reactiveValues(glmModelFit = NULL, classTreeFit = NULL, randForFit = NULL)
  
  
  observeEvent(input$trainButton, {
    
    subsetTrainglmDF <- dataset$train_data %>%
      select(all_of(input$glmPredictors),y)
    
    set.seed(123)
    modelFitValues$glmModelFit <- train(
      form = y ~ .,
      data = subsetTrainglmDF,
      trControl = trainControl(method = "cv", number = 5),
      method = "glm",
      family = "binomial"
    )
  #-------------------------------------------------------------------------------#  
    subsetTrainClassTreeDF <- dataset$train_data %>%
      select(all_of(input$classTreePredictors),y)
    
    modelFitValues$classTreeFit <- rpart(y ~ ., data = dataset$train_data)
  
  #-------------------------------------------------------------------------------#  
    
    subsetTrainRandTreeDF <- dataset$train_data %>%
      select(all_of(input$randomForestPredictors),y)
    
    modelFitValues$randForFit <- train(y ~ ., data=subsetTrainRandTreeDF, 
                                       method="rf",
                                       preProcess = c("center", "scale"),
                                       trControl = trainControl(method = "cv",number = 2),
                                       tuneGrid = data.frame(mtry = input$mtry)) 
    
  })
  
  output$trainAccuracyglm <- renderText({
    if(is.null(modelFitValues$glmModelFit)){
      
      "Model not trained"
    }else{
    
      paste(as.character(round((modelFitValues$glmModelFit$results$Accuracy * 100),2)),"%")
    }
    
  })
  
  output$testAccuracyglm <- renderText({
    if(is.null(modelFitValues$glmModelFit)){
      
      "Model not trained"
    }else{
      acc <- calc_acc(actual = dataset$test_data$y,
                      predicted = predict(modelFitValues$glmModelFit
                                          ,newdata = select(dataset$test_data,-y)))
      paste(as.character(round((acc * 100),2)),"%")
    }
    
  })
  
  output$confglm <- renderPrint({
    if(is.null(modelFitValues$glmModelFit)){
      
      "Model not trained"
    }else{
      
      confusionMatrix(predict(modelFitValues$glmModelFit,newdata = select(dataset$test_data,-y)),
                      dataset$test_data$y)
      
    }
    
  })
  
  output$summaryStatglm <- renderPrint({
      
      if(is.null(modelFitValues$glmModelFit)){
        
        "Model not trained"
      }else{
        
        summary(modelFitValues$glmModelFit)
      
        }
  })
  
  
  #-------------------------------------------------------------------------------#  
  
  output$trainAccuracyClassificationtree <- renderText({
    if(is.null(modelFitValues$classTreeFit)){
      
      "Model not trained"
    }else{
      acc <- calc_acc(actual = dataset$train_data$y,
               predicted = predict(modelFitValues$classTreeFit, 
                                   type = "class",newdata = select(dataset$train_data,-y)))
      paste(as.character(round((acc * 100),2)),"%")
    }
    
  })
  
  output$testAccuracyclass <- renderText({
    if(is.null(modelFitValues$classTreeFit)){
      
      "Model not trained"
    }else{
      acc <- calc_acc(actual = dataset$test_data$y,
                      predicted = predict(modelFitValues$classTreeFit, 
                                          type = "class",newdata = select(dataset$test_data,-y)))
      paste(as.character(round((acc * 100),2)),"%")
    }
    
  })
  
  output$confClass <- renderPrint({
    if(is.null(modelFitValues$classTreeFit)){
      
      "Model not trained"
    }else{
      
      confusionMatrix(predict(modelFitValues$classTreeFit, 
                             type = "class",newdata = select(dataset$test_data,-y)),
                      dataset$test_data$y)
      
    }
    
  })
  
  
  output$classTreePlot <- renderPlot({
    if(!is.null(modelFitValues$classTreeFit)){
      plot(modelFitValues$classTreeFit)
      text(modelFitValues$classTreeFit,pretty = 1, cex = 0.7)
    }
  })
  
  output$summaryStatClassTree <- renderPrint({
    
    if(is.null(modelFitValues$classTreeFit)){
      
      "Model not trained"
    }else{
      
      summary(modelFitValues$classTreeFit)
      
    }
  })

 #-------------------------------------------------------------------------------#  
  
  output$trainAccuracyRandFor <- renderText({
    if(is.null(modelFitValues$randForFit)){
      
      "Model not trained"
    }else{
      
      paste(as.character(round((modelFitValues$randForFit$results$Accuracy * 100),2)),"%")
    }
    
  })
  
  output$testAccuracyRand <- renderText({
    if(is.null(modelFitValues$randForFit)){
      
      "Model not trained"
    }else{
      acc <- calc_acc(actual = dataset$test_data$y,
                      predicted = predict(modelFitValues$randForFit, 
                                          newdata = select(dataset$test_data,-y)))
      paste(as.character(round((acc * 100),2)),"%")
    }
    
  })
  
  output$confRand <- renderPrint({
    if(is.null(modelFitValues$randForFit)){
      
      "Model not trained"
    }else{
      
      confusionMatrix(predict(modelFitValues$randForFit, 
                              newdata = select(dataset$test_data,-y)),
                      dataset$test_data$y)
      
    }
    
  })
  
  
  output$RandTreePlot <- renderPlot({
    if(!is.null(modelFitValues$classTreeFit)){
      
      plot(varImp(modelFitValues$randForFit))
      
    }
  })
  
  output$summaryStatRandTree <- renderPrint({
    
    if(is.null(modelFitValues$randForFit)){
      
      "Model not trained"
    }else{
      
      summary(modelFitValues$randForFit)
      
    }
  })
  
#-----------------------------------------------------------------------------------------------------------------------#
#---------------------Prediction Tab------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------# 
  
 output$finalPred <- renderText({
   
   if (is.null(modelFitValues$randForFit)){
     outputVal <- "Please train the model before predicting the output"
   } else {
     if (input$modelSelect == "log"){
       fitModel <- modelFitValues$glmModelFit
     }else if (input$modelSelect == "classTree"){
       fitModel <- modelFitValues$classTreeFit
     }else if (input$modelSelect == "randFor"){
       fitModel <- modelFitValues$randForFit
           }
   
     age <- input$agePred
     job <- input$jobPred
     marital <- input$maritalPred
     education <- input$educationPred
     default <- input$defaultPred
     housing <- input$housingPred
     loan <- input$loanPred
     contact <- input$contactPred
     month <- input$monthPred
     day_of_week <- input$dayPred
     duration <- input$durationPred
     campaign <- input$campaignPred
     pdays <- input$pDaysPred
     previous <- input$previousPred
     poutcome <- input$poutPred
     
     userDF <- tibble(age,
                       job,
                       marital,
                       education,
                       default,
                       housing,
                       loan,
                       contact,
                       month,
                       day_of_week,
                       duration,
                       campaign,
                       pdays,
                       previous,
                       poutcome)
     
     if(input$modelSelect == "classTree"){
       
       predictionVal <- predict(fitModel,type = "class",newdata = userDF)
       
     }else{          
        predictionVal <- predict(fitModel,newdata = userDF)
     }
     
     if (predictionVal== "yes"){
       outputVal = "Subscribed for Term deposit"
       }
     else if (predictionVal== "no"){
       outputVal = "Did not subscribe for Term deposit"
       }
   }
   outputVal
 })
  
})
