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





shinyServer(function(input, output, session) {
  
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
  
})
