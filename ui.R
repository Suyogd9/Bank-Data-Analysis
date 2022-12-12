#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
##########################################
####   Shiny ui                       ####
##########################################
library(shinyWidgets)
library(shiny)
library(shinyWidgets)
library(shiny)
library(plotly)
library(shinythemes)
library(DT)
library(rsconnect)
library(jpeg)
library(shinydashboard)
library(dplyr)
library(ggpubr)
library(corrplot)
# ------------------
# Main title section
# ------------------

colNameList <- c("Age","Job Type", "Marital status", "Education", "Has Credit", "Housing", "Loan", "Contact Type",
                 "Month", "Day_of_the_week","Last_contact_duration","Campaign", "pDays(Last contacted)","Previous(Previously contacted or not)",
                 "pOutcome(previous marketing campaign)", "Term deposit")

bankDataUI <- read_csv("data/bankData.csv")
bankDataUI <- bankDataUI %>%
  mutate(education = gsub("\\.", " ",education)) %>%
  mutate(pdays = if_else(pdays == 999,0, pdays))
bankDataUI <- mutate_if(bankDataUI, is.character, as.factor)

ui <- navbarPage(
  "Bank Data Analysis",
  theme = shinytheme("cerulean"),
  tabPanel(
    "About",
    titlePanel(div(
      windowTitle = "bankImageSG",
      img(src = "bankImage2.jpg", width = "100%", class = "bg"),
    )),
    tabsetPanel(
      type = "tabs",
      tabPanel( "Purpose of the App", 
                tags$style(HTML(
                  
                  "#my-box {
                  font-family: 'Times New Roman';
                        font-size: 16px;}
                  
                  .sidebar {
                     border: none;
                    }
                  
                  "
                )),
                box(title = "",
                    id = "my-box",
                    width = 100,
                    height = 300,
                   p("The Portuguese Bank has recently experienced a decline in revenue, and they are looking for ways to reverse this trend. 
                     After investigating the issue, they discovered that their customers are not investing enough in long-term deposits, which 
                     is negatively impacting the bank's performance. As a result, the bank is interested in identifying which existing customers 
                     are more likely to invest in long-term deposits, and directing their marketing efforts towards these customers. The app is 
                     designed to help users identify these potential customers and focus their marketing efforts accordingly. I believe that 
                     these observations and predictions can be applied to other countries with similar banking systems and population demographics."),
                   p("Below, you can find brief information about the tabs and their purposes."),
                   p(strong(em("The About page:")),br()," This page provides a brief description of the app's purpose, some information 
                   about the data, and links to the dataset and additional information. It also includes 
                   guidance on the use of each tab in the app."),
                   p(strong(em("The Data page:")),br()," This page allows users to browse and filter the data using the dropdown menus. 
                     Users can choose to display a specific number of records and even download a CSV file 
                     of the filtered data for further analysis. The page is designed to make it easy for users 
                     to explore and understand the data in the app."),
                   p(strong(em("The Data Exploration:")),br()," This page offers an in-depth analysis of the bank data, allowing users to 
                     create numerical and graphical summaries, change the type of plots displayed, customize the 
                     variables used for the summaries, and modify the data in the plots. This page provides users 
                     with a powerful tool for understanding and interpreting the data, enabling them to gain valuable 
                     insights and make informed decisions"),
                   p(strong(em("The Modeling page:")),br()," This offers a range of tools for fitting supervised learning models, including binary 
                     logistic regression, classification tree, and random forest tree. The page includes three additional 
                     tabs that provide detailed information about the fitted models, allow users to choose the variables 
                     used for model fitting and training, and display model training accuracy, test accuracy, and summary 
                     statistics. The Prediction tab enables users to select the values of the predictors and obtain a prediction 
                     for the response, providing a powerful tool for data analysis and decision making."),
                   br(),
                   br(),
                   br()
                   )
        ),
      tabPanel( "About the dataset", 
                tags$style(HTML(
                  "#my-box1 {
                  font-family: 'Times New Roman';
                        font-size: 16px;}
                  
                  .sidebar {
                     border: none;
                    }
                  
                  "
                )),
                box(title = "",
                    id = "my-box1",
                    width = 100,
                    height = 300,
                    p("The data used in this app pertains to direct marketing campaigns conducted by a Portuguese banking institution. The campaigns 
                      were conducted primarily through phone calls, and in some cases, multiple contacts were made to a single client in order to determine 
                      their interest in subscribing to a bank term deposit product. The data includes information on the clients' responses to the marketing 
                      efforts, as well as other relevant details such as their age, job, and educational background."),
                    p("The classification goal is to predict if the client will subscribe (yes/no) a ",strong(em("term deposit")),"."),
                    p("For more information on the dataset, please visit the ", a("website", href="https://archive.ics.uci.edu/ml/datasets/Bank+Marketing#"),
                      "." ),
                    p(strong(em("Source: ")), br(), "[Moro et al., 2014] S. Moro, P. Cortez and P. Rita. A Data-Driven Approach to Predict the Success of Bank Telemarketing. 
                      Decision Support Systems, Elsevier, 62:22-31, June 2014")
                  )
              )
      ),

    tags$br()
  ),
  ################################################
  #### Panel: Data                             ####
  ################################################

  tabPanel("Data",
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("selectVariables","Select variables for Filtering:",
                                  choices = colNameList,
                                  selected = colNameList),
               numericInput("numberOfRows","Select the number of rows to filter: (minimum value is 1)",
                            value =  10, min = 1, max = 32950),
               downloadButton('download',"Download the data (CSV)")
               ),
             mainPanel(
               h3("Dataset"),
               dataTableOutput("table")
             )
             )
           ),

#  
  ################################################
  #### Panel: Data Exploration                ####
  ################################################
  tabPanel("Data Exploration ",
           tabsetPanel(
             type = "tabs",
             tabPanel( "Plots",
                       sidebarLayout(
                         sidebarPanel(
                           radioButtons("barPlotRadio","Select the variable for Bar plot:",
                                        choices = c("Job Type" = "job",
                                                    "Marital Status" = "marital",
                                                    "Education Type" = "education",
                                                    "Contact type" = "contact"),
                                        selected = "job"),
                           numericInput("twistAngle","Select the angle of text on the x-axis:",
                                        value = 90, min = 18, max = 90)
                         ),
                         mainPanel(
                           h3("Bar plot", align = "center"),
                           plotOutput("barPlot")
                         )
                       ),
                       tags$br(),
                      sidebarLayout(
                        sidebarPanel(
                           radioButtons("histPlotRadio","Select the variable for Histogram plot:",
                                        choices = c("Age" = "age",
                                                    "Duration" = "duration",
                                                    "Campaign" = "campaign"),
                                        selected = "age"),
                           numericInput("binWidth","Select the binwidth:",
                                        value = 2, min = 1, max = 30)
                         ),
                        mainPanel(

                           h3("Histogram plot", align = "center"),
                           plotOutput("histPlot")
                           
                         )
                       ),
                      sidebarLayout(
                        sidebarPanel(
                                 radioButtons("boxPlotRadio","Select the variable for Box plot:",
                                              choices = c("Age" = "age",
                                                          "Duration" = "duration",
                                                          "Campaign" = "campaign",
                                                          "Last contacted from a previous campaign" = "pdays",
                                                          "Contacts performed before this campaign" = "previous"),
                                              selected = "age"),
                                 numericInput("aplhaValue","Select the opacity of the points:",
                                              value = 0.03, min = 0, max = 1)
                               ),
                        mainPanel(
                                 h3("Box plot", align = "center"),
                                 plotOutput("boxPlot")
                                 
                               )
                        ),
                      sidebarLayout(
                        sidebarPanel(
                          
                          radioButtons("scatterPlotRadio1","Select the First variable for Scatter plot:",
                                       choices = c("Age" = "age",
                                                   "Duration" = "duration",
                                                   "Campaign" = "campaign",
                                                   "Last contacted from a previous campaign" = "pdays",
                                                   "Contacts performed before this campaign" = "previous"),
                                       selected = "age"),
                          radioButtons("scatterPlotRadio2","Select the Second variable for Scatter plot:",
                                       choices = c("Age" = "age",
                                                   "Duration" = "duration",
                                                   "Campaign" = "campaign",
                                                   "Last contacted from a previous campaign" = "pdays",
                                                   "Contacts performed before this campaign" = "previous"),
                                       selected = "duration"),
                        ),
                        mainPanel(
                          h3("Scatter plot", align = "center"),
                          plotOutput("scatterPlot")
                          
                        )
                      ),
                      
                      fluidRow(
                        column(
                          2
                        ),
                        column(style = "border: 4px double #54B6F9;",
                          8,
                          h3("Correlation plot", align = "center"),
                          plotOutput("corrPlot")
                        )
                      )
                      ),
             tabPanel("Summaries",
                      fluidRow(
                        column(
                          4,
                          checkboxGroupInput("selectVariablesSumm","Select variables for numerical summaries:",
                                             choices = c("Age" = "age",
                                                         "Duration" = "duration",
                                                         "Campaign" = "campaign",
                                                         "Last contacted from a previous campaign" = "pdays",
                                                         "contacts performed before this campaign" = "previous"),
                                             selected =  c("age",
                                                           "duration",
                                                           "campaign",
                                                           "pdays",
                                                           "previous")),
                          numericInput("roundValSumm","Round the mean value by:", 2, min = 1, max = 10),
                          downloadButton('downloadSumm',"Download the data (CSV)")
                        ),
                        column(
                          8,h3("Numerical Summaries", align = "center"),
                          dataTableOutput("summTable")
                          )
                      ),
                      hr(),
                      fluidRow(
                        column(
                          4,
                          radioButtons("contingencyRadio","Select the variable for contigency table:",
                                       choices = c("Marital Status" = "marital",
                                                   "Type of Education" = "education",
                                                   "Type of Contact" = "contact"),
                                       selected = "marital"),
                        ),
                        column(
                          8,h3("Contigency table with term deposit", align = "center"),
                          dataTableOutput("contTable")
                          
                        )
                      )
                    )
             )
           ),

  ################################################
  #### Panel: Modeling                        ####
  ################################################
  tabPanel("Modeling ",
           tabsetPanel(
                   type = "tabs",
                   tabPanel(
                     "Modeling Info"),
                   tabPanel(
                     "Model Fitting",
                     fluidRow(
                       height = 100,
                       column(
                         3
                       ),
                       column(
                         6,
                         tags$style(".col-sm-6 {border: 4px solid #54B6F9;}"),
                         tags$br(),
                         box(width = 12,
                             border = FALSE,
                             height = "320px",
                             h3(style = "font-size: 16px; font-style: italic; color: black;",
                             "Please select the value of the split ratio by using the slider input giving alongside. 
                              After selecting the split ratio click",strong("Split Data"),"button that will split the 
                              data into training and test data.This can be further use to train and test the models."),
                              br(),
                               sliderInput(
                                 inputId = "splitRatio",
                                 label = "Split Lamp to be used for Training Data:",
                                 min = 0.1,
                                 max = 0.9,
                                 value = 0.7
                               ),
                               actionButton("goButton", "Split Data"),
                               hr(),
                               conditionalPanel(
                                inputId = "splitText",
                                condition = "input.goButton",
                                textOutput("splitStatus")
                              )
                             )
                           )
                       ),
                     br(),
                     hr(),
                     fluidRow(
                       column(
                         4,
                         tags$style(".col-sm-4 {border: 4px solid #54B6F9;}"),
                         checkboxGroupInput("glmPredictors",
                                              "Please select the variables for fitting logistic regression model:",
                                              choices = c("Age" = "age",
                                                          "Job Type" = "job",
                                                          "Marital Status" = "marital",
                                                          "Type of education" = "education",
                                                          "Has default credit" = "default", 
                                                          "Housing status" = "housing",
                                                          "Loan status" = "loan",
                                                          "Type of contact" = "contact",
                                                          "Month" = "month",
                                                          "Day of the week" = "day_of_week",
                                                          "Duration" = "duration",
                                                          "Campaign" = "campaign",
                                                          "Last contacted from a previous campaign" = "pdays",
                                                          "contacts performed before this campaign" = "previous",
                                                          "outcome of the previous marketing campaign" = "poutcome"),
                                              selected = c("Age" = "age",
                                                           "Job Type" = "job",
                                                           "Marital Status" = "marital",
                                                           "Type of education" = "education",
                                                           "Has default credit" = "default", 
                                                           "Housing status" = "housing",
                                                           "Loan status" = "loan",
                                                           "Type of contact" = "contact",
                                                           "Month" = "month",
                                                           "Day of the week" = "day_of_week",
                                                           "Duration" = "duration",
                                                           "Campaign" = "campaign",
                                                           "Last contacted from a previous campaign" = "pdays",
                                                           "contacts performed before this campaign" = "previous",
                                                           "outcome of the previous marketing campaign" = "poutcome"))
                       ),
                       column(
                         4,
                         tags$style(".col-sm-4 {border: 4px solid #54B6F9;}"),
                         checkboxGroupInput("classTreePredictors",
                                              "Please select the variables for fitting classification tree model:",
                                              choices = c("Age" = "age",
                                                          "Job Type" = "job",
                                                          "Marital Status" = "marital",
                                                          "Type of education" = "education",
                                                          "Has default credit" = "default", 
                                                          "Housing status" = "housing",
                                                          "Loan status" = "loan",
                                                          "Type of contact" = "contact",
                                                          "Month" = "month",
                                                          "Day of the week" = "day_of_week",
                                                          "Duration" = "duration",
                                                          "Campaign" = "campaign",
                                                          "Last contacted from a previous campaign" = "pdays",
                                                          "contacts performed before this campaign" = "previous",
                                                          "outcome of the previous marketing campaign" = "poutcome"),
                                              selected = c("Age" = "age",
                                                           "Job Type" = "job",
                                                           "Marital Status" = "marital",
                                                           "Type of education" = "education",
                                                           "Has default credit" = "default", 
                                                           "Housing status" = "housing",
                                                           "Loan status" = "loan",
                                                           "Type of contact" = "contact",
                                                           "Month" = "month",
                                                           "Day of the week" = "day_of_week",
                                                           "Duration" = "duration",
                                                           "Campaign" = "campaign",
                                                           "Last contacted from a previous campaign" = "pdays",
                                                           "contacts performed before this campaign" = "previous",
                                                           "outcome of the previous marketing campaign" = "poutcome"))
                       ),
                       column(
                         4,
                         tags$style(".col-sm-4 {border: 4px solid #54B6F9;}"),
                         checkboxGroupInput("randomForestPredictors",
                                              "Please select the variables for fitting Random forest model:",
                                              choices = c("Age" = "age",
                                                          "Job Type" = "job",
                                                          "Marital Status" = "marital",
                                                          "Type of education" = "education",
                                                          "Has default credit" = "default", 
                                                          "Housing status" = "housing",
                                                          "Loan status" = "loan",
                                                          "Type of contact" = "contact",
                                                          "Month" = "month",
                                                          "Day of the week" = "day_of_week",
                                                          "Duration" = "duration",
                                                          "Campaign" = "campaign",
                                                          "Last contacted from a previous campaign" = "pdays",
                                                          "contacts performed before this campaign" = "previous",
                                                          "outcome of the previous marketing campaign" = "poutcome"),
                                              selected = c("Age" = "age",
                                                           "Job Type" = "job",
                                                           "Marital Status" = "marital",
                                                           "Type of education" = "education",
                                                           "Has default credit" = "default", 
                                                           "Housing status" = "housing",
                                                           "Loan status" = "loan",
                                                           "Type of contact" = "contact",
                                                           "Month" = "month",
                                                           "Day of the week" = "day_of_week",
                                                           "Duration" = "duration",
                                                           "Campaign" = "campaign",
                                                           "Last contacted from a previous campaign" = "pdays",
                                                           "contacts performed before this campaign" = "previous",
                                                           "outcome of the previous marketing campaign" = "poutcome")),
                         br(),
                         sliderInput(
                           inputId = "mtry",
                           label = "Select the number of random variables collected at each split:(mtry)",
                           min = 1,
                           max = 8,
                           value = 4
                         )
                       )
                     ),
                     br(),
                     hr(),
                     fluidRow(
                       column(3),
                       column(6,
                              tags = list(
                                tags$style(".col-sm-6 {border: 4px solid #54B6F9;}")),
                              tags$br(),
                              box(width = 12,
                                  height = "220px",
                                  h3(style = "font-size: 16px; font-style: italic; color: black;",
                                     "After selecting the variables for all the models, click",strong("Train Models"),
                                     "button that will train all the three models and all the results can be seen below."),
                                  br(),
                                  h3(style = "font-size: 16px; font-style: italic; color: #D8320E;",
                                     "Warning!! Random Forest takes approximately 2 minutes to run with default settings...
                                      Please be patient!!"),
                                  br(),
                                  actionButton("trainButton", "Train Models"),
                                  br()
                              )),
                       column(3)
                     ),
                     br(),
                     hr(),
                     fluidRow(
                       column(
                         4,
                          h3("Traning accuracy for generalized linear regression model:"),
                          verbatimTextOutput("trainAccuracyglm"),
                          br(),
                          h3("Testing accuracy for generalized linear regression model:"),
                          verbatimTextOutput("testAccuracyglm"),
                          br(),
                          h3("Confusion matrix for generalized linear regression model:"),
                          verbatimTextOutput("confglm"),
                          br(),
                          h3("Summary statistics for generalized linear regression model:"),
                          verbatimTextOutput("summaryStatglm")
                       ),
                       column(
                         4,
                         h3("Traning accuracy for classification tree:"),
                         verbatimTextOutput("trainAccuracyClassificationtree"),
                         br(),
                         h3("Testing accuracy for classification tree:"),
                         verbatimTextOutput("testAccuracyclass"),
                         br(),
                         h3("Confusion matrix for classification tree:"),
                         verbatimTextOutput("confClass"),
                         br(),
                         plotOutput("classTreePlot"),
                         br(),
                         h3("Summary statistics for classification tree:"),
                         verbatimTextOutput("summaryStatClassTree")
                       ),
                       column(
                         4,
                         h3("Traning accuracy for Random Forest model:"),
                         verbatimTextOutput("trainAccuracyRandFor"),
                         br(),
                         h3("Testing accuracy for Random Forest model:"),
                         verbatimTextOutput("testAccuracyRand"),
                         br(),
                         h3("Confusion matrix for Random Forest model:"),
                         verbatimTextOutput("confRand"),
                         br(),
                         plotOutput("RandTreePlot"),
                         br(),
                         h3("Summary statistics for Random forest tree:"),
                         verbatimTextOutput("summaryStatRandTree")
                         
                       ),
                     )
                     ),
                   tabPanel(
                     "Prediction",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("modelSelect","Select the model for prediction:",
                                      c("Binary Logistic Regression"="log",
                                        "Classification Tree"="classTree",
                                        "Random Forest"="randFor"),
                                      selected = "log"),
                          br(),
                          numericInput("agePred","Select the age:", value = 26,
                                       min = 17, max=98),
                          br(),
                          selectInput("jobPred","Select the Job type for prediction:",
                                      unique(bankDataUI$job)),
                          br(),
                          selectInput("maritalPred","Select the Marital status for prediction:",
                                      unique(bankDataUI$marital)),
                          br(),
                          selectInput("educationPred","Select the type of education for prediction:",
                                      unique(bankDataUI$education)),
                          br(),
                          selectInput("deafultPred","Select the type of default credit for prediction:",
                                      unique(bankDataUI$default)),
                          br(),
                          selectInput("housingPred","Select the type of housing for prediction:",
                                      unique(bankDataUI$housing)),
                          br(),
                          selectInput("loanPred","Select the if the customer has taken loan for prediction:",
                                      unique(bankDataUI$loan)),
                          br(),
                          selectInput("contactPred","Select the type of contact used for connecting:",
                                      unique(bankDataUI$contact)),
                          br(),
                          selectInput("monthPred","Select the month in which the customer was contacted:",
                                      unique(bankDataUI$month)),
                          br(),
                          selectInput("dayPred","Select the day of the week on which the customer was contacted:",
                                      unique(bankDataUI$day_of_week)),
                          br(),
                          numericInput("durationPred","Select the duration of the call(in seconds):", value = 258,
                                       min = 0, max=4918),
                          br(),
                          numericInput("campaignPred","Select  number of contacts performed during this campaign for a 
                                       particular customer:",
                                       value = 3,
                                       min = 1, max=56),
                          br(),
                          numericInput("pDaysPred","Select number of days that passed by after the customer was last
                                       contacted:",
                                       value = 3,
                                       min = 0, max=27),
                          br(),
                          numericInput("previousPred","Select  number of contacts performed before this campaign for
                                       this customer:",
                                       value = 2,
                                       min = 0, max=7),
                          br(),
                          selectInput("poutPred","Select outcome of the previous marketing campaign :",
                                      unique(bankDataUI$poutcome)),
                          br()
                        ),
                        mainPanel(
                          textOutput("finalPred")
                        )
                      )
                     )
           )
          )
)
