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
# ------------------
# Main title section
# ------------------

colNameList <- c("Age","Job Type", "Marital status", "Education", "Has Credit", "Housing", "Loan", "Contact Type",
                 "Month", "Day_of_the_week","Last_contact_duration","Campaign", "pDays(Last contacted)","Previous(Previously contacted or not)",
                 "pOutcome(previous marketing campaign)", "Term deposit")

ui <- navbarPage(
  "Bank Data Analysis",
  theme = shinytheme("cerulean"),
  tabPanel(
    "About",
    titlePanel(div(
      windowTitle = "bankImageSG",
      img(src = "bankImage2.jpg", width = "100%", class = "bg"),
    )),
    tags$hr(),
    tabsetPanel(
      type = "tabs",
      tabPanel( "Purpose of the App", 
                tags$style(HTML(
                  "#my-box {
                  font-family: 'Times New Roman';
                        font-size: 16px;}"
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
                        font-size: 16px;}"
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
#     
#     
#     ##########################################
#     ####  Panel: Main>Summary             ####
#     ##########################################
#     
#     tabsetPanel(
#       type = "tabs",
#       tabPanel(
#         "Summary",
#         ################################################
#         #### Panel: Main>Summary>Tables & Pie Chart ####
#         ################################################
#         
#         # ------------------
#         # ranking $ pie chart section
#         # ------------------
#         
#         sidebarLayout(
#           sidebarPanel(
#             h3("Data by Year"),
#             tags$br(),
#             selectInput(
#               "checkYear",
#               "Select Year",
#               choices = list("2018", "2017", "2016",
#                              "2015", "2014", "2013"),
#               selected = "2018"
#             )
#           ),
#           
#           mainPanel(
#             tabsetPanel(
#               type = "tabs",
#               tabPanel("Ranking", tableOutput("datahead")),
#               tabPanel("No. of Graduates", plotOutput(outputId = "piePlot"))
#             ),
#             tags$br(),
#             tags$br(),
#           )
#         ),
#         tags$hr(),
#         
#         
#         sidebarLayout(
#           sidebarPanel(
#             # ------------------
#             # Data overview filters
#             # ------------------
#             
#             h3("Data Overview"),
#             tags$br(),
#             setSliderColor(c("#2c3e50 ", "#2c3e50"), c(1, 2)),
#             sliderInput(
#               "incomeRange",
#               label = "Salary Range",
#               min = 1600,
#               max = 5000,
#               value = c(1600, 5000)
#             ),
#             # setSliderColor(c("e67e22 ", "#e67e22"), c(1, 2)),
#             sliderInput(
#               "employRange",
#               label = "Employment Rate Range",
#               min = 0,
#               max = 100,
#               value = c(0, 100)
#             ),
#             selectInput(
#               "checkYearGroup",
#               "Select Year",
#               choices = data$year,
#               selected = "2018",
#               multiple = TRUE
#             ),
#             
#             #checkboxGroupInput("checkYear", label = "Select Year",
#             #                  choices = list("2013", "2014", "2015", "2016", "2017", "2018"),
#             #                 selected = list("2013", "2014", "2015", "2016", "2017", "2018"), inline = TRUE),
#             
#             actionButton("actionDT", "Filter", class = "btn btn-warning"),
#           ),
#           mainPanel(
#             h3("Browse All"),
#             tags$br(),
#             dataTableOutput("myTable"),
#             tags$br(),
#             tags$br(),
#           )
#         ),
#         tags$hr(),
#       ),
#       
#       
#       ################################################
#       #### Panel: Main>Plots                      ####
#       ################################################
#       
#       tabPanel(
#         "Visual Comparison",
#         
#         # --------------------
#         # density plot section
#         # --------------------
#         
#         sidebarLayout(
#           sidebarPanel(
#             h3("Density Plot Panel"),
#             tags$br(),
#             selectInput(
#               "selectvar",
#               label = "Choose a variable to display",
#               choices = c(
#                 "Basic Montly Salary (Median)" = "basic_monthly_median",
#                 "Fulltime Employment Rate" = "employment_rate_ft_perm"
#               ),
#               selected = "basic monthly mean"
#             ),
#             
#             checkboxGroupInput(
#               "checkGroup",
#               label = "Select University",
#               choices = list(
#                 "Nanyang Technological University" = "Nanyang Technological University",
#                 "National University of Singapore" = "National University of Singapore",
#                 "Singapore Institute of Technology" = "Singapore Institute of Technology",
#                 "Singapore Management University" = "Singapore Management University",
#                 "Singapore University of Social Sciences" = "Singapore University of Social Sciences",
#                 "Singapore University of Technology and Design" = "Singapore University of Technology and Design"
#               ),
#               selected = list(
#                 "Nanyang Technological University" = "Nanyang Technological University",
#                 "National University of Singapore" = "National University of Singapore",
#                 "Singapore Institute of Technology" = "Singapore Institute of Technology",
#                 "Singapore Management University" = "Singapore Management University",
#                 "Singapore University of Social Sciences" = "Singapore University of Social Sciences",
#                 "Singapore University of Technology and Design" = "Singapore University of Technology and Design"
#               )
#             ),
#           ),
#           mainPanel(
#             h3("Distribution"),
#             plotlyOutput(outputId = "densityPlot"),
#             tags$br(),
#             tags$br()
#           )
#         ),
#         tags$hr(),
#         
#         # --------------------
#         # bar plot section
#         # --------------------
#         sidebarLayout(
#           sidebarPanel(
#             h3("Bar Plot Panel"),
#             tags$br(),
#             radioButtons(
#               "radio",
#               label = "Select University",
#               choices = list(
#                 "Nanyang Technological University" = "Nanyang Technological University",
#                 "National University of Singapore" = "National University of Singapore",
#                 "Singapore Institute of Technology" = "Singapore Institute of Technology",
#                 "Singapore Management University" = "Singapore Management University",
#                 "Singapore University of Social Sciences" = "Singapore University of Social Sciences",
#                 "Singapore University of Technology and Design" = "Singapore University of Technology and Design"
#               ),
#               selected = "Nanyang Technological University"
#             ),
#             tags$hr()
#           ),
#           mainPanel(
#             h3("Median Income by School (aggregate)"),
#             plotlyOutput(outputId = "uniPlot"),
#             tags$br(),
#             tags$br()
#           )
#         ),
#         tags$hr(),
#         
#         # --------------------
#         # box plot section
#         # --------------------
#         sidebarLayout(
#           sidebarPanel(
#             h3("Box Plot Panel"),
#             tags$br(),
#             checkboxGroupInput(
#               "checkGroupbox",
#               label = "Select University",
#               choices = list(
#                 "Nanyang Technological University" = "Nanyang Technological University",
#                 "National University of Singapore" = "National University of Singapore",
#                 "Singapore Institute of Technology" = "Singapore Institute of Technology",
#                 "Singapore Management University" = "Singapore Management University",
#                 "Singapore University of Social Sciences" = "Singapore University of Social Sciences",
#                 "Singapore University of Technology and Design" = "Singapore University of Technology and Design"
#               ),
#               selected = list(
#                 "Nanyang Technological University" = "Nanyang Technological University",
#                 "National University of Singapore" = "National University of Singapore",
#                 "Singapore Institute of Technology" = "Singapore Institute of Technology",
#                 "Singapore Management University" = "Singapore Management University",
#                 "Singapore University of Social Sciences" = "Singapore University of Social Sciences",
#                 "Singapore University of Technology and Design" = "Singapore University of Technology and Design"
#               )
#             ),
#             
#             tags$hr()
#           ),
#           mainPanel(
#             h3("Median Income Comparison (aggregate)"),
#             plotlyOutput(outputId = "boxPlot"),
#             tags$br(),
#             tags$br(),
#             tags$br(),
#           )
#         ),
#         
#         tags$hr(),
#         
#         # --------------------
#         # Scatter plot section
#         # --------------------
#         
#         
#         fluidPage(fluidRow(
#           h3("Fulltime Employment Rate vs. Median Income by University in 2018"),
#           align = "center",
#           plotlyOutput(outputId = "scatPlot", width = "100%"),
#           div(style = "height:400px")
#         )),
#         
#         tags$br(),
#         tags$br(),
#         tags$hr(),
#         
#       ),
#       
#       
#       ################################################
#       #### Panel: Main>Details                    ####
#       ################################################
#       
#       tabPanel(
#         "Details By University",
#         h3("Graduates' Income and Employment Rate by Year", align = "center"),
#         br(),
#         div(style = "display:vertical-align:center;center-align",
#             fluidRow(
#               column(
#                 4,
#                 selectInput(
#                   "detailUniversity",
#                   label = "Select University",
#                   choices = unique(data$university),
#                   selected = "National University of Singapore",
#                   width = 400
#                 ),
#               ),
#               column(
#                 4,
#                 selectInput(
#                   "detailSchool",
#                   "Select School",
#                   choices = "",
#                   selected = "",
#                   width = 400
#                 )
#               ),
#               column(4,
#                      column(
#                        8,
#                        selectInput(
#                          "detailMajor",
#                          "Select Program",
#                          choices = "",
#                          selected = "",
#                          width = 400
#                        )
#                      ),
#                      column(
#                        4,
#                        tags$br(),
#                        actionButton("detailFilter", "Filter", class = "btn btn-warning btn-sm")
#                      ))
#             )),
#         
#         tags$br(),
#         tags$br(),
#         tags$hr(),
#         tags$br(),
#         
#         fluidRow(
#           column(4, tableOutput("detailTable")),
#           column(4, h5("Montly Median Income", align="center"), plotOutput(outputId = "detailPlot", height = "300px")),
#           column(4, h5("Fulltime Employment rate", align="center"), plotOutput(outputId = "detailPlotem", height = "300px"))
#         ),
#         
#         tags$br(),
#         tags$br(),
#         tags$br(),
#         tags$br(),
#         tags$hr(),
#         tags$br()
#       )
#     )
  ),
#   
#   
#   
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
                      tags$br(),
                       fluidRow(
                         column(
                           4,
                           radioButtons("barPlotRadio","Select the variable for Bar plot:",
                                        choices = c("Job Type" = "job",
                                                    "Marital Status" = "marital",
                                                    "Education Type" = "education",
                                                    "Contact type" = "contact"),
                                        selected = "job"),
                           numericInput("twistAngle","Select the angle of text on the x-axis:",
                                        value = 90, min = 18, max = 90)
                         ),
                         column(
                           8, h3("Bar plot", align = "center"),
                           plotOutput("barPlot")
                         )
                       ),
                       tags$br(),
                       fluidRow(
                         column(
                           4,
                           radioButtons("histPlotRadio","Select the variable for Histogram plot:",
                                        choices = c("Age" = "age",
                                                    "Duration" = "duration",
                                                    "Campaign" = "campaign"),
                                        selected = "age"),
                           numericInput("binWidth","Select the binwidth:",
                                        value = 2, min = 1, max = 30)
                         ),
                         column(
                           8,h3("Histogram plot", align = "center"),
                           plotOutput("histPlot")
                           
                         )
                       )),
             tabPanel("Summaries",
                      tags$br(),
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
                      tags$br(),
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
                     "Model Fitting"),
                   tabPanel(
                     "Prediction")
           )
          )
)
