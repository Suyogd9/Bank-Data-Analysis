# ST558_Project4

--- 
## About the App
This application was developed as part of a course project for ST 558 in the Fall 2022 semester. The aim of the project was to create a Shiny app that allows users to explore and model data. The app was designed to be user-friendly and intuitive, so that anyone can use it to analyze and understand their data. The app includes a range of features and tools that allow users to visualize, explore, and model their data, and to interpret the results of their analyses. Whether you are a student, researcher, or data scientist, this app can be a valuable tool for exploring and understanding your data.

Shiny is an open-source R package that makes it simple to build interactive web applications directly from R. With Shiny, you can create standalone apps that can be hosted on a webpage or embedded in R Markdown documents, or you can build dashboards to display your data and analyses. Shiny allows you to add reactive elements to your app, so that the app automatically updates whenever the underlying data changes. This makes it easy to create dynamic and responsive apps that can help you and others explore and understand your data. Whether you are a beginner or an experienced R user, Shiny can help you create powerful and engaging apps with minimal effort. 

The app is built on the Portuguese Bank data and uses predictive modelling to identify the customers who are most likely to invest in long-term deposits. The app allows users to explore the data and model it to make predictions about which customers are most likely to invest. It also includes visualizations and other tools that help users interpret the results of their analyses.

By using Shiny to create this app, I was able to make the data and analysis accessible and interactive, which can help the bank and others understand the data and make better-informed decisions.

The [Bank Dataset](https://archive.ics.uci.edu/ml/datasets/Bank+Marketing#) used in this app pertains to direct marketing campaigns conducted by a Portuguese banking institution. The campaigns were conducted primarily through phone calls, and in some cases, multiple contacts were made to a single client in order to determine their interest in subscribing to a bank term deposit product. The data includes information on the clients' responses to the marketing efforts, as well as other relevant details such as their age, job, and educational background.

This application is built using the [shinydashboard](https://rstudio.github.io/shinydashboard/index.html) package, which is an extension of the Shiny package that allows you to create custom dashboards for your Shiny apps. With shinydashboard, you can easily customize the header, sidebar, and body of your app to create a professional-looking and intuitive interface. This makes it easier for users to navigate your app and access the different features and tools that you have included.

---
## Packages required to run the app

1. shiny
2. shinydashboard
3. tidyverse
4. DT
5. corrplot
6. caret
7. jpeg
8. shinyWidgets
9. shinythemes
10. rsconnect
11. dplyr
12. ggpubr
13. corrplot
14. rpart

### R code for installing packages

Copy this code snippet and run it on R terminal to install the required packages.
```
install.packages(c("shiny","shinydashboard","tidyverse","DT","corrplot","caret","jpeg","shinyWidgets","shinythemes","rsconnect","dplyr","ggpubr","corrplot","rpart"))
```

### Running the app
You can run this app without having to clone this repository. After installing the required packages given above, run the following command in your R terminal
```
shiny::runGitHub("ST558-Final-Project","Suyogd9")
```
