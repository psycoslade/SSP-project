Sys.which("make")
install.packages("jsonlite", type = "source")
install.packages("shinydashboard", "tmap", "tmaptools", "leaflet", "devtools")

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyverse)


#loading the data
setwd("C:/Users/Cies/Desktop/BISS/Smart Service Project/SSP dashboard")
DataFinal <-  read.csv("dataFin.csv", header = TRUE, sep = "," )
DataFinal



#here is a data frame with the tutorial messages that will show up in the task
#ui dynamic how-to video shows how to incorporate using apply function
TaskMessage <- c("Welcome to the Limburg Marketing dashboard! This tutorial will guide you through the use and update of this system to maximise your use of data.",
                 "The tabs to your left will allow you to interact with the data differently. the first tab will lead you to where you are now, Lifestyle Map, Descriptives show you an initial understanding of people attracted to middle Limburg for tourism",
                 "Predictive Analytics will further delve into trends that can be spotted and predictions that can be made",
                 "The last tab will guide you through the procedure of uploading new data and formating procedures",
                 "For user interaction we color coded our visualizations based on their purpose",
                 "blue boxes are outputs which will show you information",
                 "red boxes are inputs that allow you to adjust the variables of relevance",
                 "orange boxes are the core visualizations which will make understanding the trends simple",
                 "To conclude our tutorial read through the information shown in the data interaction tab") 
TaskDF <- data.frame("ButtonValue" = 1:9, "taskMessage" = c(TaskMessage))
TaskDF


#this is the current placeholder vector for regions
placeholder_region <- c("Limburg", "Brabant", "Gelderland")
placeholder_region
placeholder_lifestyle <- c("lifestyle 1", "lifestyle 2", "lifestyle 3")
placeholder_lifestyle

#placeholder value
PlaceholderValue <- 10

#this code is for the top bar and drop down messages
header <- dashboardHeader(title = "LM Dashboard",
  dropdownMenu(type = "messages",
               messageItem(from = "Group 3 SSP",
                           message = "Thank you for working with BISS Master students"),
  dropdownMenu(type = "tasks",
              taskItem(value = PlaceholderValue,
                       color = "aqua",
                       "Limburg Marketing Tutorial"))))

#LifestyleBinary creation
LifestyleDF <- DataFinal[,23:29]
  max(DataFinal[1,23:29])
  
  
#Table Selector creation
IncomeTable<- lm(mean_income ~ adventure + pleasure + harmony + connection + rest + insight + style, data=DataFinal)
IncomeTableSum <-summary(IncomeTable)
DOSTable <- lm(Duration.of.Stay ~ adventure + pleasure + harmony + connection + rest + insight + style, data=DataFinal)
DOSTableSum<- summary(DOSTable)
DOSIncomeTable <- lm(Duration.of.Stay ~ mean_income, data=DataFinal)
DOSIncomeTableSum <- summary(DOSIncomeTable)

summary(IncomeTable)

TableSelector <- c(IncomeTableSum, DOSTableSum, DOSIncomeTableSum)
TableSelectorNames <- c("Income regression results", "DOS regression results", "Income and DOS results")
TableSelectorList <-list(TableSelectorNames = TableSelector)
TableSelectorList["Income Regression results"]

#Plots to be used
IncomePlot <- ggplot(DataFinal, aes(mean_income)) + 
  geom_point(aes(y = adventure , colour = "Adventure")) + 
  geom_point(aes(y = pleasure , colour = "Pleasure")) +
  geom_point(aes(y = harmony , colour = "Harmony")) +
  geom_point(aes(y = connection , colour = "Connection")) +
  geom_point(aes(y = rest , colour = "Yellow")) +
  geom_point(aes(y = insight , colour = "Insight")) +
  geom_point(aes(y = style , colour = "Style"))

AdventurePlot <- ggplot(DataFinal, aes(x=adventure, y=mean_income)) + 
  geom_point() +
  geom_smooth(method = lm)
PleasurePlot <- ggplot(DataFinal, aes(x=pleasure, y=mean_income)) + 
  geom_point() +
  geom_smooth(method = lm)
HarmonyPlot <- ggplot(DataFinal, aes(x=harmony, y=mean_income)) + 
  geom_point() +
  geom_smooth(method = lm)
ConnectionPlot <- ggplot(DataFinal, aes(x=connection, y=mean_income)) + 
  geom_point() +
  geom_smooth(method = lm)
RestPlot <- ggplot(DataFinal, aes(x=rest, y=mean_income)) + 
  geom_point() +
  geom_smooth(method = lm)
InsightPlot <- ggplot(DataFinal, aes(x=insight, y=mean_income)) + 
  geom_point() +
  geom_smooth(method = lm)
StylePlot <- ggplot(DataFinal, aes(x=style, y=mean_income)) + 
  geom_point() +
  geom_smooth(method = lm)


IncomePlots <- c(AdventurePlot, PleasurePlot, HarmonyPlot, ConnectionPlot,
                    RestPlot, InsightPlot, StylePlot)
LifestyleSelector <- c("Adventure", "Pleasure", "Harmony", "Connection", "Rest", "Insight", "Style") 
LifestyleSelector
 
#------------------------------------------------------------------------------------------------------------------------
#this code is for the sidebar and new pages
#a lot of this code coincides with the body
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Tutorial",
             tabName = "Tutorial", icon = icon("location-arrow")),
    menuItem("Dashboard Map",
             tabName = "LifestyleMap", icon = icon("map")),
    menuItem("Dashboard Descriptives",
             tabName = "DescriptiveAnalytics", icon = icon("calculator")),
    menuItem("Dashboard Predictive",
             tabName = "PredictiveAnalytics", icon = icon("chart-line")),
    menuItem("Data interaction",
             tabName = "DataInteraction", icon = icon("database"))
  )
)

#this code is for the main body of data vis such as the mapping feature
body <- dashboardBody(
  tabItems(
#Tutorial
    tabItem(tabName = "Tutorial",
    fluidRow(
      shinydashboard::box(width = 12, title = "Tutorial", height= 150, status = "info", textOutput("TaskMessage")
    ),
      (actionButton("NextButton", "NEXT", icon = NULL, width = NULL)))),
#Lifestyle Mapping feature
    tabItem(tabName = "LifestyleMap",
    fluidRow(
      shinydashboard::box(width = 4, title = "Infobox 1", "showing number of results", height = 100, status = "info"),
      shinydashboard::box(width = 4, title = "Infobox 2", "showing highest lifestyle value", height = 100, status = "info"),
      shinydashboard::box(width = 4, title = "Infobox 3", "showing something else", height = 100, status = "info"),
      shinydashboard::box(width = 12, title = "Lifestyle Map", "This is where the Lifestyle Map will be", height = 400, status = "warning", solidHeader = TRUE),
      shinydashboard::box(width = 6, title = "Location Inputs", textInput("text", "Postal Code:"), selectInput(inputId = "Region", label =  "region",
                         choices = placeholder_region),solidHeader = TRUE, status = "danger"),
      shinydashboard::box(widtch = 6, title = "Variable Inputs", sliderInput("Years of Data", label = "Years of data", min = 2000, max = 2030, 
                          value = c(2008, 2018)),selectInput(inputId = "Lifestyle Search", label = "Lifestyle Search", choices = placeholder_lifestyle),
                          solidHeader = TRUE, status = "danger"))),
#Desctiptive analytics
    tabItem(tabName = "DescriptiveAnalytics",
      fluidRow(
        #these values need to be made interactive later
        infoBox("Mean Duration of Stay", mean(DataFinal$`Duration.of.Stay`), icon = icon("clock")),
        infoBox("total Households Included", nrow(DataFinal), icon = icon("hashtag")))),
#Predictive analytics
    tabItem(tabName = "PredictiveAnalytics",
            fluidRow(
              tabBox(title = "Predictive Regresions", side = "right", selected = "Income Regressions", width = 12, height = 500,
                tabPanel("Income Regressions"),
                tabPanel("Duration of Stay Regressions")),
                shinydashboard::box(width = 8, title = "Multiviate Regresion results", id = "TableRegressions", status = "info", solidHeader = TRUE, 
                                    textOutput(TableSelector)),
                shinydashboard::box(width = 4, title = "Selectors", status = "danger", 
                                    selectInput("TableMenu", label = "SelectMenu", choices = c("Income regression results", "DOS regression results", "Income and DOS results")))
              )
            ),
#Data Loading
    tabItem(tabName = "Data Interaction")))


#----------------------------------------------------------------------------------------------------------------------------

ui <- dashboardPage(header = header, sidebar = sidebar, body = body)

server <- function(input, output,session) {
  
output$TaskMessage <- renderText({
  TaskMessage[input$NextButton]})

output$TableSelector <- renderText({
  TableSelector[input$TableMenu]})

}

shiny::shinyApp(ui,server)

