#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(DT)
library(shinydashboard)

shinyUI(dashboardPage(skin="green",
  dashboardHeader(title = "Examining French Baccalaureat Passing Rates",
                  titleWidth = 460),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About",tabName = "about",icon = icon("info-circle")),
      menuItem("National-Level",tabName = "map",icon = icon("map")),
      menuItem("Department-Level",tabName = "data",icon = icon("database")),
      menuItem("Lycee-Level",tabName = "data6", icon = icon("database"))
    )
  ),
  
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",h3("Context, Data and Author"),
              column(width=1),
              column(width = 8),
              includeMarkdown("Intro.Rmd"),
              column(width=3)),
      tabItem(tabName = "data6", h3("Lycee Data: Student Distribution & Passing Rates"),
              DT::dataTableOutput("table")),
      tabItem(tabName = "map",
              fluidRow(
                column(width=6,h3("Overiew at National Level"))),
              fluidRow(column(width=5)),
              fluidRow(
                column(width=3,offset=1,selectizeInput('exam',label = NULL, choices = L1))),
              fluidRow(column(width = 5)),
              fluidRow(
                column(width = 5,leafletOutput("map2", width = "100%", height = "400px"))
              ),
              fluidRow(
                uiOutput("minBox"),
                uiOutput("avgBox"),
                uiOutput("maxBox"))
              ),
      tabItem(tabName = "data", h3("Key Statistics by Departement"),
              fluidRow(),
              selectizeInput('Departements', label = NULL, choices = L,options = list(placeholder = 'Type a Departement name')),
              column(width = 5,plotOutput("box1", width = "106%", height = "310px")),
              column(width = 5,plotOutput("box2", width = "110%", height = "310px")),
              column(width= 5,plotOutput("box3",width = "106%", height = "310px")),
              column(width=5, plotOutput("bar1",width = "110%", height = "310px")))
    )
  ))
)
