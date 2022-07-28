
library(shinydashboard)
library(shiny)

# create sidebar 
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "about"),
    menuItem("Data Exploration", tabName = "explore"),
    menuItem("Modeling", tabName = "modeling"),
    menuItem("Data", tabName = "data")
  )
)

# create body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "about",
            tabsetPanel(
              tabPanel("Purpose",
                       h3("The purpose of the app is to explore breastfeeding data, which allow users to: "),
                       h4(tags$ul(
                         tags$li("Do data exploration"), 
                         tags$li("Fit three supervised learning models"), 
                         tags$li("Use one of the models for prediction")))
              ),
              tabPanel("Introduction"),
              tabPanel("Data"),
              tabPanel("Image",
                       img(src = "picture.jpeg", width = "80%"))
              
              ))))

dashboardPage(
  dashboardHeader(title = "Breastfeeding Data Analysis",
                  titleWidth = 360),
  sidebar,
  body
)
