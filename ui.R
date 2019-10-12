library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)




#########################################
# Application Navbar 
#########################################
header <- dashboardHeader( title = "Radius 2.0",
                           #uiOutput("loggedinuser") ,
                           uiOutput("logoutbtn"))



#########################################
# Application Sidebar 
#########################################
sidebar <- dashboardSidebar(uiOutput("sidebarpanel"),collapsed = T) 



#########################################
# Application Body 
#########################################
body <- dashboardBody(tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")),
                      shinyjs::useShinyjs(), uiOutput("body"))



#########################################
# UI Function Call
#########################################

ui<-dashboardPage(header, sidebar, body, skin = "green")
