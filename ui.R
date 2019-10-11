#########################################
#Importing the required libraries required
#########################################



#Shiny libraries
library(shiny) 
library(shinydashboard)
library(shinyjs)
library(shinyalert)
library(plotly)




#########################################
# Main login screen 
#########################################
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 img(src='D:\\Shiny_Radius\\www\\background2.PNG',style="
                     width: 70%;
                     display: block;
                     margin-left: auto;
                     margin-right: auto;"),
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                  padding: 10px 15px; width: 150px; cursor: pointer;
                                  font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                  padding-top: 5px;font-size:16px;", 
                                  class = "text-center")))
                     ))
                     )


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
body <- dashboardBody(tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "D:\\Shiny_Radius\\www\\bootstrap.css")),       tags$head(  tags$style(
  HTML(".shiny-notification {
       position:fixed;
       top: calc(10%);
       left: calc(50%);
       }
       "
  )
  )
  ),shinyjs::useShinyjs(), uiOutput("body"))



#########################################
# File Upload Page Body
#########################################
FileUploadRow <- fluidRow( 
  box(
    selectInput(inputId = 'filetype',label = 'Select File Type',choices = c("CALL ACTIVITY","EMAIL","PROMO DISPLAY","PROMO SEARCH","SPEND METRICS","SALES"),selected = "SALES"),
    fileInput('file1', h4('Choose xlsx file to upload'),
              accept = c(".xlsx")
    ),
    tags$hr(),
    sliderInput(inputId='sheetno', label="Sheet Number", min=1, max=10, value=1, step =1),
    textInput(inputId = 'nullvalues',label='NA Values',value = 'NA'),
    tags$hr(),
    actionButton("filesave", "Save File", style = "color: white; background-color:#3c8dbc;
                 padding: 10px 15px; width: 150px; cursor: pointer;
                 font-size: 18px; font-weight: 600;"),
    width = 4,height = 800,status = "info"
    )
  ,box(
    h4('Table Output'),
    DT::dataTableOutput('contents'),
    width = 8,height = 800,status = "info"
  )
  )





#########################################
# Main Page Body
#########################################
MainPageRow1 <- fluidRow( 
  box(
    h3("Sales"),
    plotlyOutput("salesplot"),
    width = 4,height = 400,status = "info"
  )
  ,box(
    h3("Promotion"),actionButton("button", "GET DETAILED MARKETING ANALYSIS"),
    width = 8,height = 400,background = 'blue'
  )
)

MainPageRow2 <- fluidRow( 
  box(
    h3("ROI Analysis"),
    width = 3,height = 400,background = 'purple'
  )
  ,box(
    h3("Marketing Mix"),
    width = 9,height = 400,background = 'orange'
  )
  
  
)





#########################################
# Marketing Analysis Body
#########################################
MarketPageRow1 <- fluidRow( 
  box(
    h3("Geography Wise Marketing Activity"),
    width = 4,height = 300,background = 'teal',textOutput("SliderText")
  )
  ,box(
    h3("Channel wise Marketing Activity"),
    width = 4,height = 300,background = 'red'
  )  ,box(
    h3("Franchise Marketing Activity"),
    width = 4,height = 300,background = 'lime'
  )
)

MarketPageRow2 <- fluidRow( 
  box(
    h3("TRADITIONAL MEDIA MARKETING"),
    h3("DIGITAL MEDIA"),
    width = 12,height = 50,background = 'yellow'
  )
  
  
  
)


MarketPageRow3 <- fluidRow( 
  box(
    h3("Region wise Call Activity"),
    width = 3,height = 350,background = 'green'
  )
  ,box(
    h3("Ads showtime comparison on tv"),
    width = 3,height = 350,background = 'red'
  )  ,box(
    h3("salesforce incentives"),
    width = 3,height = 350,background = 'fuchsia'
  )
  ,box(
    h3("target achieved %"),
    width = 3,height = 350,background = 'black'
  )
  
)



#########################################
# UI Function Call
#########################################

ui<-dashboardPage(header, sidebar, body, skin = "green")


