#########################################
#Importing the required libraries required
#########################################



#Shiny libraries
library(shiny) 
library(shinydashboard)
library(shinyjs)
library(shinyalert)

#Excel Read library
library("readxl")

#It creates an HTML widget to display R data objects with DataTables
library(DT)

#User password encryption and decryption library
library(sodium)

#SQLite Database interface library
library(RSQLite)

#Date function libraries
library(lubridate)


#########################################
# Setting the Working Directory
#########################################
setwd("D:\\Shiny_Radius\\")


#########################################
# Setting the Database Connection
#########################################
conn <- dbConnect(RSQLite::SQLite(), "RadiusDB.db")












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
# Max File Size
#########################################

options(shiny.maxRequestSize = 20*1024^2)




#########################################
# UI Function Call
#########################################

ui<-dashboardPage(header, sidebar, body, skin = "green")






#########################################
# File Used in the Sales Summary
#########################################

monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

Sales_Summarized<-read.csv("D:\\Shiny_Radius\\Datasets\\Sales_Summarized.csv")

Sales_Summarized$Date=ymd(Sales_Summarized$Date)





#########################################
# Server for Conditional Computation
#########################################
server <- function(input, output, session) {
  
  #Importing User Table from the database
  credentials  =  dbGetQuery(conn, "SELECT * FROM USER")
  
  #User Authentication
  login = FALSE
  USER <- reactiveValues(login = login)
  
  rv = reactiveValues()
  rv$Sales_Summarized=Sales_Summarized
  
  
  
  
  
  #
  observe({
    
    rv$Sales_Summarized <- Sales_Summarized[Sales_Summarized$Brand == input$Brand_P1,]
    
    updateSliderInput(session,"DateFilter_P1",min = min(rv$Sales_Summarized$Date),
                      max = max(rv$Sales_Summarized$Date),
                      value=c(min(rv$Sales_Summarized$Date),max(rv$Sales_Summarized$Date)),timeFormat="%b %Y")
    
    
    
    
  })
  
  
  output$salesplot <- renderPlotly({
    
    DF3<-rv$Sales_Summarized%>%dplyr::filter(Date>=input$DateFilter_P1[1])%>%dplyr::filter(Date<=input$DateFilter_P1[2])%>%dplyr::arrange(Date)
    DF3$Monthly_Growth<-0
    for(i in seq(2,nrow(DF3))){
      DF3$Monthly_Growth<-((DF3$Monthly_Growth[i]-DF3$Monthly_Growth[i-1])/DF3$Monthly_Growth[i-1])*100
    }
    plot_ly(DF3, x = ~Date, 
            y = ~total, 
            name = 'trace 0', 
            type = 'scatter', 
            mode = 'lines')%>%
      plotly::layout( height = 325)
  })
  
  
  observeEvent(input$button, {
    updateTabItems(session, "tabs", "Marketing_Analysis")
    
  })
  
  
  observe({ 
    if (USER$login == FALSE) {
      
      if (!is.null(input$login)) {
        
        if (input$login > 0) {
          
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          
          if(length(which(credentials$username==Username))==1) { 
            
            pasmatch  <- credentials["password"][which(credentials$username==Username),]
            pasverify <- password_verify(pasmatch, Password)
            
            if(pasverify) {
              
              USER$login <- TRUE
            } 
            else {
              
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } 
          else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  
   })
  
  #Displaying Logged in User to Navbar
  #output$loggedinuser<-renderUI({ 
  #  
  #  req(USER$login)
  #  tags$h2(input$userName,
  #         class = "dropdown", 
  #          style = "background-color: #eee !important; border: 0;
  #          font-weight: bold; margin:5px; padding: 10px;")
  #  })
  
  #Logout Button
  output$logoutbtn <- renderUI({
    
    req(USER$login)
    tags$div(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
            font-weight: bold; margin:5px; padding: 10px;")
  })
  
  #Sidebar Panel dynamic according to the logged in user
  output$sidebarpanel <- renderUI({
    
    if (USER$login == TRUE ){ 
      #If the logged in user is advanced then File upload option is also displayed
      if (credentials[,"user_type"][which(credentials$username==input$userName)]=="advanced") {
        sidebarMenu(id="tabs",
          menuItem("File Upload", tabName = "FileUpload", icon = icon("th")),
          menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard")),
          shinyjs::hidden(menuItem("Market Analysis", tabName = "Marketing_Analysis", icon = icon("dashboard"))),
          menuItem("Brand Filter",selectInput(inputId = "Brand_P1", label = "Select Brand",
                      choices = list("MAVYRET" = "MAVYRET", "HUMIRA" = "HUMIRA",
                                     "LUPRON DEPOT" = "LUPRON DEPOT","CREON"="CREON"), selected = "HUMIRA")),
          menuItem("Date Filter",sliderInput("DateFilter_P1",
                                             label= tags$b(h4("Filter Date")),
                                             width='100%',
                                             step=31,
                                             min = as.Date("2016-01-01","%Y-%m-%d"),
                                             max = as.Date("2016-12-01","%Y-%m-%d"),
                                             value=c(as.Date("2016-01-01"),as.Date("2016-12-01")),
                                             timeFormat="%b %Y"))
          
        )
      }
      else{
        sidebarMenu(id="tabs",
          menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard")),
          shinyjs::hidden(menuItem("Market Analysis", tabName = "Marketing_Analysis", icon = icon("dashboard"))),
          menuItem("Brand Filter",selectInput(inputId = "Brand_P1", label = "Select Brand",
                                              choices = list("MAVYRET" = "MAVYRET", "HUMIRA" = "HUMIRA",
                                                             "LUPRON DEPOT" = "LUPRON DEPOT","CREON"="CREON"), selected = "HUMIRA")),
          menuItem("Date Filter",sliderInput("DateFilter_P1",
                                             label= tags$b(h4("Filter Date")),
                                             width='100%',
                                             step=31,
                                             min = as.Date("2016-01-01","%Y-%m-%d"),
                                             max = as.Date("2016-12-01","%Y-%m-%d"),
                                             value=c(as.Date("2016-01-01"),as.Date("2016-12-01")),
                                             timeFormat="%b %Y"))
        
        )
        
      }
    }
  })
  
  
  #Bodu Panel dynamic according to the logged in user
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      #If the logged in user is advanced then File upload option is also displayed
      if (credentials[,"user_type"][which(credentials$username==input$userName)]=="advanced") {
        
        tabItems(
          tabItem(
            tabName ="FileUpload", class = "active",
            h2("File Upload Page"),FileUploadRow
           
          ),
          tabItem(
            tabName ="dashboard",h2("Main Page"),
            MainPageRow1,MainPageRow2),
          

          
          tabItem(tabName = "Marketing_Analysis",h2("Marketing Analysis Page"),
                  MarketPageRow1,MarketPageRow2,MarketPageRow3
          )
        )
      } 
      else {
        
        tabItems(
          
           tabItem(
             
             tabName ="dashboard", class = "active",h2("Main Page"),
             MainPageRow1,MainPageRow2),
           
           
           
           tabItem(tabName = "Marketing_Analysis",h2("Marketing Analysis Page"),
                   MarketPageRow1,MarketPageRow2,MarketPageRow3
          ))
        
      }
      
    }
    else {
      loginpage
    }
  })
  
  output$results <-  DT::renderDataTable({
    datatable(iris, options = list(autoWidth = TRUE,
                                   searching = FALSE))
  })
  
  output$contents <- DT::renderDT({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    readxl::read_excel(inFile$datapath,
                       sheet = input$sheetno,na = input$nullvalues)
  })
  
  observeEvent(input$filesave, {
    inFile <- input$file1
    if (is.null(inFile)){
       showNotification(h3("Upload File First."),type = "error",duration = 3,closeButton = F)
       return()
    }
    file.copy(inFile$datapath, file.path("D:\\Shiny_Radius\\Datasets", inFile$name) )
    showNotification(h3("Upload Successful."),type = "message",duration = 3,closeButton = F)
    shinyjs::reset("FileUpload")
  })

  
  
}



#########################################
# Run App Function Call
#########################################
runApp(list(ui = ui, server = server), launch.browser = TRUE)


#########################################
# Disconnecting the DB after Use
#########################################
dbDisconnect(conn)

