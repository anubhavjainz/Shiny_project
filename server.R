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
# Max File Size
#########################################

options(shiny.maxRequestSize = 20*1024^2)








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
# Disconnecting the DB after Use
#########################################
dbDisconnect(conn)

