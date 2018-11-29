#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("xlsx")
#install.packages("readr")
#install.packages("lubridate")
#install.packages("maps")
#install.packages("mapdata")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plyr)
library(arules)
library(readr)
library(openxlsx)
library(lubridate)
library(plotrix)
library(maps)
library(mapdata)
library(tidyr)

# Data loading
logs <- read_delim("logs.csv", 
                   ";", escape_double = FALSE, locale = locale(encoding = "ASCII"), 
                   trim_ws = TRUE)
# Special characters
logs$User <- gsub("\u000e" , "\351" ,logs$User)
logs$User <- gsub("\003" , "\311" ,logs$User)
logs$User <- gsub("\017" , "\350" ,logs$User)
logs$User <- gsub("\021" , "\353" ,logs$User)

surveydataece <- read.xlsx("surveydataece.xlsx","surveydata")
name_of_users1 <- c(surveydataece$Name)
name_of_users2 <- c(unique(logs$User))

# data to select
name_of_users <- unique(c(name_of_users1 ,name_of_users2))
week_possibilities <- c(22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44)

# Header
header <- dashboardHeader(title = "iBriquet")

# Side bar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Single user", tabName = "singuser", icon = icon("dashboard")),
    menuItem("All users", tabName = "allusers", icon = icon("dashboard"))
  )
)

#FIRST PAGE
frow5 <- column(12,fluidRow(
  selectizeInput("user_name","User Name", choice = name_of_users),
  valueBoxOutput("naaame"),
  offset = 1
))

frow6 <- column(10,mainPanel(
  
  tabsetPanel( 
    tabPanel("Info about mode",verbatimTextOutput("basic_infos"),
             verbatimTextOutput("general_infos"),style='width: 900px'), # Tab panel for the information about the modes
    tabPanel("Health condition", 
             plotOutput("pieChart"),
             fluidRow(
               valueBoxOutput("weigth"),
               valueBoxOutput("heigth"),
               valueBoxOutput("years_of_smoking"))), # Tab panel for the information about the health conditions
    tabPanel("cigarette consumption", 
             selectizeInput("week_choice","Week Choice", choice = week_possibilities),
             plotOutput("cigperweek"),
             verbatimTextOutput("possible_weeks"),
             radioButtons("selected_mode2", "Mode", inline = TRUE,list("Auto skipped" = "auto_skipped",
                                                               "Behaviour" = "behaviour",
                                                               "Cheated" = "cheated",
                                                               "Friend" = "friend",
                                                               "On time" = "on_time",
                                                               "Skipped" = "skipped",
                                                               "Snoozed" = "snoozed"))
    ), # Tab panel for the lighter use
    tabPanel("Coordinates", plotOutput("map"), verbatimTextOutput("cig")) # Tab panel for the location of the user
  )),
  offset = 2
)

row_one_02 <- fluidRow(
  valueBoxOutput("gender"),
  valueBoxOutput("age"),
  valueBoxOutput("familystatus")
)

tabitem1 <- tabItem(tabName = "singuser", h2("single user graphs"), row_one_02, frow5, frow6)

#SECOND PAGE
frow1 <- fluidRow(
  valueBoxOutput("completesurveys"),
  valueBoxOutput("progexpecmean"),
  valueBoxOutput("agemean")
)

frow2 <- fluidRow(
  column(12, align = "center", box(
    title = "Frequency of modes"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("freqOfMode", height = "300px")),
    offset = 3
  )
)

frow3 <- fluidRow(
  column(12,radioButtons("selected_mode", "Mode", inline = TRUE,
                         list("Auto skipped" = "auto_skipped",
                              "Behaviour" = "behaviour",
                              "Cheated" = "cheated",
                              "Friend" = "friend",
                              "On time" = "on_time",
                              "Skipped" = "skipped",
                              "Snoozed" = "snoozed")),
         offset = 4,
         br()
  ))

frow4 <- fluidRow(
  column(7,valueBoxOutput("Exfreqofmode"),
         offset = 5)
)

tabitem2 <- tabItem(tabName = "allusers", h2("all users graphs"), frow1, frow2, frow3, frow4)

body <- dashboardBody(tabItems(tabitem1,tabitem2))

ui <- dashboardPage(header, sidebar, body, skin='red')

server <- function(input,output){
  #some data manipulation to derive the values of KPI boxes
  #Single user
  #Split data into user for logs file
  datatreated_user <- reactive({
    df <- logs
    data_treat1 <- subset(df,User==input$user_name)
  })
  
  #Split data into user for survey file
  datatreated_user_survey <- reactive({
    df <- surveydataece
    data_treat2 <- subset(df,Name==input$user_name)
  })
  
  #Data treatment to display the available week of a user
  datatreated_user_available_week <- reactive({
    df <- logs
    data_treat <- subset(df,User==input$user_name)
    data_treat$Time <- as.Date(data_treat$Time, format = "%d/%m/%Y")
    data_treat$weekDay <- format(data_treat$Time,"%A")
    data_treat$weeknb <- week(data_treat$Time)
    data_treat <- data_treat[,c(2,6,7)]
  })
  
  #Data treatment for the selection of the mode
  data2 <- reactive({  
    selected_mode2 <- switch(input$selected_mode2,
                     auto_skipped = "Auto skipped",
                     behaviour = "Behaviour",
                     cheated = "Cheated",
                     friend = "Friend",
                     on_time = "On time",
                     skipped = "Skipped",
                     snoozed = "Snoozed")
  })
  
  #Data treatment to plot the use of the lighter function of the day on a graph
  datatreated_user_date <- reactive({
    selected_mode2 <- data2()
    df <- logs
    data_treat <- subset(df,User==input$user_name)
    data_treat$Time <- as.Date(data_treat$Time, format = "%d/%m/%Y")
    data_treat$weekDay <- format(data_treat$Time,"%A")
    data_treat$weeknb <- as.numeric(week(data_treat$Time))
    data_treat <- data_treat[,c(2,6,7)]
    data_treat <- subset(data_treat, weeknb == input$week_choice)
    dfday <- data.frame(day = c("null"), nbcig=c(-1))
    if(nrow(data_treat)!=0){
      data_treat <- data_treat[,c(1,2)]
      data_treat$Type <- as.character(data_treat$Type)
      data_treat <- subset(data_treat, data_treat$Type == selected_mode2)
      if(nrow(data_treat)!=0){
        data_treat <- data_treat[,c(2)]
        data_treat <- data.frame(table(data_treat$weekDay))
        
        dfday <- data.frame(day = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"), nbcig=c(0,0,0,0,0,0,0))
        
        for(i in 1:7){
          for(j in 1:nrow(data_treat)){
            if(dfday$day[i] == data_treat$Var1[j]){
              dfday$nbcig[i] <- data_treat$Freq[j]
            }
          }
        }
        
        dfday$order <- c(1,2,3,4,5,6,7)
        dfday$day<-factor(dfday$day,levels = dfday$day[order(dfday$order)])
      }
    }
    dfday
  })
  
  #Data treatment to display the diseases and information about the health of the chosen user
  datatreated_user_health_condition <- reactive({
    df <- surveydataece[,c(5,12,13,14,15,16,17,18,19)]
    data_treat <- subset(df,Name==input$user_name)
    data_treat_by_health <- data.frame(health.condition = c("No information"), value=c(1))
    if(nrow(data_treat)!=0){
      data_treat <- data_treat[,2:9]
      data_treat_by_health <- data.frame(health.condition = c("None","diabetes","Gastrointestinal Reflux","Heart Problems","high blood pressure","overweight","psychological or psychiatric conditions","other"),value = c(0,0,0,0,0,0,0,0))
      for(i in 1:length(data_treat)){
        if(!(is.na(data_treat[1,i]))){
          data_treat_by_health[i,2] <- 1
        }
      }
      data_treat_by_health <- subset(data_treat_by_health, value == 1)
    }
    data_treat_by_health
  })
  
  #Data treatment to display a map of the locations of the lighter uses
  datatreated_user_coordinates <- reactive({
    df <- logs
    data_treat <- subset(df,User==input$user_name)
    data_treat <- data_treat[,4:5]
    data_treat$loc <- paste(data_treat$Latitude,data_treat$Longitude)
    data_treat <- table(data_treat$loc)
    data_treat <- data.frame(data_treat)
    data_treat <- separate(data_treat, names(data_treat)[1], c("Latitude","Longitude"), sep=" ")
    data_treat <- data_treat[order(data_treat$Freq, decreasing = TRUE),]
    data_treat <- data_treat[1:7,]
    data_treat <- subset(data_treat,!is.na(Latitude))
    data_treat
  })
  
  #Information about the location
  output$cig <- renderPrint({
    if(nrow(datatreated_user_coordinates())!=0){
      datatreated_user_coordinates()
    }else{
      "no information about lighter's places of use"
    }
  })
  
  #Display the location
  output$map <- renderPlot({
    map('worldHires')
    for(i in 1:nrow(datatreated_user_coordinates())){
      points(datatreated_user_coordinates()[i,1],datatreated_user_coordinates()[i,2], pch=16, col = 'red')
    }
  })
  
  #Graph of the number of use by week
  output$cigperweek <- renderPlot({
    plot = qplot(datatreated_user_date()$day,datatreated_user_date()$nbcig, geom="line", group=1)
    plot = plot + labs(x="weekday") + labs(y="number of uses")
    print(plot)
  })
  
  #Piechart of health condition
  output$pieChart <- renderPlot({
    pie(datatreated_user_health_condition()[,2],labels=datatreated_user_health_condition()[,1],col=rainbow(length(datatreated_user_health_condition()[,1])),explode=0.1,
        main="Pie Chart of health problems")
  })
  
  #Displays the weight of the user
  output$weigth <- renderValueBox({
    valueBox(
      formatC(datatreated_user_survey()[1,10], format="d", big.mark=',')
      ,paste('weigth in kg')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "yellow")
  })
  
  #Displays the height of the user
  output$heigth <- renderValueBox({
    valueBox(
      formatC(datatreated_user_survey()[1,11], format="d", big.mark=',')
      ,paste('heigth in cm')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "yellow")
  })
  
  #Displays the number of years of smoking of the user
  output$years_of_smoking <- renderValueBox({
    valueBox(
      formatC(datatreated_user_survey()[1,7]-datatreated_user_survey()[1,20], format="d", big.mark=',')
      ,paste('smoking years')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "yellow")
  })
  
  #Available weeks for the user
  output$possible_weeks <- renderPrint({
    info = unique(datatreated_user_available_week()$weeknb)
    cat("Possible weeks for this user : ", info)
  })
  
  #Total number of use for the user
  output$general_infos <- renderPrint({
    
    info=datatreated_user()
    total=nrow(info)
    cat("Total number of uses for all modes : ", total)
    
  })
  
  #Deatils of number of uses of each mode
  output$basic_infos <- renderPrint({
    
    info=datatreated_user()
    
    stats.mode <- function(x) {cbind(freq = table(x), percentage = prop.table(table(x))*100)}
    stats.mode(info$Type)
    
  }) 
  
  #Displays the gender of the user
  output$gender <- renderValueBox({
    valueBox(
      formatC(datatreated_user_survey()$Gender, format="d", big.mark=',')
      ,paste('Gender')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  
  #Display the family status of the user
  output$familystatus <- renderValueBox({
    valueBox(
      formatC(datatreated_user_survey()$Family.status, format="d", big.mark=',')
      ,paste('Family status')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "red")  
  })
  
  #Display the age of the user
  output$age <- renderValueBox({
    valueBox(
      formatC(datatreated_user_survey()$Age, format="d", big.mark=',')
      ,paste('Age')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")  
  })
  
  #All users
  #Age mean
  age <- (sum(surveydataece[,7])/nrow(surveydataece))
  #nb of completed surveys
  completesurveys = subset(surveydataece, All.surveys.completed == "yes")
  nbyes <- nrow(completesurveys)
  #progress expectations mean
  mean <- (sum(surveydataece[,3])/nrow(surveydataece))*100
  #Frequency of type
  freq <- data.frame(table(logs[,2]))
  
  #creating the valueBoxOutput content
  output$completesurveys <- renderValueBox({
    valueBox(
      formatC(nbyes, format="d", big.mark=',')
      ,paste('nb of completed surveys')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  
  #Displays the progress expectation means for all users
  output$progexpecmean <- renderValueBox({
    valueBox(
      formatC(mean, format="d", big.mark=',')
      ,paste('progress expectation mean %')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")  
  })
  
  #Displays the age mean of all the users
  output$agemean <- renderValueBox({
    valueBox(
      formatC(age, format="d", big.mark=',')
      ,paste('age average of all the users')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "red")  
  })
  
  #Histogram of all the modes
  output$freqOfMode <- renderPlot({
    ggplot(data = freq, aes(x=Var1, y=Freq, fill=factor(Var1))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Frequency of mode") + 
      xlab("Mode") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Frequency of all the modes") + labs(fill = "Mode")
  })
  
  data <- reactive({  
    selected_mode <- switch(input$selected_mode,
                    auto_skipped <- freq[1,2],
                    behaviour = freq[2,2],
                    cheated = freq[3,2],
                    friend = freq[4,2],
                    on_time = freq[5,2],
                    skipped = freq[6,2],
                    snoozed = freq[7,2])
  })
  
  #Display the number of uses of the mode for the selected one
  output$Exfreqofmode <- renderValueBox({
    selected_mode <- input$selected_mode
    
    valueBox(data(),
             formatC(selected_mode, format="d", big.mark=',')
             ,paste('frequency of the chosen mode')
             ,icon = icon("stats",lib='glyphicon')
             ,color = "purple")  
  })
}

shinyApp(ui=ui,server=server)

