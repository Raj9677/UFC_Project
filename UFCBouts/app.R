library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(caret)
library(tibble)
library(tidyr)
library(data.table)
library(rsconnect)
library(markdown)
library(DT)
library(RMariaDB)
library(sqldf)
library(date)

source('fights.R')

source('mstat.R')

source('mpred.R')

source('model.R')

source('ExpectedValueFunctions.R')

ui <- fluidPage(
  
  
  tags$style(HTML("
      #r1 {
          border: 2px double white;
          background: #FFA2A2;
      }
      #b1 {
          border: 2px double white;
          background: #A2D0FF;
          
      }
      #r2 {
          border: 2px double white;
          background: #FFA2A2;
          
      }
      #b2 {
          border: 2px double white;
          background: #A2D0FF;
        
          
      }
      
      #g1 {
          border: 2px double white;
          background: #F9F9F9;
        
          
      }
      
      #g2 {
          border: 2px double white;
          background: #EEEEEE;
        
          
      }
    ")),
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  navbarPage(
    "Ultimate UFC",
    
    tabPanel("Home Page", 
             h2("Welcome to Ultimate UFC App"),
             tags$br(),
             tags$br(),
             
             tags$b("The Ultimate Fighting Championship (UFC) is an American mixed martial arts (MMA) promotion company based in Las Vegas, Nevada.
               It is the largest MMA promotion company in the world and features some of the highest-level fighters in the sport on its roster."),
             tags$br(),
             tags$br(),
             tags$b("This shiny app will help you explore many features on the UFC dataset and provide meaningful insights along with some descriptive information for UFC."),
             tags$br(),
             tags$br(),
             tags$b("Navigate through the navigation bar at the top to discover different features and stats the app provides. "),
             tags$br(),
             tags$br(),
             tags$br(),
             tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/5khE_hgWY_o", allowfullscreen=NA)
    ),
    
    tabPanel("Fighter Match up",
             fluidRow(titlePanel('Fighter Match Up') , align='center',
                      column(4,
                             
                             wellPanel('Expected Result',
                                       uiOutput('resultR')
                             )
                             
                      ),       
                      column(4, align='center',selectInput('gender', 'Select Gender Class', choices = c('select' = '', levels(mstat$gender))),
                             selectInput('class', 'Select Weight Class', choices = c('select' = '', levels(mstat$weight_class)))),
                      column(4,
                             wellPanel('Expected Result',
                                       uiOutput('resultB')
                             )
                      ),
                      fluidRow(column(6,titlePanel('Red Corner'),id='r1',selectInput('name', 'Select Fighter', choices = c('select' = '', levels(mstat$name)))),
                               column(6,titlePanel('Blue Corner'),id='b1',selectInput('name2', 'Select Fighter', choices = c('select' = '', levels(mstat$name))))),
                      
                      fluidRow(column(6,'', numericInput('red','Enter Betting Odds',0,
                                                         -999, 999, 1),id='r2'),
                               column(6,'', numericInput('blue','Enter Betting Odds',0,
                                                         -999, 999, 1),id='b2')),
                      
                      fluidRow(column(6,'Characteristics', tableOutput('table7'),id='g1'),
                               column(6,'Characteristics', tableOutput('table8'),id='g1')),
                      
                      fluidRow(column(6,'Fighter Record', tableOutput('table3'),id='g2'),
                               column(6,'Fighter Record', tableOutput('table4'),id='g2')),
                      
                      fluidRow(column(6,'Win Details', tableOutput('table5'),id='g1'),
                               column(6,'Win Details', tableOutput('table6'),id='g1')),
                      
                      fluidRow(column(6,'Fight History', tableOutput('table'),id='g2'),
                               column(6,'Fight History', tableOutput('table2'),id='g2'))
                      )
             
    ),
    
    tabPanel("Stats",
             fluidRow(titlePanel('UFC Statistics') , align='center',
                      column(4),       
                      column(4, align='center',selectInput('gender2', 'Select Gender Class', choices = c('select' = '', levels(mstat$gender))),
                             selectInput('class2', 'Select Weight Class', choices = c('select' = '', levels(mstat$weight_class))),
                             dateRangeInput('daterange','Select date range:', start = as.Date(min(fights$date)), end = as.Date(max(fights$date)), min = min(fights$date),
                                            max = max(fights$date), format = "mm/dd/yyyy", language = "en", separator = " to ", width = NULL)),
                      column(4)),
             fluidRow(column(4,align='center','Top 10 Wins', tableOutput('winstab')),
                      column(4,align='center','Top 10 Kos', tableOutput('kostab')),
                      column(4,align='center','Win Distribution', tableOutput('wintype'))
                      )
             
    ),
    
    tabPanel("Upcoming matches",  
             h2("Upcoming matches"), 
             tags$b(DT::dataTableOutput("upcoming_events")
             )
    ),
    
    tabPanel("Analysis",
             h2("Analysis"),
             tags$br(),
             tags$br(),
             tags$b("The project started with us receiving kaggle dataset for UFC past matches for Red and Blue Fighters along with various characteristics of the fighters and the matches."),
             tags$b("The analysis is done based on three sections: 1] Factors important for deciding winnner, 2] Developing a model that predicts the winner. 3]  develop a model to identify how to invest/bet on certain fights"),
             tags$br(),
             tags$b("The first step in the analysis was data cleaning and shaping. First, we decided to remove the columns that would not make any sense for predicting the winner based on that column. Columns like Date, Location, Fighter Name, Gender, finish round, etc would not impact if red won or blue. As a result, these columns were further filtered out from the analysis."),
             tags$b("Secondly, the data provided had a lot of missing points and was not in the correct shape. As a result, we made sure that the columns having the majority of data not available would not be a part of our analysis and prediction. "),
             tags$br(),
             tags$br(),
             tags$br(),
             tags$pre(tags$code("columns_not_related = c(\"index\",\"R_fighter\",\"B_fighter\",\"date\",\"location\",\"country\",\"constant_1\",\"finish\",\"finish_details\",\"finish_round\",\"finish_round_time\",\"total_fight_time_secs\",
                        \"gender\",\"no_of_rounds\",\"title_bout\",\"empty_arena\")"),
                      tags$br(),
                      tags$code("#new_m = m[, !(colnames(m) %in% columns_not_related)]"),
                      tags$br(),
                      tags$code("source(\"DataQualityReport.R\")"),
                      tags$br(),
                      tags$code("# First remove columns that have less than 60% data available"),
                      tags$br(),
                      tags$code("# incomplete <- DataQualityReport(new_m)[,c(1,4)]"),
                      tags$br(),
                      tags$code("#new_m <- new_m[,which(incomplete$PercentComplete >60)]")),
             tags$br(),
             tags$br(),
             tags$b("As mentioned above firstly, we have removed all columns that have less than 60% data available. A lot of columns with Ranks where subsetted out of the data on account of this. The rationale behind this was it will not be possible to do analysis based on partial data available."),
             tags$br(),
             tags$b("Once data preparation is done, We first treated this data to be either numeric or factor based on the column types. We want to start dummyfication of the dataset to do analysis and as a result, it would be important to complete this transformation."),
             tags$br(),
             tags$br(),
             tags$b("After this, we found correlation to find important factors between winner and other characteristics."),
             tags$pre(
               tags$code("for (i in 1:ncol(new_m)) 
                                  {
                                  if (is.numeric(new_m[,i])==TRUE) {new_m[,i]<-as.numeric(new_m[,i])}
                                  else if (is.character(new_m[,i])==TRUE & colnames(new_m[i])=='date') {new_m[,i]<-as.Date(new_m[,i],\"%m/%d/%Y\")} 
                                  else if (is.character(new_m[,i])==TRUE) {new_m[,i]<-as.factor(new_m[,i])}
                                  }"),
               tags$br(),
               tags$code("# Now first do dummification and then create correlation matrix to find important factors"),
               tags$code("dummies1 = dummyVars( ~ ., data = new_m)"),
               tags$code("ex1 = data.frame(predict(dummies1,newdata = new_m))"),
               tags$code("descrCor <-  cor(ex1$Winner.Blue, ex1,use = \"pairwise.complete.obs\")"),),
             tags$br(),
             tags$b("Using cutoff of 0.1 we narrowed down to certain features having decent correlation to winner.The list includes : - "),
             tags$br(),
             tags$b("R_sig_str_pct_bout,B_odds,R_kd_bout,R_pass_bout,R_tot_str_landed_bout,
                    R_td_pct_bout,R_td_landed_bout,R_sig_str_landed_bout,R_sub_attempts_bout,R_tot_str_attempted_bout,
                    R_sig_str_attempted_bout,B_kd_bout,R_odds,B_pass_bout,B_sig_str_pct_bout,B_tot_str_landed_bout,B_sig_str_landed_bout,
                    B_td_landed_bout,B_td_pct_bout,B_tot_str_attempted_bout,B_sub_attempts_bout,
                    age_dif,B_sig_str_attempted_bout,B_td_attempted_bout,win_streak_dif "),
             tags$br(),
             tags$b("The relation could be confirmed using the graphs:- "),
             img(src = 'Characteristic_Comparision.PNG'),
             tags$br(),
             tags$br(),
             tags$br(),
             tags$b("After the confirmation of the important factors using correlation matrix and graphs, we ran the model with a logistic regression model to classify the output between winner and loser."),
             tags$b("After finding the output, we adjusted the model with significance test of 0.05, i.e. removed certain input columns having high p-value beyond our requirements. Finally, we ran the model to find that the model had all significant regressors. Eventually,we found out the Confusion matrix calculated the overall model accuracy of 88%."),
             tags$b("The AUC/ROC plot for the model can be seen below:-"),
             tags$br(),
             tags$br(),
             img(src = 'ROC_AUC.PNG'),
             tags$b("This model is eventually used in the tab Fighter Match up to predict winner."),
    ),
    tabPanel("Betting Optimizer",  
             h3("Maxmimize Profits with our Expected Value Calculator"),
             tags$br(),
             tags$br(),
             tags$b("Although it would be impossible to make sure that you turn profit 100% times using betting as a tool in a probabilistic event, we could use the principal of Expected value to have a stronger chance of making money over time. "),
             tags$b("The Expected Value principal mentions that the higher the number of simulations for an event, the closer the value would get to the Expected value. Therefore, over time we would expect to make money."),
             tags$b("Here we combine the Odds presented to us and the probabilities of a win for each fighter on a fight to see if we could get an Expected value above 0. Combining multiple expected value above 0 would eventually help us make money over time"),
             tags$br(),
             tags$br(),
             tags$b("Select Fields and Edit Columns as per your demands: Click to toggle between selection and deselection of rows. Double click a cell to edit its value. The button Calculate EV helps you calculate the expected value for selection."),
             tags$b("Select rows that you want to calculate betting on, make sure the values are present and valid. Character strings in Odds and probability columns will be coerced to NA."),
             
             tags$b(DT::dataTableOutput("Joined_Data")),
             tags$br(),
             actionButton(inputId = "Go",  label = "Calculate EV"),
             tags$br(),
             tags$br(),
             tags$b("Selected Rows:-"),
             verbatimTextOutput('Joined_Data_select'),
             verbatimTextOutput('selected_var'),
             tags$br(),
    ),
    
    position = c("static-top", "fixed-top", "fixed-bottom"),
    header = NULL,
    footer = NULL,
    inverse = TRUE,
    collapsible = TRUE,
    fluid = TRUE
    
    
  ))


server <- function(input, output, session) {
  
  
  
  observe({
    updateSelectInput(session, 'class', choices = mstat[mstat$gender==input$gender, 'weight_class'])
  })
  
  observe({
    updateSelectInput(session, 'class2', choices = fights[fights$gender==input$gender2, 'weight_class'])
  })
  
  observe({
    updateSelectInput(session, 'name', 
                      choices = mstat[mstat$gender==input$gender & mstat$weight_class==input$class, 'name'])
  })
  
  observe({
    updateSelectInput(session, 'name2', 
                      choices = mstat[mstat$gender==input$gender & mstat$weight_class==input$class & mstat$name!=input$name, 'name'])
  })
  
  
  
  
  tab <- reactive({ 
    
    fights %>% 
      filter(gender == input$gender) %>% 
      filter(R_fighter == input$name | B_fighter ==input$name) %>% subset(select=c(R_fighter,B_fighter,Winner,date)) %>%
      mutate(., date=as.character(date))
  })
  
  
  
  
  tab2 <- reactive({ 
    
    fights %>% 
      filter(gender == input$gender) %>% 
      filter(R_fighter == input$name2 | B_fighter ==input$name2) %>% subset(select=c(R_fighter,B_fighter,Winner,date)) %>%
      mutate(., date=as.character(date))
  })
  
  wins <- reactive({ 
    
    a <- fights %>% filter(gender == input$gender2) %>% 
      filter(weight_class == input$class2) %>% 
      filter(date >= input$daterange[1] & date <= input$daterange[2]) 
    
    a <- data.frame(table(a$winner_name))
    
    a[
      with(a, order(-Freq)),
    ] %>%
      
      `colnames<-` (c('Fighter','Wins')) %>%
      
      head(10)
    
  
  })
  
  
  kos <- reactive({ 
    
    a <- fights %>% filter(gender == input$gender2) %>% 
      filter(weight_class == input$class2) %>% 
      filter(finish =='KO/TKO') %>%
      filter(date >= input$daterange[1] & date <= input$daterange[2]) 
    
    a <- data.frame(table(a$winner_name))
    
    a[
      with(a, order(-Freq)),
    ] %>%
      
      `colnames<-` (c('Fighter','KO/TKOs')) %>%
      
      head(10)
    
    
  })
  
  
  fightstats <- reactive({ 
    
    a <- fights %>% filter(gender == input$gender2) %>% 
      filter(weight_class == input$class2) %>% 
      filter(date >= input$daterange[1] & date <= input$daterange[2]) 
    
    a <- data.frame(table(a$finish))
    
    a[
      with(a, order(Var1)),
    ] %>%
      
      `colnames<-` (c('Finish','Count')) %>%
      
      head(10)

      })
  
  
  stat <- reactive({ 
    
    
    mstat %>%
      subset(.,name==input$name)
    
  })
  
  
  stat2 <- reactive({ 
    
    
    
    mstat %>%
      subset(.,name==input$name2)
    
  })
  
  
  stat_ph <- reactive({ 
    
    stat() %>%
      
      subset(.,select=c('Stance','Height_cms','Reach_cms','Weight_lbs','Age'))
    
  })
  
  
  stat2_ph <- reactive({ 
    
    stat2() %>%
      
      subset(.,select=c('Stance','Height_cms','Reach_cms','Weight_lbs','Age'))
    
  })
  
  
  
  stat_record <- reactive({ 
    
    stat() %>%
      
      subset(.,select=c('wins','losses','draw'))
    
  })
  
  
  stat2_record <- reactive({ 
    
    stat2() %>%
      
      subset(.,select=c('wins','losses','draw'))
    
  })
  
  stat_wins <- reactive({ 
    
    stat() %>%
      
      subset(.,select=c('KOTKO','TKO_Doctor','Majority','Split', 'Unanimous', 'Submission'))
    
  })
  
  
  stat2_wins <- reactive({ 
    
    stat2() %>%
      
      subset(.,select=c('KOTKO','TKO_Doctor','Majority','Split', 'Unanimous', 'Submission'))
    
  })
  
  
  expected <- reactive({ 
    
    R <- mpred %>%
      
      subset(name==input$name) %>%
      
      `colnames<-` (c('R_fighter','R_odds','R_tot_str_landed_bout','R_td_landed_bout','R_pass_bout',
                      'R_kd_bout','R_sub_attempts_bout','R_sig_str_landed_bout','R_sig_str_pct_bout',
                      'R_tot_str_attempted_bout','R_td_attempted_bout','R_td_pct_bout','date'))
    B <-  mpred %>%
      
      subset(name==input$name2) %>%
      
      `colnames<-` (c('B_fighter','B_odds','B_tot_str_landed_bout','B_td_landed_bout','B_pass_bout',
                      'B_kd_bout','B_sub_attempts_bout','B_sig_str_landed_bout','B_sig_str_pct_bout',
                      'B_tot_str_attempted_bout','B_td_attempted_bout','B_td_pct_bout','date'))
    
    input <- data.frame(R$R_sig_str_pct_bout,input$blue,R$R_kd_bout,R$R_pass_bout,R$R_tot_str_landed_bout,
                        R$R_td_landed_bout,R$R_sig_str_landed_bout,R$R_sub_attempts_bout,
                        B$B_kd_bout,B$B_pass_bout,B$B_sig_str_pct_bout,B$B_sig_str_landed_bout,
                        B$B_td_pct_bout,B$B_tot_str_attempted_bout,B$B_sub_attempts_bout,
                        B$B_td_attempted_bout) %>%
      
      `colnames<-` (c('R_sig_str_pct_bout','B_odds','R_kd_bout','R_pass_bout','R_tot_str_landed_bout',
                      'R_td_landed_bout','R_sig_str_landed_bout','R_sub_attempts_bout',
                      'B_kd_bout','B_pass_bout','B_sig_str_pct_bout','B_sig_str_landed_bout',
                      'B_td_pct_bout','B_tot_str_attempted_bout','B_sub_attempts_bout',
                      'B_td_attempted_bout'))
    
    win <- predict(logit, newdata=input,type="response")
    
    win <- data.frame(win) %>%
      
      subset(.,select='win')
    
    win <- cbind(ifelse(win>=0.50,1,0),win) 
    
    
    
  })
  
  
  
  
  
  
  
  output$resultR <- renderUI({
    
    d <- expected()
    if (d[,1] ==1) 
      HTML("Winner") else HTML("Loser")
  })
  
  output$resultB <- renderUI({
    d <- expected()
    if (d[,1] ==0) 
      HTML("Winner") else HTML("Loser")
  })
  
  
  
  output$table <- renderTable({ 
    
    tab()
    
  })
  
  output$table2 <- renderTable({ 
    
    tab2()
    
  })
  
  output$table3 <- renderTable({ 
    
    stat_record()
    
  })
  
  output$table4 <- renderTable({ 
    
    stat2_record()
    
  })
  
  
  output$table5 <- renderTable({ 
    
    stat_wins()
    
  })
  
  output$table6 <- renderTable({ 
    
    stat2_wins()
    
  })
  
  
  output$table7 <- renderTable({ 
    
    stat_ph()
    
  })
  
  output$table8 <- renderTable({ 
    
    stat2_ph()
    
  })
  
  output$winstab <- renderTable({ 
    
    wins()
    
  })
  
  output$kostab <- renderTable({ 
    
    kos()
    
  })
  
  
  output$wintype <- renderTable({ 
    
    
    fightstats()
    
    
  })
  
  output$upcoming_events <- DT::renderDataTable({
    DT::datatable(display_uc,options = list(lengthMenu = c(20, 50,100), pageLength = 20))  %>% formatStyle(names(display_uc[,1:7]),backgroundColor = "#FFA2A2")  %>% formatStyle(names(display_uc[,11:17]),backgroundColor = "#A2D0FF")
  })
  
  
  
  output$Joined_Data <- DT::renderDataTable({
    DT::datatable(Joined_Data, editable = "cell",selection="multiple",options = list(lengthMenu = c(20, 50,100), pageLength = 20) )
  })
  
  Betting_Data <- reactiveVal(Joined_Data)
  
  text1 <- reactiveVal("")
  
  observeEvent(input[["Joined_Data_cell_edit"]], {
    cell <- input[["Joined_Data_cell_edit"]]
    newdf <- Betting_Data()
    if(cell$col>2){
      cell$value = as.numeric(cell$value)
    }
    
    newdf[cell$row, cell$col] <- cell$value
    Betting_Data(newdf)
  })
  
  observeEvent(input$Go, {
    DataSelected = Betting_Data()[input$Joined_Data_rows_selected,]
    
    Fault = FALSE
    
    if(any(is.na(DataSelected))){
      Fault = TRUE
      text1("Please remove NA Values or inappropriate text fields from the Rows for calculation")
      
    }
    
    
    if(is.numeric(DataSelected[,3]) == FALSE){
      Fault = TRUE
      text1("Please make sure R_odds are numeric")
    }
    
    
    if(is.numeric(DataSelected[,4]) == FALSE){
      Fault = TRUE
      text1("Please make sure B_odds are numeric")
    }
    
    if(nrow(DataSelected)==0){
      Fault = TRUE
      text1("") 
      
    }
    if(is.numeric(DataSelected[,5]) == FALSE || any(is.na(DataSelected[,5]))){
      Fault = TRUE
      text1("Please make sure R_prob is numeric")
    }else{
      if(nrow(DataSelected)!=0){
        if(DataSelected[,5] <0 ||  DataSelected[,5] >1){
          Fault = TRUE
          text1("Please make sure R_prob is between 0 and 1")
        } 
      }
    }
    
    
    if(nrow(DataSelected)>0 && Fault == FALSE){
      #Call function
      text1(get_winner_plus_odds(DataSelected))
      
    }
  })
  
  output$rowData <- renderTable({
    
    Betting_Data()[input$Joined_Data_rows_selected,]
    
  })
  
  output$selected_var <- renderText({ 
    paste(text1())
  })
  
  
  output$Joined_Data_select = renderPrint(input$Joined_Data_rows_selected)
}



shinyApp(ui = ui, server = server)

