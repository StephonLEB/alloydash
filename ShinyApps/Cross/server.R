library(lubridate)
library(dplyr)
library(ggplot2)

library(shiny)
gar_set_client(scopes = c("https://www.googleapis.com/auth/analytics.readonly"))
options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/analytics.readonly"))
shinyServer(function(input, output,session) {
  
  auth <- callModule(gar_auth_js,"auth")
  library(googleAnalyticsR)
  organic_segment <- "gaid::-5"   
  seg_obj <- segment_ga4("Organic",segment_id = organic_segment)
  ga_id <- 1834906
  
  baseline <- reactive({boost <- with_shiny(google_analytics,ga_id,
                                            date_range=c(input$date[1],input$date[2]),
                                            dimensions=c("date"),
                                            segments = seg_obj,
                                            metrics=c("sessions"),
                                            shiny_access_token = auth())
  
  boost$date <- floor_date(boost$date,unit=input$choice)
  boost <- group_by(boost,date) %>% summarize(sessions=sum(sessions))
  
  
  })
  
  changeline <- reactive({
    
    ####Previous Month
    prev_period_boost <- with_shiny(google_analytics,ga_id,
                                    date_range=c(as.Date(input$date[1])-(as.Date(input$date[2])-as.Date(input$date[1])),
                                                 as.Date(input$date[2])-(as.Date(input$date[2])-as.Date(input$date[1]))),
                                    dimensions=c("date"),
                                    segments = seg_obj,
                                    metrics=c("sessions"),
                                    shiny_access_token = auth())
    
    
    
    prev_period_boost$date <- floor_date(prev_period_boost$date,unit=input$choice)
    prev_period_boost <- group_by(prev_period_boost,date) %>% summarize(sessions=sum(sessions))
    
    
    
  })
  
  ##Previous Year
  
  pastline <- reactive ({
    
    prev_year_boost <- with_shiny(google_analytics,ga_id,
                                  date_range=c(as.Date(input$date[1])-years(1),
                                               as.Date(input$date[2])-years(1)),
                                  dimensions=c("date"),
                                  segments = seg_obj,
                                  metrics=c("sessions"),
                                  shiny_access_token = auth())
    
    
    
    prev_year_boost$date <- floor_date(prev_year_boost$date,unit=input$choice)
    prev_year_boost <- group_by(prev_year_boost,date) %>% summarize(sessions=sum(sessions))
    
    
    
    
    
    
  })
  ############Goals
  goalline <- reactive({
    
    goals <- with_shiny(google_analytics,ga_id,
                        date_range=c(input$date[1],input$date[2]),
                        dimensions=c("date"),
                        metrics=c("goalCompletionsAll","costPerConversion","adCost"),
                        shiny_access_token = auth())
    
    goals$date <- floor_date(goals$date,unit=input$choice)
    goals <- group_by(goals,date) %>% summarize(goalCompletionsAll=sum(goalCompletionsAll),costPerConversion=mean(costPerConversion),adCost=sum(adCost))
  })
  ### Previous Month
  changegoals <- reactive({
    prev_period_goals <- with_shiny(google_analytics,ga_id,
                                    date_range=c(as.Date(input$date[1])-(as.Date(input$date[2])-as.Date(input$date[1])),
                                                 as.Date(input$date[2])-(as.Date(input$date[2])-as.Date(input$date[1]))),
                                    dimensions=c("date"),
                                    metrics=c("goalCompletionsAll","costPerConversion"),
                                    shiny_access_token = auth())
    
    
    
    prev_period_goals$date <- floor_date(prev_period_goals$date,unit=input$choice)
    prev_period_goals <- group_by(prev_period_goals,date) %>% summarize(goalCompletionsAll=sum(goalCompletionsAll),costPerConversion=mean(costPerConversion))
    
  })
  ### Previous Year 
  
  pastgoals <- reactive ({
    
    prev_year_goals <- with_shiny(google_analytics,ga_id,
                                  date_range=c(as.Date(input$date[1])-years(1),
                                               as.Date(input$date[2])-years(1)),
                                  dimensions=c("date"),
                                  metrics=c("goalCompletionsAll","costPerConversion"),
                                  shiny_access_token = auth())
    
    
    
    prev_year_goals$date <- floor_date(prev_year_goals$date,unit=input$choice)
    prev_year_goals <- group_by(prev_year_goals,date) %>% summarize(goalCompletionsAll=sum(goalCompletionsAll),costPerConversion=mean(costPerConversion))
    
  })
  ########### Table Data  
  landing <- reactive({
    pages <- with_shiny(google_analytics,ga_id,
                        date_range=c(input$date[1],input$date[2]),
                        dimensions=c("landingPagePath"),
                        segments = seg_obj,
                        metrics=c("sessions","bounceRate"),
                        shiny_access_token = auth())
  pages <- arrange(pages,desc(sessions)) %>% top_n(10,sessions) %>% rename('Bounce Rate'=bounceRate, Sessions = sessions)
  })
  
  output$line <- renderPlot({
    
    ##Oranic Plot
    crossings <- baseline()
    crossers <- changeline()
    crossmen <- pastline()
    boost <- baseline()
    prev_period_boost <- changeline()
    
    if(1 %in% input$lookback)
    {crossings <- crossings[1:length(crossers$date),]}
    else if(2 %in% input$lookback)
    {crossings <- crossings[1:length(crossmen$date),]}
    else
    {crossings <- crossings}
    
    if(1 %in% input$lookback)
    {ggplot(crossings,aes(x=date,y=sessions)) + geom_line(size=1.5) + xlim(min(crossings$date),max(crossings$date)) +
        theme_classic() + geom_line(data=crossers,aes(x=crossings$date,y=crossers$sessions),color="red",size=1.5) + expand_limits(y=0)}
    else if(2 %in% input$lookback)
    {ggplot(crossings,aes(x=date,y=sessions)) + geom_line(size=1.5) + xlim(min(crossings$date),max(crossings$date)) +
        theme_classic() + geom_line(data=crossmen,aes(x=crossings$date,y=crossmen$sessions),color="blue",size=1.5) + expand_limits(y=0)}
    else {ggplot(crossings,aes(x=date,y=sessions)) + geom_line(size=1.5) + xlim(min(crossings$date),max(crossings$date)) +
        theme_classic() + expand_limits(y=0)}
    ########Value Boxes
  })
  output$sessions <- renderValueBox({
    visits <- baseline()
    valueBox(sum(visits$sessions),"Organic Visits",icon=icon("window-maximize"))})
  
  output$goals <- renderValueBox({
    completions <- goalline()
    valueBox(sum(completions$goalCompletionsAll),"Goal Completions",icon=icon("filter"))
  })
  output$dollars <- renderValueBox({
    completions <- goalline()
    valueBox(round(sum(completions$adCost),digits = 2),"AdWords Cost",icon=icon("filter"))
  })
  # output$rate <- renderValueBox({
  #    num <- baseline()
  #   denom <- goalline()
  #  valueBox(sum(num$sessions)/sum(denom$goalCompletionsAll),"Rate",icon=icon("filter"))
  #  })
  output$goalgraph <- renderPlot({
    completes <- goalline()
    lastcompletes <- changegoals()
    yearcompletes <- pastgoals()
    
    if(1 %in% input$lookback)
    {completes <- completes[1:length(lastcompletes$date),]}
    else if(2 %in% input$lookback)
    {completes <- completes[1:length(yearcompletes$date),]}
    else
    {completes <- completes}
    
    if(1 %in% input$lookback)
    {ggplot(completes,aes(x=date,y=goalCompletionsAll)) + geom_line(size=1.5) + xlim(min(completes$date),max(completes$date)) +
        theme_classic() + geom_line(data=lastcompletes,aes(x=completes$date,y=lastcompletes$goalCompletionsAll),color="red",size=1.5)+ expand_limits(y=0)}
    else if(2 %in% input$lookback)
    {ggplot(completes,aes(x=date,y=goalCompletionsAll)) + geom_line(size=1.5) + xlim(min(completes$date),max(completes$date)) +
        theme_classic() + geom_line(data=yearcompletes,aes(x=completes$date,y=yearcompletes$goalCompletionsAll),color="blue",size=1.5) + expand_limits(y=0)}
    else {ggplot(completes,aes(x=date,y=goalCompletionsAll)) + geom_line(size=1.5) + xlim(min(completes$date),max(completes$date)) +
        theme_classic()+ expand_limits(y=0)}
  })
  ###Goals Breakdown
  goal1 <- reactive({
    goal1 <- with_shiny(google_analytics,ga_id,
                        date_range=c(input$date[1],input$date[2]),
                        dimensions=c("date"),
                        metrics=c("goal1Completions"),
                        shiny_access_token = auth())
    goal1 <- mutate(goal1,Completions=sum(goal1Completions)) %>% summarize(Completions=max(Completions)) %>% mutate(Goal = "Vendor Evaluation")
  })
  goal2 <- reactive({
    goal2 <- with_shiny(google_analytics,ga_id,
                        date_range=c(input$date[1],input$date[2]),
                        dimensions=c("date"),
                        metrics=c("goal2Completions"),
                        shiny_access_token = auth())
    goal2 <- mutate(goal2,Completions=sum(goal2Completions)) %>% summarize(Completions=max(Completions)) %>% mutate(Goal = "General Contact Form")
  })
  goal3 <- reactive({
    goal3 <- with_shiny(google_analytics,ga_id,
                        date_range=c(input$date[1],input$date[2]),
                        dimensions=c("date"),
                        metrics=c("goal3Completions"),
                        shiny_access_token = auth())
    goal3 <- mutate(goal3,Completions=sum(goal3Completions)) %>% summarize(Completions=max(Completions)) %>% mutate(Goal = "Consultation Request")
  })
  goal4 <- reactive({
    goal4 <- with_shiny(google_analytics,ga_id,
                        date_range=c(input$date[1],input$date[2]),
                        dimensions=c("date"),
                        metrics=c("goal4Completions"),
                        shiny_access_token = auth())
    goal4 <- mutate(goal4,Completions=sum(goal4Completions)) %>% summarize(Completions=max(Completions)) %>% mutate(Goal = "Newsletter Subscription")
  })
  goal5 <- reactive({
    goal5 <- with_shiny(google_analytics,ga_id,
                        date_range=c(input$date[1],input$date[2]),
                        dimensions=c("date"),
                        metrics=c("goal5Completions"),
                        shiny_access_token = auth())
    goal5 <- mutate(goal5,Completions=sum(goal5Completions)) %>% summarize(Completions=max(Completions)) %>% mutate(Goal = "Partner Request")
  })
  goal6 <- reactive({
    goal6 <- with_shiny(google_analytics,ga_id,
                        date_range=c(input$date[1],input$date[2]),
                        dimensions=c("date"),
                        metrics=c("goal6Completions"),
                        shiny_access_token = auth())
    goal6 <- mutate(goal6,Completions=sum(goal6Completions)) %>% summarize(Completions=max(Completions)) %>% mutate(Goal = "Customer Journey Download")
  })
  goal7 <- reactive({
    goal7 <- with_shiny(google_analytics,ga_id,
                        date_range=c(input$date[1],input$date[2]),
                        dimensions=c("date"),
                        metrics=c("goal7Completions"),
                        shiny_access_token = auth())
    goal7 <- mutate(goal7,Completions=sum(goal7Completions)) %>% summarize(Completions=max(Completions)) %>% mutate(Goal = "Myth Content Offer")
  })
  really <- reactive({
    goal1 <- goal1()
    goal2 <- goal2()
    goal3 <- goal3()
    goal4 <- goal4()
    goal5 <- goal5()
    goal6 <- goal6()
    goal7 <- goal7()
    reals <-bind_rows(goal1,goal2) %>% bind_rows(goal3) %>% bind_rows(goal4) %>% bind_rows(goal5) %>% bind_rows(goal6) %>% bind_rows(goal7)
  })
  
  
  output$goalbreak <- renderPlot({
    reality <- really()
    ggplot(reality,aes(x=Goal,y=Completions)) + geom_col(width=.65,fill="blue") + theme_classic()+ coord_flip()+theme(axis.text.y = element_text(size=10)) + labs(title="")
  })
  
  output$table <- renderTable({
    lands <- landing()
    lands
  })
  output$rate <- renderPlot({
    completes <- goalline()
    lastcompletes <- changegoals()
    yearcompletes <- pastgoals()
    
    if(1 %in% input$lookback)
    {completes <- completes[1:length(lastcompletes$date),]}
    else if(2 %in% input$lookback)
    {completes <- completes[1:length(yearcompletes$date),]}
    else
    {completes <- completes}
    
    if(1 %in% input$lookback)
    {ggplot(completes,aes(x=date,y=costPerConversion)) + geom_line(size=1.5) + xlim(min(completes$date),max(completes$date)) +
        theme_classic() + geom_line(data=lastcompletes,aes(x=completes$date,y=lastcompletes$costPerConversion),color="red",size=1.5) + expand_limits(y=0)}
    else if(2 %in% input$lookback)
    {ggplot(completes,aes(x=date,y=costPerConversion)) + geom_line(size=1.5) + xlim(min(completes$date),max(completes$date)) +
        theme_classic() + geom_line(data=yearcompletes,aes(x=completes$date,y=yearcompletes$costPerConversion),color="blue",size=1.5) + expand_limits(y=0)}
    else {ggplot(completes,aes(x=date,y=costPerConversion)) + geom_line(size=1.5) + xlim(min(completes$date),max(completes$date)) +
        theme_classic()+ expand_limits(y=0)}
  })
})
