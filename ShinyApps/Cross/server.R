library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(magrittr)
library(DT)
library(rvest)
library(purrr)
library(stringr)

library(shiny)
gar_set_client(scopes = c("https://www.googleapis.com/auth/analytics.readonly"))
options(
    googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/analytics.readonly")
)
shinyServer(function(input, output, session) {
    auth <- callModule(gar_auth_js, "auth")
    library(googleAnalyticsR)
    organic_segment <- "gaid::-5"
    seg_obj <- segment_ga4("Organic", segment_id = organic_segment)
    paid_obj <- segment_ga4("Paid", segment_id = "gaid::-4")
    blogs_list <- dim_filter("landingPagePath","REGEXP","blog/") %>% list() %>% filter_clause_ga4()
    ga_id <- 1834906
    
    baseline <- reactive({
        boost <- with_shiny(
            google_analytics,
            ga_id,
            date_range = c(input$date[1], input$date[2]),
            dimensions = c("date"),
            segments = seg_obj,
            metrics = c("sessions"),
            shiny_access_token = auth()
        )
        
        boost$date <- floor_date(boost$date, unit = input$choice)
        boost <-
            group_by(boost, date) %>% summarize(sessions = sum(sessions))
        
        
    })
    
    changeline <- reactive({
        ####Previous Month
        prev_period_boost <- with_shiny(
            google_analytics,
            ga_id,
            date_range = c(
                as.Date(input$date[1]) - (as.Date(input$date[2]) - as.Date(input$date[1])),
                as.Date(input$date[2]) -
                    (as.Date(input$date[2]) - as.Date(input$date[1]))
            ),
            dimensions = c("date"),
            segments = seg_obj,
            metrics = c("sessions"),
            shiny_access_token = auth()
        )
        
        
        
        prev_period_boost$date <-
            floor_date(prev_period_boost$date, unit = input$choice)
        prev_period_boost <-
            group_by(prev_period_boost, date) %>% summarize(sessions = sum(sessions))
        
        
        
    })
    
    ##Previous Year
    
    pastline <- reactive ({
        prev_year_boost <- with_shiny(
            google_analytics,
            ga_id,
            date_range = c(
                as.Date(input$date[1]) - years(1),
                as.Date(input$date[2]) -
                    years(1)
            ),
            dimensions = c("date"),
            segments = seg_obj,
            metrics = c("sessions"),
            shiny_access_token = auth()
        )
        
        
        
        prev_year_boost$date <-
            floor_date(prev_year_boost$date, unit = input$choice)
        prev_year_boost <-
            group_by(prev_year_boost, date) %>% summarize(sessions = sum(sessions))
        
        
        
        
        
        
    })
    ############Goals
    goalline <- reactive({
        goals <- with_shiny(
            google_analytics,
            ga_id,
            date_range = c(input$date[1], input$date[2]),
            dimensions = c("date"),
            metrics = c("goalCompletionsAll", "costPerConversion", "adCost"),
            shiny_access_token = auth()
        )
        
        goals$date <- floor_date(goals$date, unit = input$choice)
        goals <-
            group_by(goals, date) %>% summarize(
                goalCompletionsAll = sum(goalCompletionsAll),
                costPerConversion = mean(costPerConversion),
                adCost = sum(adCost)
            )
    })
    ### Previous Month
    changegoals <- reactive({
        prev_period_goals <- with_shiny(
            google_analytics,
            ga_id,
            date_range = c(
                as.Date(input$date[1]) - (as.Date(input$date[2]) - as.Date(input$date[1])),
                as.Date(input$date[2]) -
                    (as.Date(input$date[2]) - as.Date(input$date[1]))
            ),
            dimensions = c("date"),
            metrics = c("goalCompletionsAll", "costPerConversion"),
            shiny_access_token = auth()
        )
        
        
        
        prev_period_goals$date <-
            floor_date(prev_period_goals$date, unit = input$choice)
        prev_period_goals <-
            group_by(prev_period_goals, date) %>% summarize(
                goalCompletionsAll = sum(goalCompletionsAll),
                costPerConversion = mean(costPerConversion)
            )
        
    })
    ### Previous Year
    
    pastgoals <- reactive ({
        prev_year_goals <- with_shiny(
            google_analytics,
            ga_id,
            date_range = c(
                as.Date(input$date[1]) - years(1),
                as.Date(input$date[2]) -
                    years(1)
            ),
            dimensions = c("date"),
            metrics = c("goalCompletionsAll", "costPerConversion"),
            shiny_access_token = auth()
        )
        
        
        
        prev_year_goals$date <-
            floor_date(prev_year_goals$date, unit = input$choice)
        prev_year_goals <-
            group_by(prev_year_goals, date) %>% summarize(
                goalCompletionsAll = sum(goalCompletionsAll),
                costPerConversion = mean(costPerConversion)
            )
        
    })
    ########### Table Data
    landing <- reactive({
        pages <- with_shiny(
            google_analytics,
            ga_id,
            date_range = c(input$date[1], input$date[2]),
            dimensions = c("landingPagePath"),
            segments = seg_obj,
            metrics = c("sessions", "bounceRate"),
            shiny_access_token = auth()
        )
        pages <- arrange(pages, desc(sessions)) %>%
            top_n(15, sessions) %>%
            mutate(bounceRate = round(bounceRate, 1)) %>%
            rename(
                'Bounce Rate' = bounceRate,
                Sessions = sessions,
                'Landing Page Path' = landingPagePath
            ) %>%
            select(1, 3, 4)
    })
    
    ###### AdWords Goal Completions
    adwords <- reactive({
        paidconverts <- with_shiny(
            google_analytics,
            ga_id,
            date_range = c(input$date[1], input$date[2]),
            dimensions = c("date"),
            metrics = c("goalCompletionsAll"),
            segments = paid_obj,
            shiny_access_token = auth()
        )
    })
    
    
    output$line <- renderPlot({
        ##Oranic Plot
        crossings <- baseline()
        crossers <- changeline()
        crossmen <- pastline()
        boost <- baseline()
        prev_period_boost <- changeline()
        
        if (1 %in% input$lookback)
        {
            crossings <- crossings[1:length(crossers$date), ]
        }
        else if (2 %in% input$lookback)
        {
            crossings <- crossings[1:length(crossmen$date), ]
        }
        else
        {
            crossings <- crossings
        }
        
        if (1 %in% input$lookback)
        {
            ggplot(crossings, aes(x = date, y = sessions)) +
                geom_line(size = 1.5) +
                xlim(min(crossings$date), max(crossings$date)) +
                theme_classic() +
                geom_line(
                    data = crossers,
                    aes(x = crossings$date, y = crossers$sessions),
                    color = "red",
                    size = 1.5
                ) +
                theme(
                    axis.line.y = element_blank(),
                    panel.grid.major.y = element_line(color = "gray"),
                    axis.ticks.y = element_blank(),
                    axis.text = element_text(size = 12)
                ) +
                labs(y = NULL, x = NULL) +
                expand_limits(y = 0)
        }
        else if (2 %in% input$lookback)
        {
            ggplot(crossings, aes(x = date, y = sessions)) +
                geom_line(size = 1.5) +
                xlim(min(crossings$date), max(crossings$date)) +
                theme_classic() +
                geom_line(
                    data = crossmen,
                    aes(x = crossings$date, y = crossmen$sessions),
                    color = "blue",
                    size = 1.5
                ) +
                theme(
                    axis.line.y = element_blank(),
                    panel.grid.major.y = element_line(color = "gray"),
                    axis.ticks.y = element_blank(),
                    axis.text = element_text(size = 12)
                ) +
                labs(y = NULL, x = NULL) +
                expand_limits(y = 0)
        }
        else {
            ggplot(crossings, aes(x = date, y = sessions)) +
                geom_line(size = 1.5) + xlim(min(crossings$date), max(crossings$date)) +
                theme_classic() +
                theme(
                    axis.line.y = element_blank(),
                    panel.grid.major.y = element_line(color = "gray"),
                    axis.ticks.y = element_blank(),
                    axis.text = element_text(size = 12)
                ) +
                labs(y = NULL, x = NULL) +
                expand_limits(y = 0)
        }
        ########Value Boxes
    })
    output$sessions <- renderValueBox({
        visits <- baseline()
        valueBox(sum(visits$sessions),
                 "Organic Visits",
                 icon = icon("window-maximize"))
    })
    
    output$goals <- renderValueBox({
        completions <- goalline()
        valueBox(sum(completions$goalCompletionsAll),
                 "Goal Completions",
                 icon = icon("filter"))
    })
    output$dollars <- renderValueBox({
        completions <- goalline()
        valueBox(round(sum(completions$adCost), digits = 2), "AdWords Cost", icon =
                     icon("filter"))
    })
    
    output$paidgoals <- renderValueBox({
        ads <- adwords()
        valueBox(sum(ads$goalCompletionsAll),
                 "AdWords Goal Completions",
                 icon = icon("filter"))
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
        
        if (1 %in% input$lookback)
        {
            completes <- completes[1:length(lastcompletes$date), ]
        }
        else if (2 %in% input$lookback)
        {
            completes <- completes[1:length(yearcompletes$date), ]
        }
        else
        {
            completes <- completes
        }
        
        if (1 %in% input$lookback)
        {
            ggplot(completes, aes(x = date, y = goalCompletionsAll)) +
                geom_line(size = 1.5) + xlim(min(completes$date), max(completes$date)) +
                theme_classic() +
                geom_line(
                    data = lastcompletes,
                    aes(
                        x = completes$date,
                        y = lastcompletes$goalCompletionsAll
                    ),
                    color = "red",
                    size = 1.5
                ) +
                theme(
                    axis.line.y = element_blank(),
                    panel.grid.major.y = element_line(color = "gray"),
                    axis.ticks.y = element_blank(),
                    axis.text = element_text(size = 12)
                ) +
                labs(y = NULL, x = NULL) +
                expand_limits(y = 0)
        }
        else if (2 %in% input$lookback)
        {
            ggplot(completes, aes(x = date, y = goalCompletionsAll)) +
                geom_line(size = 1.5) + xlim(min(completes$date), max(completes$date)) +
                theme_classic() +
                geom_line(
                    data = yearcompletes,
                    aes(
                        x = completes$date,
                        y = yearcompletes$goalCompletionsAll
                    ),
                    color = "blue",
                    size = 1.5
                ) +
                theme(
                    axis.line.y = element_blank(),
                    panel.grid.major.y = element_line(color = "gray"),
                    axis.ticks.y = element_blank(),
                    axis.text = element_text(size = 12)
                ) +
                labs(y = NULL, x = NULL) +
                expand_limits(y = 0)
        }
        else {
            ggplot(completes, aes(x = date, y = goalCompletionsAll)) +
                geom_line(size = 1.5) + xlim(min(completes$date), max(completes$date)) +
                theme_classic() +
                theme(
                    axis.line.y = element_blank(),
                    panel.grid.major.y = element_line(color = "gray"),
                    axis.ticks.y = element_blank(),
                    axis.text = element_text(size = 12)
                ) +
                labs(y = NULL, x = NULL) +
                expand_limits(y = 0)
        }
    })
    ###Goals Breakdown
    
    really <- reactive({
        vro <- with_shiny(
            google_analytics,
            ga_id,
            date_range = c(input$date[1], input$date[2]),
            dimensions = "date",
            metrics = c(
                "goal1Completions",
                "goal2Completions",
                "goal3Completions",
                "goal4Completions",
                "goal5Completions",
                "goal6Completions",
                "goal7Completions"
            ),
            shiny_access_token = auth()
        )
        vro <-
            vro %>% mutate(goal1Completions = sum(goal1Completions)) %>%
            mutate(goal2Completions = sum(goal2Completions)) %>%
            mutate(goal3Completions = sum(goal3Completions)) %>%
            mutate(goal4Completions = sum(goal4Completions)) %>%
            mutate(goal5Completions = sum(goal5Completions)) %>%
            mutate(goal6Completions = sum(goal6Completions)) %>%
            mutate(goal7Completions = sum(goal7Completions))
        vro <- vro[1, ]
        vro <- gather(vro, goaltype, Completions, 2:8)
        Goal <- c(
            "Vendor Evaluation",
            "General Contact Form",
            "Consultation Request",
            "Newsletter Subscription",
            "Partner Request",
            "Customer Journey Download",
            "Myth Content Offer"
        )
        vro <- cbind(vro, Goal)
        vro <- vro[, 3:4]
    })
    
    
    output$goalbreak <- renderPlot({
        reality <- really()
        ggplot(reality, aes(x = Goal, y = Completions)) +
            geom_col(width = .65, fill = "blue") +
            theme_classic() +
            coord_flip() +
            theme(
                axis.text = element_text(size = 12),
                panel.grid.minor.x = element_line(color = "gray")
            ) +
            labs(title = "",
                 x = NULL,
                 y = NULL)
    })
    
    output$table <-
        DT::renderDT(
            landing(),
            server = TRUE,
            selection = list(
                target = 'row',
                selected = 1,
                mode = 'single'
            ),
            options = list(pageLength = 7, lengthMenu = c(7, 15))
        )
    
    pagewisetrends <- reactive({
        pages <- with_shiny(
            google_analytics,
            ga_id,
            date_range = c(input$date[1], input$date[2]),
            dimensions = c("landingPagePath"),
            segments = seg_obj,
            metrics = c("sessions", "bounceRate"),
            shiny_access_token = auth()
        )
        pages <-
            arrange(pages, desc(sessions)) %>% top_n(10, sessions) %>% rename('Bounce Rate' =
                                                                                  bounceRate, Sessions = sessions)
    })
    top_pages <- reactive({
        #top <- pages$landingPagePath
        pages <-
            dim_filter(
                "landingPagePath",
                operator = "IN_LIST",
                expressions = pagewisetrends()$landingPagePath[input$table_rows_selected]
            )
    })
    top_landing <- reactive({
        #tops <- top_pages()
        page_filter <- filter_clause_ga4(list(top_pages()))
    })
    final <- reactive({
        top_pages <- with_shiny(
            google_analytics,
            ga_id,
            date_range = c(input$date[1], input$date[2]),
            dimensions = c("week", "landingPagePath"),
            segments = seg_obj,
            metrics = c("sessions", "bounceRate"),
            dim_filters = top_landing(),
            shiny_access_token = auth()
        )
        top_pages$landingPagePath %<>% as.factor()
        top_pages$week <- top_pages$week %>% as.numeric()
        top_pages
    })
    
    output$pagewise <- renderPlot({
        pagetrends <- final()
        ggplot(data = pagetrends, aes(
            x = week,
            y = sessions,
            color = landingPagePath
        )) +
            geom_line(size = 2) +
            theme_classic() +
            theme(
                legend.position = "bottom",
                axis.line.y = element_blank(),
                panel.grid.major.y = element_line(color = "gray"),
                axis.ticks.y = element_blank(),
                axis.text = element_text(size = 12)
            ) +
            labs(y = NULL) +
            expand_limits(y = 0)
    })
    blogtraffic_intermediate <- reactive({
        blog_pages <- with_shiny(google_analytics,ga_id, 
                                  date_range = c(input$date[1], input$date[2]),
                                  dimensions = c("landingPagePath"),
                                  metrics = c("sessions"),
                                  dim_filters = blogs_list,
                                  segments = seg_obj,
                            shiny_access_token = auth())
        blog_pages <- blog_pages %>% mutate(page = paste0("https://www.alloymagnetic.com",landingPagePath))
        blog_pages <- blog_pages %>% mutate(author = map(page,~try(read_html(.) %>% 
                                                             html_node("span.author") %>% 
                                                             html_text())) %>% unlist()) 
        
        
    })
    blogtraffic_tabular <- reactive({
        blog_pages <- blogtraffic_intermediate()
        blog_pages <- blog_pages %>% group_by(author) %>% summarise(sessions = sum(sessions),
                                                                    `# of blogs` = n()) %>%
            filter(!str_detect(author, "404")) %>%
            mutate(author = str_remove(author, " by ")) %>%
            arrange(desc(sessions))
    })
    
    output$blog_table <- DT::renderDT(
        blogtraffic_tabular(),
        server = TRUE,
        selection = list(
            target = 'row',
            selected = 1,
            mode = 'single'
        )
    )
    
    blogtraffic_expressive <- reactive({
        blog_pages <- blogtraffic_intermediate()
        blog_pages <- blog_pages %>% 
            mutate(author = str_remove(author, " by ")) %>% 
            filter(!str_detect(author, "404")) %>% 
            filter(author %in% blogtraffic_tabular()$author[input$blog_table_rows_selected]) %>%
            select(-page,-author)
    })
    
    
    output$blog_expanded <- DT::renderDT(
        blogtraffic_expressive(),
        server = TRUE,
        selection = "none"
    )
})
