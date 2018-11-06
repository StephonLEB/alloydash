
library(lubridate)
library(shiny)
library(shinydashboard)
library(googleAuthR)
options(
  googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/analytics.readonly")
)
options(googleAuthR.webapp.client_id = "826695155322-q0ujitovp3qsn497bahe9vahhjkcbvfq.apps.googleusercontent.com")
options(googleAuthR.webapp.client_secret = "cby-ySOakPQdXTPMBESMCTyC")

dashboardPage(skin = "red",
  dashboardHeader(title = "Alloy Marketing Dashboard", titleWidth = 400),
  dashboardSidebar(
    sidebarMenu(
      id = "menu",
      menuItem(
        "Dashboard",
        tabName = "dash",
        icon = icon("line-chart")
      ),
      menuItem("Landing Pages", tabName = "table", icon = icon("table")),
      
      dateRangeInput(
        "date",
        label = "Pick a Range",
        start = Sys.Date()-weeks(6),
        end = Sys.Date()
      ),
      conditionalPanel(
        "input.menu=='dash'",
        radioButtons(
          "choice",
          label = "How to View Traffic",
          choices =
            list(
              "Daily" = "day",
              "Weekly" = "week",
              "Monthly" = "month"
            ),
          selected = "day"
        ),
        radioButtons(
          "lookback",
          label = "Compare to:",
          choices = list(
            "None" = "",
            "Previous Period" = 1,
            "Last Year" = 2
          ),
          selected = ""
        ),
        gar_auth_jsUI("auth")
        
      )
    )
  ),
  dashboardBody(tabItems(
    tabItem(
      tabName = "dash",
      fluidRow(
        box(
          plotOutput("line"),
          width = 12,
          title = "Organic Visits",
          status = "danger",
          solidHeader = TRUE
        )
        
      ),
      fluidRow(
        valueBoxOutput("sessions", width = 3),
        valueBoxOutput("goals", width = 3),
        valueBoxOutput("dollars", width = 3),
        valueBoxOutput("paidgoals", width = 3)
      ),
      fluidRow(
        box(
          plotOutput("goalgraph"),
          width = 12,
          title = "Goal Completions (Red = Previous Period)",
          status = "danger",
          solidHeader = TRUE
        )
      ),
      fluidRow(
        box(
          plotOutput("goalbreak"),
          width = 12,
          title = "Goal Completions Breakdown",
          status = "danger",
          solidHeader = TRUE
        )
      )
    ),
    tabItem(tabName = "table", fluidRow(box(
      DT::DTOutput("table",height=425), width = 6
    ),box(plotOutput("pagewise",height=425),width = 6))
)
    
  ))
  
)
