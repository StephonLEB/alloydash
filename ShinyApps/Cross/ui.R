
library(shiny)
library(shinydashboard)
library(googleAuthR)
options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/analytics.readonly"))
options(googleAuthR.webapp.client_id = "826695155322-q0ujitovp3qsn497bahe9vahhjkcbvfq.apps.googleusercontent.com")
options(googleAuthR.webapp.client_secret = "cby-ySOakPQdXTPMBESMCTyC")

dashboardPage(
  dashboardHeader(title="Alloy Marketing Dashboard",titleWidth = 400),
  dashboardSidebar(
    sidebarMenu(id="menu",
      menuItem("Dashboard",tabName = "dash",icon = icon("line-chart")),
      menuItem("Landing Pages",tabName = "table",icon = icon("table")),
      
             dateRangeInput("date",
                            label="Pick a Range",
                            start="2018-06-01",
                            end="2018-06-31"),
      conditionalPanel("input.menu=='dash'", radioButtons("choice",
                          label = "How to View Traffic",
                          choices=list("Daily"="day","Weekly"="week","Monthly"="month"),
                          selected = "day"),
             radioButtons("lookback",
                          label = "Compare to:",
                          choices = list("None"="","Previous Period"=1,"Last Year"=2),
                          selected = ""),
             gar_auth_jsUI("auth")
             
  )
    )  
  ),
  dashboardBody(
      tabItems(
             tabItem(tabName = "dash", fluidRow(
                box(plotOutput("line"),width=12,title="Organic Traffic",status="danger",solidHeader = TRUE
                )
                
              ),
              fluidRow(
                valueBoxOutput("sessions",width = 4),
                valueBoxOutput("goals",width = 4),
                valueBoxOutput("dollars",width = 4)
              ),
              fluidRow( box(plotOutput("goalgraph"),width=12,title="Goal Completions (Red = Previous Period)",status="danger",solidHeader = TRUE
              )
              ),
              fluidRow( box(plotOutput("goalbreak"),width=12,title="Goal Completions Breakdown",status="danger",solidHeader = TRUE
              )
              ),
              fluidRow(box(plotOutput("rate"),width = 12,title="AdWords Cost Per Lead",status = "danger",solidHeader = TRUE))),
              tabItem(tabName="table",fluidRow(box(tableOutput("table"),width = 6)))
      
      )
  )
  
)

