# TODO(jason):------------------------------------------------------------------
# Add menu items to show why trades were not opened
# Add buy and hold to the profit plot (Short Put, Strangle complete)
# Change buy and hold plot to return on margin instead of profit
# Test all stocks, studies, and dates
# Move to root folder
# If expiration is reached, make full profit or specific loss instead of just
#    end of day price
# Add confidence intervals to the plots
# Change all long variable names to underscores
# Function names to verbs
# Argument names in functions (df, x, y, z, p, n)
# detail arguments should always be given a default value

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinythemes)

# Shiny UI----------------------------------------------------------------------
# JavaScript ----
actionLink <- function(inputId, ...) {
  tags$a(href = 'javascript:void',
         id = inputId,
         class = 'action-button',
         ...)
}

warnings.table <- data.frame()

shinydashboard::dashboardPage(
  skin = "green",
  shinydashboard::dashboardHeader(
    shinydashboard::dropdownMenuOutput("messageMenu"),
    shinydashboard::dropdownMenuOutput("notificationMenu"),
    shinydashboard::dropdownMenuOutput("taskMenu")
  ),
  # Sidebar Section ----
  sidebar <- shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      shinydashboard::menuItem("Study", tabName = "Study", icon = icon("cogs"),
               shiny::selectInput("stock", "Stock", symbol_list),
               shiny::selectInput("study", "Study", c("Short Put", "Strangle")),
               shiny::selectInput("openOption", "Open on", c("First of Month", "First of Week", "Daily"))),
      shinydashboard::menuItem("Entry Criteria", tabName = "Entry Criteria", icon = icon("cogs"),
                               shiny::sliderInput("open_dte", "DTE", 0, 90, 45, step = 5),
                               shiny::conditionalPanel(
                 condition = ("input_study == 'Strangle'"),
                 shiny::sliderInput("call_delta", "Call delta", 0, 1, .16, step = .01)),
                 shiny::conditionalPanel(
                 condition = ("input_study == 'Short Put' || input_study == 'Strangle'"),
                 shiny::sliderInput("put_delta", "Put delta", -1, 0, -.16, step = .01)),
               # shiny::conditionalPanel(
               #   condition = ("input_study == 'Call Calendar' || input_study == 'Poor Mans Cov Call'"),
               #   shiny::sliderInput("second_dte", "Min short DTE", 0, 90, 30, step = 5)),
               shiny::sliderInput("open_ivrank", "IV Rank", 0, 100, c(0, 100), step = 1)),
      # shiny::sliderInput("min_roc", "Min ROC", 0, 50, 0, step = 1)),
      shinydashboard::menuItem("Exit Criteria", tabName = "Exit Criteria", icon = icon("cogs"),
                               shiny::sliderInput("proftarg", "Profit target %", 0, 100, 50, step = 5),
                               shiny::sliderInput("loss_lim", "Max loss x times credit received", 0, 10, 2, step = .25),
               # shiny::conditionalPanel(
               #   condition = ("input_study == 'Poor Mans Cov Call'"),
               #   shiny::sliderInput("l_loss_lim", "Long max loss % debit paid", 10, 100, 50, step = 5)),
               shiny::sliderInput("gamma_days", "Days prior to expiration", 0, 45, 0, step = 1)),
      # shiny::conditionalPanel(
      #   condition = ("(input_study == 'Short Put' || input_study == 'Strangle' || input_study == 'Straddle') &&
      #                  (input_stock == 'AMZN' || input_stock == 'GS' || input_stock == 'IBM')  &&
      #                  (input_openOption == 'First of Week' || input_openOption == 'First of Month' ||
      #                  input_openOption == 'Previous Close' || input_openOption == 'Daily')"),
      #   shiny::selectInput("earn_close", "Close day prior to earnings?", c("No", "Yes")))),
      shiny::actionButton('goPlot', 'Run Study', icon = icon("play-circle")))
  ),
  # Body Section ----
  body <- shinydashboard::dashboardBody(
    shiny::fluidRow(
      shiny::column(width = 4,
             tags$head(HTML("<script type='text/javascript' src='google-analytics.js'></script>")),
             useShinyjs(),
             shiny::htmlOutput("total_profit"),
             shiny::htmlOutput("avg_prof_trade"),
             shiny::htmlOutput("avg_prof_day"),
             shiny::htmlOutput("avg_days"),
             h4("")),
      shiny::column(width = 4,
                    shiny::htmlOutput("n_trades"),
                    shiny::htmlOutput("percent_winners"),
                    shiny::htmlOutput("max_loss"),
                    shiny::htmlOutput("max_win")),
      shiny::column(width = 4,
                    shiny::htmlOutput("exit_profit_target"),
                    shiny::htmlOutput("exit_loss_limit"),
                    shiny::htmlOutput("exit_expiration"),
                    shiny::htmlOutput("exit_gamma_risk"))),
    shiny::fluidRow(
      tabBox(
        id = "tabset1", height = "800px", width = "1000px",
        shiny::tabPanel("Trade Results",
                 #imageOutput("loading.image"),
                 shiny::htmlOutput("welcome.message"),
                 rbokehOutput("rbokeh.trades"),
                 shiny::fluidRow(
                   shiny::column(width = 1,
                          ""),
                   shiny::column(width = 5,
                                 shiny::selectInput("xvar", "X-axis variable", axis_vars, selected = "open_ivrank")),
                   shiny::column(width = 6,
                                 shiny::selectInput("yvar", "Y-axis variable", axis_vars, selected = "profit"))
                 )),
        shiny::tabPanel("Portfolio",
                 rbokehOutput("rbokeh.profits")),
        shiny::tabPanel("Table",
                        shiny::downloadButton('downloadData', 'Download'),
                 h4(" "),
                 shiny::dataTableOutput('table'))
        # shiny::tabPanel("Warnings",
        #          shiny::dataTableOutput("table2"))
      )
    )
  )
)
