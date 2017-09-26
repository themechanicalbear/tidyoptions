# TODO(jason):------------------------------------------------------------------
# - Should axis variable be in global?
# - Change symbol_list to read from file list in data folder

# Global setup
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyjs)
library(ggvis)
library(rbokeh)
library(tidyoptions)

# Variables that can be put on the axis
axis_vars <- c(
  "Days Held" = "days_held",
  "IV Rank" = "open_ivrank",
  "Open ROC" = "open_roc",
  "Profit" = "profit",
  "RSI" = "open_rsi",
  "Year" = "year"
  )

symbol_list <- c("AMZN", "EEM", "EWZ", "FXI", "GDX", "GS", "IBM", "SLV", "SPY", "XLE")
