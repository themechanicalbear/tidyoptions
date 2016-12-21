# Header-----------------------------------------------------------------------
# title: "Data processing step #1"
# author: "Jason Taylor"
# update: "Dec-20-2016"


# TODO(jason):-----------------------------------------------------------------


# Instructions:----------------------------------------------------------------
# This is the first script in the data cleansing process from iVolatility.com
#  **************  Uncomment to run after downloading data. *******************
# **************    Raw data is not included in package.   ********************
# **************         Instructions to get data     *************************

# 1.  Download data from http://www.ivolatility.com/data_download.j
#     - Individual contracts (Raw IV) data
#     - include greeks in file download
# 2.  Combine multiple raw files to one file
# 3.  Ensure that the market closed dates are up to date
# 4.  IVRank data from http://www.cboe.com/micro/equityvix/introduction.aspx
#     - Make sure you remove the disclaimer at the top line of file

# Build------------------------------------------------------------------------
comment.for.build <- TRUE
if (comment.for.build == FALSE) {

# Setup------------------------------------------------------------------------
  # Ensure environment includes libraries needed
  library(data.table)
  library(dplyr)
  library(TTR)
  library(RcppBDT)
  library(purrr)

  # BEGIN - Area of customization for different symbols--------------------------

  # Add IV data to dataset choose file associated with the symbol chosen
  data(vx.vix.daily.prices)

  # Script expects files in the working directory under the following folder
  files <- list.files(path = "data/ivolatility_raw_files",
                      pattern = ".csv", full.names = TRUE)

  rbind_csv <- function(file_name) {
    fread(file_name)
  }

  dat <- map(files, rbind_csv)
  raw_data <- bind_rows(dat)

  # Assign symbol variable to process
  symbol <- as.character(raw_data[1, "symbol"])


  # Process data-----------------------------------------------------------------

  # Dates the market was closed and can't be used in studies
  closed_dates <- fread("data/market_closed.csv")
  closed_dates$closed_dates <- as.Date(closed_dates$closed_dates)

  # Remove columns we don't need to save space and processing time in later
  # This removes (exchange, style, stock price for IV, and *)
  raw_data <- raw_data %>%
    select(-c(2, 9, 16, 17)) %>%
    mutate(date = as.Date(date, "%m/%d/%y"),
           expiration = as.Date(expiration, "%m/%d/%y"))

  names(raw_data) <- c("symbol", "date", "price", "option", "expiration",
                       "strike", "call.put", "ask", "bid", "mid.price",
                       "iv", "volume", "open.interest", "delta", "vega",
                       "gamma", "theta", "rho")

  # Third Friday expiration dates
  monthy_exp_dates <-
    third_friday(min(year(raw_data$date)), max(year(raw_data$date)))

  # Remove rows for dates that the market was closed
  # For some reason iVolatility sometimes includes these
  raw_data <- raw_data %>%
    mutate(exp_day = weekdays(expiration, abbreviate = FALSE),
           expiration = as.Date(ifelse(exp_day == "Saturday",
                                       expiration - 1,
                                       expiration), origin = "1970-01-01"),
           expiration = as.Date(ifelse(expiration %in% closed_dates,
                                       expiration - 1,
                                       expiration), origin = "1970-01-01"),
           dte = as.integer(expiration - date),
           exp_type = ifelse(is.element(expiration, monthy_exp_dates),
                             "Monthly", "Weekly")) %>%
    filter(!date %in% closed_dates) %>%
    select(-exp_day)

  # Calculate rsi_14 day for use in studies
  # Gather all the unique trading dates to calculate study values
  unique_dates <- raw_data %>%
    distinct(date, .keep_all = TRUE) %>%
    mutate(rsi_14 = RSI(price, 14)) %>%
    select(date, rsi_14)

  raw_data <- left_join(raw_data, unique_dates, by = "date")

  # Remove market closed dates from IV data
  iv.raw.data <- iv.raw.data %>%
    mutate(date = as.Date(Date, "%m/%d/%Y")) %>%
    filter(!date %in% closed_dates) %>%
    mutate(iv_rank_252 = round(100 * runPercentRank(Close, n = 252,
                                                    cumulative = FALSE,
                                                    exact.multiplier = 1),
                               digits = 0),
           iv_rank_90 = round(100 * runPercentRank(Close, n = 90,
                                                   cumulative = FALSE,
                                                   exact.multiplier = 1),
                              digits = 0)) %>%
    select(date, iv_rank_252, iv_rank_90)

  # Join the data together adding iv_rank
  raw_data <- left_join(raw_data, iv.raw.data, by = "date")

  complete.data <- raw_data %>%
    filter(rsi_14 != "NA",
           iv_rank_252 != "NA")

  # Export to .rda file for package
  save(complete.data, file = paste0(symbol, ".options.RData"))
}
