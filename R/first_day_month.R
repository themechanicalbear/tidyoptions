# Header------------------------------------------------------------------------
# title: "First day of month calendar"
# author: "Jason Taylor"
# update: "Dec-20-2016"


# TODO(jason):------------------------------------------------------------------


# Instructions:-----------------------------------------------------------------
# - Update the market closed dates prior to running
# - Calendar is created through 2020 update if needed


comment.for.build <- TRUE
if (comment.for.build == FALSE) {

  # Dates the market was closed and can'te be used in studies
  closed_dates <- fread("data/market_closed.csv")
  closed_dates$closed_dates <- as.Date(closed_dates$closed_dates)

  # Create first day of the month data file for use on trade entry
  open_first_day_month <- data.frame(day = 1,
                                     mon = c(1:12, 1:12, 1:12, 1:12, 1:12, 1:12, 1:12, 1:12,
                                             1:12, 1:12, 1:12),
                                     year = c(rep(2010, 12),rep(2011, 12), rep(2012, 12),
                                              rep(2013, 12), rep(2014, 12), rep(2015, 12),
                                              rep(2016, 12), rep(2017, 12), rep(2018, 12),
                                              rep(2019, 12), rep(2020, 12)))

  # Combine the columns to create data frame of dates
  open_first_day_month <- open_first_day_month %>%
    mutate(date = as.Date(mdy(paste0(mon, "-", day, "-", year)))) %>%
    mutate(day_week = weekdays(date, abbreviate = FALSE)) %>%
    mutate(date = ifelse(day_week == "Saturday", date + 2, date)) %>%
    mutate(date = ifelse(day_week == "Sunday", date + 1, date)) %>%
    mutate(date = as.Date(date, origin = "1970-01-01")) %>%
    mutate(date = ifelse(date %in% closed_dates, date + 1, date)) %>%
    mutate(date = as.Date(date, origin = "1970-01-01")) %>%
    select(date)

  # Save data to be resused in options.data package
  save(open_first_day_month, file = "data/open_first_day_month.RData")

}





