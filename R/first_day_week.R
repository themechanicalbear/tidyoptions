# Header------------------------------------------------------------------------
# title: "First day of week calendar"
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

  # Create first day of the week data file for use on trade entry
  open_first_day_week <- data.frame(mon = c(1:12, 1:12, 1:12, 1:12, 1:12, 1:12, 1:12,
                                            1:12, 1:12, 1:12, 1:12),
                                    year = c(rep(2010, 12),rep(2011, 12), rep(2012, 12),
                                             rep(2013, 12), rep(2014, 12), rep(2015, 12),
                                             rep(2016, 12), rep(2017, 12), rep(2018, 12),
                                             rep(2019, 12), rep(2020, 12)))
  first_monday <- sapply(1:120,
                         function(i) format(getNthDayOfWeek(first, Mon,
                                                            open_first_day_week[i,1],
                                                            open_first_day_week[i,2])))
  second_monday <- sapply(1:120,
                          function(i) format(getNthDayOfWeek(second, Mon,
                                                             open_first_day_week[i,1],
                                                             open_first_day_week[i,2])))
  third_monday <- sapply(1:120,
                         function(i) format(getNthDayOfWeek(third, Mon,
                                                            open_first_day_week[i,1],
                                                            open_first_day_week[i,2])))
  fourth_monday <- sapply(1:120,
                          function(i) format(getNthDayOfWeek(fourth, Mon,
                                                             open_first_day_week[i,1],
                                                             open_first_day_week[i,2])))
  fifth_monday <- sapply(1:120,
                         function(i) format(getNthDayOfWeek(fifth, Mon,
                                                            open_first_day_week[i,1],
                                                            open_first_day_week[i,2])))

  open_first_day_week <- cbind(first_monday, second_monday, third_monday, fourth_monday,
                               fifth_monday)
  open_first_day_week <- as.data.frame(open_first_day_week, stringsAsFactors = FALSE)
  open_first_day_week <- mutate(open_first_day_week,
                                fifth_monday = ifelse(fifth_monday == fourth_monday,
                                                      "", fifth_monday))

  open_first_day_week <- unlist(open_first_day_week)
  open_first_day_week <- as.data.frame(open_first_day_week)
  names(open_first_day_week) <- "date"

  open_first_day_week <- open_first_day_week %>%
    mutate(date = as.Date(date)) %>%
    arrange(date) %>%
    mutate(date = ifelse(date %in% closed_dates, date + 1, date)) %>%
    mutate(date = as.Date(date, origin = "1970-01-01")) %>%
    mutate(day_week = weekdays(date, abbreviate = FALSE)) %>%
    mutate(date = ifelse(day_week == "Saturday", date + 2, date)) %>%
    mutate(date = ifelse(day_week == "Sunday", date + 1, date)) %>%
    mutate(date = as.Date(date, origin = "1970-01-01")) %>%
    select(date)

  # Remove NA values
  open_first_day_week <- as.data.frame(open_first_day_week[complete.cases(open_first_day_week),])
  names(open_first_day_week) <- "date"

  # Save data to be resused in options.data package
  save(open_first_day_week, file = "data/open_first_day_week.RData")
}
