# Description:
# - Use this script to recreate daily file for trade entry

# Instructions:
# - Update the market closed dates prior to running
# - Calendar is created through 2020 update if needed

comment.for.build <- TRUE
if (comment.for.build == FALSE) {
# Dates the market was closed and can'te be used in studies
closed.dates <- c("2010-01-01", "2010-01-18","2010-02-15", "2010-04-02",
                  "2010-05-31", "2010-07-05", "2010-09-06", "2010-11-25",
                  "2010-12-24", "2011-01-17", "2011-02-21", "2011-04-22",
                  "2011-05-30", "2011-07-04", "2011-09-05", "2011-11-24",
                  "2011-12-26", "2012-01-02", "2012-01-16", "2012-02-20",
                  "2012-04-06", "2012-05-28", "2012-07-04", "2012-09-03",
                  "2012-11-22", "2012-12-25", "2013-01-01", "2013-01-21",
                  "2013-02-18", "2013-03-29", "2013-05-27", "2013-07-04",
                  "2013-09-02", "2013-11-28", "2013-12-25", "2014-01-01",
                  "2014-01-20", "2014-02-17", "2014-04-18", "2014-05-26",
                  "2014-07-04", "2014-09-11", "2014-11-27", "2014-12-25",
                  "2015-01-19", "2015-02-16", "2015-05-25", "2015-07-03",
                  "2015-09-07", "2015-11-26")
closed.dates <- as.Date(closed.dates)

# Create first day of the week data file for use on trade entry

all.days <- function(year, year.end){
  seq(as.Date(paste0(year, "-01-01")), as.Date(paste0(year.end, "-12-31")),
      by = "+1 day")
}

daily <- as.data.frame(all.days("2010", "2020"))

names(daily) <- "date"

open.daily <- dplyr::filter(daily, weekdays(daily$date, abbreviate = FALSE) != "Saturday")
open.daily <- dplyr::filter(open.daily, weekdays(open.daily$date, abbreviate = FALSE) != "Sunday")

# If date is a market closed date remove
open.daily <- dplyr::filter(open.daily, !date %in% closed.dates)

# Save data to be resused in options.data package
save(open.daily, file = "data/open.daily.RData")
}

