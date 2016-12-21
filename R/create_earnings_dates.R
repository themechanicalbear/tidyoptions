# Header------------------------------------------------------------------------
# title: "Earnings dates"
# author: "Jason Taylor"
# Update: "Dec-20-2016"


# TODO(jason):------------------------------------------------------------------


# Function----------------------------------------------------------------------
comment.for.build <- TRUE
if (comment.for.build == FALSE) {
  # AMZN earnings dates
  AMZN.earnings.dates <- as.data.frame(matrix(c("2010-07-22", "2010-10-21",
                                                "2011-01-27", "2011-04-26",
                                                "2011-07-26", "2011-10-25",
                                                "2012-01-31", "2012-04-26",
                                                "2012-07-26", "2012-10-26",
                                                "2013-01-29", "2013-04-25",
                                                "2013-07-26", "2013-10-24",
                                                "2014-01-30", "2014-04-24",
                                                "2014-07-24", "2014-10-23",
                                                "2015-01-29", "2015-04-23",
                                                "2015-07-23", "2015-10-22")),
                                       stringsAsFactors = FALSE)
  names(AMZN.earnings.dates) <- "date"
  earnings.dates <- mutate(AMZN.earnings.dates,
                                  date = as.Date(date, origin = "1970-01-01"))

  save(earnings.dates, file = "data/earnings.dates.AMZN.RData")
  rm(earnings.dates)

  # GOOG earnings dates
  GOOG.earnings.dates <- as.data.frame(matrix(c("2010-07-15", "2010-10-14",
                                                "2011-01-20", "2011-04-14",
                                                "2011-07-14", "2011-10-13",
                                                "2012-01-19", "2012-04-12",
                                                "2012-07-19", "2012-10-17",
                                                "2013-01-22", "2013-04-18",
                                                "2013-07-18", "2013-10-17",
                                                "2014-01-30", "2014-04-16",
                                                "2014-07-17", "2014-10-16",
                                                "2015-01-29", "2015-04-23",
                                                "2015-07-16", "2015-10-22")),
                                       stringsAsFactors = FALSE)
  names(GOOG.earnings.dates) <- "date"
  earnings.dates <- mutate(GOOG.earnings.dates,
                                  date = as.Date(date, origin = "1970-01-01"))

  save(earnings.dates, file = "data/earnings.dates.GOOG.RData")
  rm(earnings.dates)

  # GS earnings dates
  GS.earnings.dates <- as.data.frame(matrix(c("2010-01-21", "2010-04-20",
                                              "2010-07-20", "2010-10-19",
                                              "2011-01-19", "2011-04-19",
                                              "2011-07-19", "2011-10-18",
                                              "2012-01-18", "2012-04-17",
                                              "2012-07-17", "2012-10-16",
                                              "2013-01-16", "2013-04-16",
                                              "2013-07-16", "2013-10-17",
                                              "2014-01-16", "2014-04-17",
                                              "2014-07-15", "2014-10-16",
                                              "2015-01-16", "2015-04-16",
                                              "2015-07-16", "2015-10-15",
                                              "2016-01-20")),
                                     stringsAsFactors = FALSE)
  names(GS.earnings.dates) <- "date"
  earnings.dates <- mutate(GS.earnings.dates,
                                  date = as.Date(date, origin = "1970-01-01"))

  save(earnings.dates, file = "data/earnings.dates.GS.RData")
  rm(earnings.dates)

  # IBM earnings dates
  IBM.earnings.dates <- as.data.frame(matrix(c("2010-01-19", "2010-04-19",
                                               "2010-07-19", "2010-10-18",
                                               "2011-01-18", "2011-04-19",
                                               "2011-07-18", "2011-10-17",
                                               "2012-01-19", "2012-04-17",
                                               "2012-07-18", "2012-10-16",
                                               "2013-01-22", "2013-04-18",
                                               "2013-07-17", "2013-10-16",
                                               "2014-01-21", "2014-04-16",
                                               "2014-07-17", "2014-10-20",
                                               "2015-01-20", "2015-04-20",
                                               "2015-07-20", "2015-10-19")),
                                      stringsAsFactors = FALSE)
  names(IBM.earnings.dates) <- "date"
  earnings.dates <- mutate(IBM.earnings.dates,
                                  date = as.Date(date, origin = "1970-01-01"))

  save(earnings.dates, file = "data/earnings.dates.IBM.RData")
  rm(earnings.dates)

}