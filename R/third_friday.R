# Header------------------------------------------------------------------------
# title: "Monthly expiration dates"
# author: "Jason Taylor"
# update: "Dec-21-2016"


# TODO(jason):------------------------------------------------------------------


# Function:---------------------------------------------------------------------


# Find third friday of month
third_friday <- function(start_yr, end_yr) {
  range_yr <- list(seq(start_yr, end_yr, by = 1))
  calendar <- data.frame(mon = unlist(rep(list(1:12), length(range_yr))),
                         year = sort(mapply(rep, range_yr, 12)))

  monthy_exp_date <- sapply(1:nrow(calendar),
                            function(i) format(getNthDayOfWeek(third, Fri,
                                                               calendar[i,1],
                                                               calendar[i,2])))
  monthy_exp_date <- as.Date(monthy_exp_date)
}
