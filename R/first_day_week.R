#' find first day of week
#' @description{
#' first_day_week creates a data.frame with the first day of the week (mondays)
#' to be used in the tidy data process.  The first day of the week is used as a
#' trade entry criteria in options.shiny and options.studies.
#' }
#' @param start_yr Numeric year to begin calendar
#' @param end_yr Numeric year to end calendar
#'
#' @export
#'
#' @return data.frame \code{mondays} in the years passed to function saved as
#' "data/mondays.RData".
#'
#' @examples
#' mondays <- first_day_week(2010, 2020)

first_day_week <- function(start_yr, end_yr) {
  range_yr <- list(seq(start_yr, end_yr, by = 1))
  calendar <- data.frame(mon = unlist(rep(list(1:12), length(range_yr))),
                         year = sort(mapply(rep, range_yr, 12)))
  weeks <- c(1:5)

  find_mondays <- function(week_num) {
    sapply(1:nrow(calendar),
           function(i) format(getNthDayOfWeek(week_num, Mon,
                                              calendar[i,1],
                                              calendar[i,2])))
  }

  mondays <- map(weeks, find_mondays)
  mondays <- data.frame(unlist(mondays), stringsAsFactors = FALSE)
  colnames(mondays) <- "date"
  market_closed <- market_closed$date

  mondays <- mondays %>%
    mutate(date = as.Date(date),
           date = ifelse(date %in% market_closed, date + 1, date),
           day_week = weekdays(as.Date(date, origin = "1970-01-01"), abbreviate = FALSE),
           date = as.Date(ifelse(day_week == "Saturday", date + 2,
                                 ifelse(day_week == "Sunday", date + 1,
                                        date)), origin = "1970-01-01")) %>%
    arrange(date) %>%
    select(date) %>%
    distinct(date)

  save(mondays, file = "data/mondays.RData")
}