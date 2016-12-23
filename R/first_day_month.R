#' find first day of month
#' @description{
#' first_day_month creates a data.frame with the first day of the month
#' to be used in the tidy data process.  The first day of the month is used as a
#' trade entry criteria in options.shiny and options.studies.
#' }
#' @param start_yr Numeric year to begin calendar
#' @param end_yr Numeric year to end calendar
#'
#' @export
#'
#' @importFrom dplyr arrange select distinct mutate
#' @importFrom lubridate mdy
#' @importFrom magrittr %>%
#'
#' @return data.frame \code{monthly} in the years passed to function saved as
#' "data/monthly.RData".
#'
#' @examples
#' monthly <- monthly(2010, 2020)
#'

first_day_month <- function(start_yr, end_yr) {
  market_closed <- market_closed$date

  range_yr <- list(seq(start_yr, end_yr, by = 1))
  calendar <- data.frame(day = 1,
                         mon = unlist(rep(list(1:12), length(range_yr))),
                         year = sort(mapply(rep, range_yr, 12)))
  monthly <- calendar %>%
    mutate(date = as.Date(mdy(paste0(mon, "-", day, "-", year))),
           date = ifelse(date %in% market_closed, date + 1, date),
           day_week = weekdays(as.Date(date, origin = "1970-01-01"), abbreviate = FALSE),
           date = as.Date(ifelse(day_week == "Saturday", date + 2,
                                 ifelse(day_week == "Sunday", date + 1,
                                        date)), origin = "1970-01-01")) %>%
    arrange(date) %>%
    select(date) %>%
    distinct(date)

  save(monthly, file = "data/monthly.RData")
}