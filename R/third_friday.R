#' Find third friday of month
#' @description{
#' third_friday creates a data.frame with options expiration dates to be used in
#' the tidy data process.  The third friday date identifies monthly expirations
#' compared to weeklys.
#' }
#' @param start_yr Numeric year to begin third friday  calendar
#' @param end_yr Numeric year to end third friday calendar
#'
#' @return data.frame \code{monthly_exp_date} with the third friday of each month
#' when options expire.
#'
#' @importFrom RcppBDT getNthDayOfWeek
#'
#' @export
#'
#' @examples
#' third_friday(2010, 2015)

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
