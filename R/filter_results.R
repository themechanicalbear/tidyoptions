# Filter down results to the first exit ----
filter_results <- function(x) {
  x <- x %>%
    dplyr::group_by(trade_num) %>%
    dplyr::filter(rank(date, ties.method = "first") == 1) %>%
    dplyr::mutate(profit = profit) %>%
    dplyr::ungroup()

  # Add in commission at a rate of $1.5 per trade excluding closing for <= .05
  x <- x %>%
    dplyr::mutate(profit = ifelse(put_mid > .05 & call_mid > .05, profit - .06,
                           ifelse(put_mid > .05 | call_mid > .05, profit - .045,
                                  profit - .03)),
           days_held = as.numeric(date) - as.numeric(open_date),
           open_price = open_price,
           close_price = close_price)

  refcols <- c("trade_num", "open_date", "expiration", "date", "price", "strike",
               "dte", "put_mid", "call_mid", "exit_reason",  "days_held",
               "open_price", "close_price", "profit", "open_margin")

  x <- x[, c(refcols, setdiff(names(x), refcols))]

  colnames(x) <- c("trade_num", "open_date", "expiration", "close_date",
                   "price", "strike", "dte", "put_mid", "call_mid",
                   "exit_reason", "days_held", "open_price", "close_price",
                   "profit", "open_margin")

  x <- dplyr::select(x, -c(put_mid, call_mid, dte))

  x <- as.data.frame(x)

  assign("results", x, envir = parent.frame())

  df <- x %>%
    dplyr::filter(profit < 0)

  assign("results_loss", df, envir = parent.frame())
}