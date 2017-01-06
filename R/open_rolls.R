# Function----------------------------------------------------------------------
# Open rolled trades
open_rolls <- function(option_data, x) {
  positions <- data.frame(stringsAsFactors = FALSE)
  assign("positions", positions, envir = parent.frame())
  for (i in 1:nrow(x))  {
    cd <- x[i, "close_date"]
    s <- x[i, "strike"]
    t <- x[i, "trade_num"]
    op <- x[i, "open_price"]
    p <- x[i, "profit"]

    trade_data <- option_data %>%
      dplyr::filter(date == cd)
    if (nrow(trade_data) > 0) {
      trade <- trade_data %>%
        dplyr::mutate(dte_diff = abs(o_dte - dte)) %>%
        dplyr::filter(dte_diff == min(dte_diff)) %>%
        dplyr::filter(dte == max(dte)) %>%
        dplyr::filter(strike == s) %>%
        dplyr::mutate(trade_num = t, open_price = (call_mid + put_mid),
               profit_target = open_price + p - (op / 4),
               loss_lim = 3 * (open_price + p),
               call_marg = 100 * (call_mid + (price / 5)),
               put_marg = 100 * (put_mid + (price / 5)) - (price - strike),
               open_margin = ifelse(call_marg > put_marg, call_marg + put_mid, put_marg + call_mid))
      positions <- dplyr::bind_rows(positions, trade)
    }
  }
  assign("positions", positions, envir = parent.frame())
}