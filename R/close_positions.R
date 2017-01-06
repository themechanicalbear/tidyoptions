# Close rolled trades ----
close_positions <- function(option_data, positions) {
  results <- data.frame(stringsAsFactors = FALSE)
  assign("results", results, envir = parent.frame())
  for (i in 1:nrow(positions))  {
    op <- positions[i, "open_price"]
    pt <- positions[i, "profit_target"]
    ll <- positions[i, "loss_lim"]
    od <- positions[i, "date"]
    e <- positions[i, "expiration"]
    s <- positions[i, "strike"]
    j <- positions[i, "trade_num"]
    im <- positions[i, "open_margin"]

    # Close at expiration
    close_exp <- option_data %>%
      dplyr::filter(date == e, expiration == e, strike == s) %>%
      dplyr::mutate(trade_num = j, exit_reason = "Expiration",
             close_price = call_mid + put_mid,
             open_price = op,
             open_date = od,
             profit = open_price - close_price,
             open_margin = im)
    results <- dplyr::bind_rows(results, close_exp)

    # Close if the profit target is met
    close_profit <- option_data %>%
      dplyr::filter(date >= od, date <= e, expiration == e, strike == s) %>%
      dplyr::filter(call_mid + put_mid <= pt)
    if (nrow(close_profit) > 0) {
      close_profit <- close_profit %>%
        dplyr::filter(date == min(date)) %>%
        dplyr::mutate(trade_num = j, exit_reason = "Profit target", close_price = pt,
               open_price = op,
               open_date = od,
               profit = open_price - close_price,
               open_margin = im)
      results <- dplyr::bind_rows(results, close_profit)
    }

    # Close if the loss limit is met
    close_loss <- option_data %>%
      dplyr::filter(date >= od, date <= e, expiration == e, strike == s) %>%
      dplyr::filter(call_mid + put_mid >= ll)
    if (nrow(close_loss) > 0) {
      close_loss <- close_loss %>%
        dplyr::filter(date == min(date)) %>%
        dplyr::mutate(trade_num = j, exit_reason = "Loss limit", close_price = ll,
               open_price = op,
               open_date = od,
               profit = open_price - close_price,
               open_margin = im)
      results <- dplyr::bind_rows(results, close_loss)
    }
  }
  assign("results", results, envir = parent.frame())
}