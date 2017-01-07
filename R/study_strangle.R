#' Strangle options strategy
#'
#' @description{
#' strangle opens a short strangle trade that meet criteria chosen in the Shiny UI
#' }
#'
#' @export
#'
#' @importFrom dplyr bind_rows filter left_join select mutate distinct
#' @importFrom lubridate year parse_date_time
#' @importFrom TTR RSI runPercentRank
#' @importFrom data.table fread
#' @importFrom purrr map
#' @importFrom RcppBDT getNthDayOfWeek
#'
#'
#' @examples
#'
strangle <- function(progress.int, t) {
  shiny::withProgress(message = "Progress Bar", detail = "Opening Trades", {
    p_positions <- data_frame()
    c_positions <- data_frame()
    put_results <- data_frame()
    call_results <- data_frame()

    if (exists("results") && is.data.frame(get("results"))) {
      t <- max(results$trade_num)
    } else {
      results <- data_frame()
      t <- 0
    }

    min_date <- min(complete.data$date)
    max_date <- max(complete.data$date)
    trade_data <- complete.data %>%
      dplyr::filter(expiration <= max_date,
                    iv_rank_252 >= low_iv,
                    iv_rank_252 <= high_iv)
    first_day <- first_day %>%
      dplyr::filter(date >= min_date,
                    date <= (max_date - 20))

    for (i in unique(first_day$date))  {
      incProgress(progress.int)
      t <- t + 1
      start_data <- trade_data %>%
        dplyr::filter(date == i)
      if (nrow(start_data) > 0) {
        put_trade <- start_data %>%
          dplyr::filter(call.put == "P") %>%
          dplyr::mutate(dte_diff = abs(o_dte - dte)) %>%
          dplyr::filter(dte_diff == min(dte_diff),
                        delta >= p_delta,
                        delta <= p_delta_lim) %>%
          dplyr::filter(delta == min(delta)) %>%
          dplyr::mutate(put_option_margin = (100 * ((.2 * price) - (price - strike) + mid.price)))

        call_trade <- start_data %>%
          dplyr::filter(call.put == "C",
                        dte == put_trade$dte,
                        delta <= c_delta,
                        delta >= c_delta_lim) %>%
          dplyr::filter(delta == max(delta)) %>%
          dplyr::mutate(call_option_margin = (100 * ((.2 * price) - (strike - price) + mid.price)))

        if (nrow(call_trade) > 0 && nrow(put_trade) > 0)  {
          put_option_margin <- put_trade$put_option_margin
          call_option_margin <- call_trade$call_option_margin
          option_margin <- ifelse(put_option_margin > call_option_margin, put_option_margin + (100 * call_trade$mid.price),
                                  call_option_margin + (100 * put_trade$mid.price))
          opening_stock_margin <- 30 * call_trade$price

          call_trade <- dplyr::mutate(call_trade,
                                      trade_num = t,
                                      type = "call",
                                      open_stock_price = price,
                                      num_shares = round(100 * (option_margin / opening_stock_margin), digits = 0))
          put_trade <- dplyr::mutate(put_trade,
                                     trade_num = t,
                                     type = "put",
                                     open_stock_price = price,
                                     num_shares = round(100 * (option_margin / opening_stock_margin), digits = 0))
          c_positions <- dplyr::bind_rows(c_positions, call_trade)
          p_positions <- dplyr::bind_rows(p_positions, put_trade)
        }
      }
    }
  }) # End opening trades progress bar
  shiny::withProgress(message = "Progress Bar", detail = "Closing Trades", value = .5, {
    # Loop through all trades and find the results
    for (i in 1:nrow(c_positions))  {
      incProgress(progress.int)
      c_position <- c_positions[i, ]
      p_position <- p_positions[i, ]

      # Closing trades possible ----
      profit_target <- (c_position$mid.price + p_position$mid.price) * prof_targ
      loss_limit <- (c_position$mid.price + p_position$mid.price) * loss_lim
      all_possible_closes <- complete.data %>%
        dplyr::filter(date >= c_position$date,
                      date <= c_position$expiration,
                      expiration == c_position$expiration)

      # Close at expiration ----
      put_close <- all_possible_closes %>%
        dplyr::filter(date == c_position$expiration,
                      strike == p_position$strike,
                      call.put == "P") %>%
        dplyr::mutate(trade_num = c_position$trade_num,
                      exit_reason = "Expiration",
                      close_price = mid.price,
                      hold_profit = p_position$num_shares * (price - p_position$open_stock_price),
                      num_shares = p_position$num_shares)
      put_results <- dplyr::bind_rows(put_results, put_close)
      call_close <- all_possible_closes %>%
        dplyr::filter(date == c_position$expiration,
                      strike == c_position$strike,
                      call.put == "C") %>%
        dplyr::mutate(trade_num = c_position$trade_num,
                      exit_reason = "Expiration",
                      close_price = mid.price,
                      hold_profit = p_position$num_shares * (price - p_position$open_stock_price),
                      num_shares = p_position$num_shares)
      call_results <- dplyr::bind_rows(call_results, call_close)

      # Close the Strangle if the profit or loss target is met ----
      possible_call_close <- all_possible_closes %>%
        dplyr::filter(strike == c_position$strike,
                      call.put == "C")
      possible_put_close <- all_possible_closes %>%
        dplyr::filter(strike == p_position$strike,
                      call.put == "P")
      possible_close <- dplyr::bind_rows(possible_call_close, possible_put_close)
      possible_close <- possible_close %>%
        dplyr::group_by(date) %>%
        dplyr::mutate(curr_price = sum(mid.price)) %>%
        dplyr::filter(curr_price <= (c_position$mid.price + p_position$mid.price) - profit_target | curr_price >= loss_limit)
      if (nrow(possible_close) > 0) {
        possible_close <- possible_close %>%
          dplyr::filter(date == min(date)) %>%
          dplyr::mutate(trade_num = c_position$trade_num,
                        exit_reason = ifelse(curr_price <= (c_position$mid.price + p_position$mid.price) - profit_target, "Profit target", "Loss limit"),
                        close_price = ifelse(curr_price <= (c_position$mid.price + p_position$mid.price) - profit_target, ((c_position$mid.price + p_position$mid.price) - profit_target) / 2,
                                      loss_limit / 2), hold_profit = p_position$num_shares * (price - p_position$open_stock_price), num_shares = p_position$num_shares)
        put_close <- dplyr::filter(possible_close, call.put == "P")
        call_close <- dplyr::filter(possible_close, call.put == "C")
        put_results <- dplyr::bind_rows(put_results, put_close)
        call_results <- dplyr::bind_rows(call_results, call_close)
      }

      # Close by gamma days input ----
      if (g > 0) {
        gamma_date <- as.Date(c_position$expiration, origin = "1970-01-01") - g
        ifelse(weekdays(gamma_date, abbreviate = FALSE) == "Sunday",
               gamma_date <- gamma_date - 2, gamma_date)
        ifelse(weekdays(gamma_date, abbreviate = FALSE) == "Saturday",
               gamma_date <- gamma_date - 1, gamma_date)
        g_put_close <- all_possible_closes %>%
          dplyr::filter(date == gamma_date,
                        expiration == c_position$expiration,
                        strike == p_position$strike,
                        call.put == "P") %>%
          dplyr::mutate(trade_num = c_position$trade_num,
                        exit_reason = "Gamma risk",
                        close_price = mid.price,
                        hold_profit = p_position$num_shares * (price - p_position$open_stock_price),
                        num_shares = p_position$num_shares)
        put_results <- dplyr::bind_rows(put_results, g_put_close)
        g_call_close <- all_possible_closes %>%
          dplyr::filter(date == gamma_date,
                        expiration == c_position$expiration,
                        strike == c_position$strike,
                        call.put == "C") %>%
          dplyr::mutate(trade_num = c_position$trade_num,
                        exit_reason = "Gamma risk",
                        close_price = mid.price,
                        hold_profit = p_position$num_shares * (price - p_position$open_stock_price),
                        num_shares = p_position$num_shares)
        call_results <- dplyr::bind_rows(call_results, g_call_close)
      }
    }

    # Filter down results to the first exit for each trade as multiple targets could be hit ----
    put_results <- put_results %>%
      dplyr::group_by(trade_num) %>%
      dplyr::filter(rank(date, ties.method = "first") == 1) %>%
      dplyr::ungroup()
    call_results <- call_results %>%
      dplyr::group_by(trade_num) %>%
      dplyr::filter(rank(date, ties.method = "first") == 1) %>%
      dplyr::ungroup()

    # Merge the opening and closing data frames to calculate profit loss ----
    merge_call_results <- merge(call_results, c_positions, by = "trade_num")
    call_results <- merge_call_results %>%
      dplyr::mutate(profit_loss = 100 * (mid.price.y - close_price), year = year(date.y)) %>%
      dplyr::select(trade_num,
                    symbol = symbol.x,
                    expiration = expiration.x,
                    close_date = date.x,
                    call_strike = strike.x,
                    call_close_price = close_price,
                    open_date = date.y,
                    price = price.y,
                    call_open_price = mid.price.y,
                    call_delta = delta.y,
                    dte = dte.y,
                    open_rsi = rsi_14.y,
                    open_ivrank = iv_rank_252.y,
                    call_profit = profit_loss,
                    year,
                    exit_reason,
                    hold_profit,
                    num_shares = num_shares.y)
    merge_put_results <- merge(put_results, p_positions, by = "trade_num")
    put_results <- merge_put_results %>%
      dplyr::mutate(profit_loss = 100 * (mid.price.y - close_price), year = year(date.y)) %>%
      dplyr::select(trade_num,
                    symbol = symbol.x,
                    expiration = expiration.x,
             close_date = date.x,
             put_strike = strike.x,
             put_close_price = close_price,
             open_date = date.y,
             price = price.y,
             put_open_price = mid.price.y,
             put_delta = delta.y,
             dte = dte.y,
             open_rsi = rsi_14.y,
             open_ivrank = iv_rank_252.y,
             put_profit = profit_loss,
             year,
             exit_reason,
             hold_profit,
             num_shares = num_shares.y)

    # Add in commission at a rate of $1.5 per trade excluding closing for .05 or less ----
    put_results <- put_results %>%
      dplyr::mutate(put_profit = ifelse(put_close_price <= .05, put_profit - 1.5, put_profit - 3))
    call_results <- call_results %>%
      dplyr::mutate(call_profit = ifelse(call_close_price <= .05, call_profit - 1.5, call_profit - 3))

    # Combine put and call results to enable calculation of totals ----
    trade_results <- merge(put_results, call_results,
                           by = c("trade_num", "symbol", "expiration", "open_date", "close_date", "dte", "open_ivrank",
                                  "open_rsi", "exit_reason", "hold_profit", "year", "price", "num_shares"))
    trade_results <- trade_results %>%
      dplyr::mutate(open_date = as.Date(as.numeric(open_date), origin = "1970-01-01"),
             close_date = as.Date(as.numeric(close_date), origin = "1970-01-01"),
             put_credit = as.numeric(put_open_price) * 100,
             open_ivrank = as.integer(open_ivrank),
             open_rsi = as.integer(open_rsi),
             call_credit = as.numeric(call_open_price) * 100,
             profit = as.numeric(call_profit + put_profit),
             year = year(open_date),
             days_held = as.numeric(close_date) - as.numeric(open_date),
             Profitable = ifelse(profit > 0, "Yes", "No"),
             hold_profit = as.numeric(hold_profit)) %>%
      dplyr::select(symbol,
                    open_date,
                    close_date,
                    expiration,
                    dte,
                    days_held,
                    put_strike,
                    call_strike,
                    exit_reason,
                    profit,
                    hold_profit,
                    open_ivrank,
                    open_rsi,
                    put_delta,
                    put_open_price,
                    put_close_price,
                    call_delta,
                    call_open_price,
                    call_close_price,
                    Profitable,
                    num_shares)
    results <- dplyr::bind_rows(results, trade_results)
  }) # End closing trades progress bar

  shiny::withProgress(message = "Progress Bar", detail = "Creating Plot", value = .95, {
    results_table <- results %>%
      dplyr::select(-Profitable)
    assign.global(results, results_table)
  }) # End creating plot progress bar
}
