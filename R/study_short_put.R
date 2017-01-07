#' Short put options strategy
#'
#' @description{
#' short_put opens a short put trade that meet criteria chosen in the Shiny UI from shinyoptions
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
short_put <- function(progress.int, t) {
  shiny::withProgress(message = "Progress Bar", detail = "Opening Trades", {
    p_positions <- data_frame()
    results <- data_frame()
    min_date <- min(complete.data$date)
    max_date <- max(complete.data$date)
    first_day <- dplyr::filter(first_day, date >= min_date & date <= (max_date - 20))
    trade_data <- dplyr::filter(complete.data, call.put == "P" & expiration <= max_date &
                           iv_rank_252 >= low_iv & iv_rank_252 <= high_iv)
    # Open trades loop
   for (i in unique(first_day$date))  {
      #incProgress(progress.int)
      t = t + 1
      start_data <- dplyr::filter(trade_data, date == i & delta >= p_delta)
      if (nrow(start_data) > 0) {
        short_put_trade <- start_data %>%
          dplyr::mutate(dte_diff = abs(o_dte - dte)) %>%
          dplyr::filter(dte_diff == min(dte_diff)) %>%
          dplyr::filter(delta == min(delta)) %>%
          dplyr::mutate(option_margin = calc_mrgn(strat = "short_put", prc = price, spstrk = strike,
                                           spcred = mid.price, qty = 1, shares = 100)) %>%
          dplyr::mutate(open_roc = 100 * (((100 * mid.price) / option_margin)), trade_num = t)
        if (nrow(short_put_trade) > 0) {
          opening_stock_margin = 30 * short_put_trade$price
          short_put_trade <- short_put_trade %>%
            dplyr::mutate(opening_price = price,
                   num_shares = round(100 * (option_margin / opening_stock_margin), digits = 0))
        p_positions <- dplyr::bind_rows(p_positions, short_put_trade)
        }
      }
    }
  }) # End opening trades progress bar
  shiny::withProgress(message = "Progress Bar", detail = "Closing Trades", value = .5, {
    # Loop through all trades and find the results
   for (i in 1:nrow(p_positions))  {
      #shiny::incProgress(progress.int)
      # p <- p_positions[i, mid.price]
      # o <- p_positions[i, date]
      # e <- p_positions[i, expiration]
      # s <- p_positions[i, strike]
      # j <- p_positions[i, trade_num]
      # ns <- p_positions[i, num_shares]
      # osp <- p_positions[i, opening_price]
     this_row <- p_positions[i, ]
     p <- this_row$mid.price
     o <- this_row$date
     e <- this_row$expiration
     s <- this_row$strike
     j <- this_row$trade_num
     ns <- this_row$num_shares
     osp <- this_row$opening_price

      xc <- p - (p * prof_targ)
      xl <- p * loss_lim

      all_possible_closes <- complete.data %>%
        dplyr::filter(date >= o, date <= e, expiration == e, call.put == "P", strike == s)

      # Close the trade if the profit or loss target is met ----
      close <- all_possible_closes %>%
        dplyr::filter(mid.price <= xc | mid.price >= xl) %>%
        dplyr::filter(date == min(date))
      if (nrow(close) > 0) {
        close <- close %>%
          dplyr::mutate(trade_num = j, open_date = o, exit_reason = ifelse(mid.price <= xc, "Profit target", "Loss limit"),
                 hold_profit = ns * (price - osp), mid.price = ifelse(mid.price > xl, xl, ifelse(mid.price < xc, xc, mid.price)))
      results <- dplyr::bind_rows(results, close)
      }

      # Close by gamma days input
      if (g > 0) {
        gamma_date <- as.Date(e, origin = "1970-01-01") - g
        ifelse(weekdays(gamma_date, abbreviate = FALSE) == "Sunday",
               gamma_date <- gamma_date - 2, gamma_date)
        ifelse(weekdays(gamma_date, abbreviate = FALSE) == "Saturday",
               gamma_date <- gamma_date - 1, gamma_date)
        g_close <- all_possible_closes %>%
          dplyr::filter(date == gamma_date)
        if (nrow(g_close) > 0) {
          g_close <- g_close %>%
            dplyr::mutate(trade_num = j, open_date = o, exit_reason = "Gamma risk", hold_profit = ns * (price - osp))
          results <- dplyr::bind_rows(results, g_close)
        }
      }
      # Find the close at expiration
      exp_close <- all_possible_closes %>%
        dplyr::filter(date == e) %>%
        dplyr::mutate(trade_num = j, open_date = o, exit_reason = "Expiration", hold_profit = ns * (price - osp))
      results <- dplyr::bind_rows(results, exp_close)
    } # End loop through all trades and find the results

    # Filter down results to the first exit for each trade as multiple targets could be hit
    results <- dplyr::group_by(results, open_date)
    results <- dplyr::filter(results, rank(date, ties.method = "first") == 1)
    results <- dplyr::ungroup(results)

    # Merge the opening and closing data frames to calculate profit loss
    results <- results %>%
      dplyr::left_join(p_positions, by = c("trade_num", "expiration")) %>%
      dplyr::mutate(profit_loss = 100 * (mid.price.y - mid.price.x),
                    year = year(date.y)) %>%
      dplyr::select(expiration, trade_num, close_date = date.x, put_strike = strike.x, close_price = mid.price.x,
                    close_rsi = rsi_14.x, close_ivrank = iv_rank_252.x, open_date = date.y, price = price.y, open_price = mid.price.y,
                    delta = delta.y, dte = dte.y, open_rsi = rsi_14.y, open_ivrank = iv_rank_252.y, exp_type = exp_type.y,
                    profit_loss, year, exit_reason, option_margin, open_roc, hold_profit)

    # Add in commission at a rate of $1.5 per trade excluding closing <= .05
    results <- results %>%
      dplyr::mutate(profit = ifelse(close_price <= .05, profit_loss - 1.5, profit_loss - 3),
                      #calc_comm(strat = "short_put", close = close_price, profit = profit_loss),
                    open_roc = round(open_roc, digits = 2),
                    open_ivrank = round(open_ivrank, digits = 0),
                    call_strike = "NA",
                    days_held = as.numeric(close_date) - as.numeric(open_date),
                    Profitable = ifelse(profit > 0, "Yes", "No"))
  }) # End closing trades progress bar
  shiny::withProgress(message = "Progress Bar", detail = "Creating Plot", value = .95, {
    # select fields we want displayed in the table view
    results_table <- dplyr::select(results, open_date, close_date, expiration, put_strike, dte, days_held, open_ivrank,
                                exit_reason, profit, option_margin, open_roc)
    colnames(results_table) <- c("Open Date", "Close Date", "Expiration", "Put Strike", "DTE", "Days Held", "Open IVRank",
                                "Exit Reason", "Profit", "Option Margin", "Open ROC")
    assign.global(results, results_table)
    }) # End creating plot progress bar
}