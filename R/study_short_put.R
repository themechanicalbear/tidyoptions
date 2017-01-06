# Function----------------------------------------------------------------------
short.put <- function(progress.int, t) {
  shiny::withProgress(message = "Progress Bar", detail = "Opening Trades", {
    p.positions <- data.frame()
    results <- data.frame()
    min.date <- min(complete.data$date)
    max.date <- max(complete.data$date)
    first.day <- dplyr::filter(first.day, date >= min.date & date <= (max.date - 20))
    trade.data <- dplyr::filter(complete.data, call.put == "P" & expiration <= max.date &
                           iv.rank >= low.iv & iv.rank <= high.iv)
    # Open trades loop
   for (i in unique(first.day$date))  {
      incProgress(progress.int)
      t = t + 1
      start.data <- dplyr::filter(trade.data, date == i & delta >= p.delta)
      if (nrow(start.data) > 0) {
        short.put.trade <- start.data %>%
          dplyr::mutate(dte.diff = abs(o.dte - dte)) %>%
          dplyr::filter(dte.diff == min(dte.diff)) %>%
          dplyr::filter(delta == min(delta)) %>%
          dplyr::mutate(option.margin = calc_mrgn(strat = "short_put", prc = price, spstrk = strike,
                                           spcred = mid.price, qty = 1, shares = 100)) %>%
          dplyr::mutate(open.roc = 100 * (((100 * mid.price) / option.margin)), trade.num = t)
        if (nrow(short.put.trade) > 0) {
          opening.stock.margin = 30 * short.put.trade$price
          short.put.trade <- short.put.trade %>%
            dplyr::mutate(opening.price = price,
                   num.shares = round(100 * (option.margin / opening.stock.margin), digits = 0))
        p.positions <- dplyr::bind_rows(p.positions, short.put.trade, fill = TRUE)
        }
      }
    }
  }) # End opening trades progress bar
  shiny::withProgress(message = "Progress Bar", detail = "Closing Trades", value = .5, {
    # Loop through all trades and find the results
   for (i in 1:nrow(p.positions))  {
      shiny::incProgress(progress.int)
      p <- p.positions[i, mid.price]
      o <- p.positions[i, date]
      e <- p.positions[i, expiration]
      s <- p.positions[i, strike]
      j <- p.positions[i, trade.num]
      ns <- p.positions[i, num.shares]
      osp <- p.positions[i, opening.price]
      xc <- p - (p * prof.targ)
      xl <- p * loss.lim

      all.possible.closes <- complete.data %>%
        dplyr::filter(date >= o, date <= e, expiration == e, call.put == "P", strike == s)

      # Close the trade if the profit or loss target is met ----
      close <- all.possible.closes %>%
        dplyr::filter(mid.price <= xc | mid.price >= xl) %>%
        dplyr::filter(date == min(date))
      if (nrow(close) > 0) {
        close <- close %>%
          dplyr::mutate(trade.num = j, open.date = o, exit.reason = ifelse(mid.price <= xc, "Profit target", "Loss limit"),
                 hold.profit = ns * (price - osp), mid.price = ifelse(mid.price > xl, xl, ifelse(mid.price < xc, xc, mid.price)))
      results <- dplyr::bind_rows(results, close, fill = TRUE)
      }

      # Close by gamma days input
      if (g > 0) {
        gamma.date <- as.Date(e, origin = "1970-01-01") - g
        ifelse(weekdays(gamma.date, abbreviate = FALSE) == "Sunday",
               gamma.date <- gamma.date - 2, gamma.date)
        ifelse(weekdays(gamma.date, abbreviate = FALSE) == "Saturday",
               gamma.date <- gamma.date - 1, gamma.date)
        g.close <- all.possible.closes %>%
          dplyr::filter(date == gamma.date)
        if (nrow(g.close) > 0) {
          g.close <- g.close %>%
            dplyr::mutate(trade.num = j, open.date = o, exit.reason = "Gamma risk", hold.profit = ns * (price - osp))
          results <- dplyr::bind_rows(results, g.close, fill = TRUE)
        }
      }
      # Find the close at expiration
      exp.close <- all.possible.closes %>%
        dplyr::filter(date == e) %>%
        dplyr::mutate(trade.num = j, open.date = o, exit.reason = "Expiration", hold.profit = ns * (price - osp))
      results <- dplyr::bind_rows(results, exp.close, fill = TRUE)
    } # End loop through all trades and find the results

    # Filter down results to the first exit for each trade as multiple targets could be hit
    ord.results <- dplyr::group_by(results, open.date)
    results <- dplyr::filter(ord.results, rank(date, ties.method = "first") == 1)

    # Merge the opening and closing data frames to calculate profit loss
    merge.results <- merge(results, p.positions, c("trade.num", "expiration"))
    results <- dplyr::mutate(merge.results, profit.loss = 100 * (mid.price.y - mid.price.x))
    results <- dplyr::mutate(results, year = year(date.y))
    results <- dplyr::select(results, expiration, trade.num, close.date = date.x, put.strike = strike.x, close.price = mid.price.x,
                      close.rsi = rsi.14.x, close.ivrank = iv.rank.x, open.date = date.y, price = price.y, open.price = mid.price.y,
                      delta = delta.y, dte = dte.y, open.rsi = rsi.14.y, open.ivrank = iv.rank.y, exp_type = exp_type.y,
                      profit = profit.loss, year, exit.reason, option.margin, open.roc, hold.profit)

    # Add in commission at a rate of $1.5 per trade excluding closing <= .05
    results <- dplyr::mutate(results, profit = calc_comm(strat = "short_put", close = close.price, profit = profit),
                      open.roc = round(open.roc, digits = 2),
                      open.ivrank = round(open.ivrank, digits = 0),
                      call.strike = "NA",
                      days.held = as.numeric(close.date) - as.numeric(open.date),
                      Profitable = ifelse(profit > 0, "Yes", "No"))
  }) # End closing trades progress bar
  shiny::withProgress(message = "Progress Bar", detail = "Creating Plot", value = .95, {
    # select fields we want displayed in the table view
    results.table <- dplyr::select(results, open.date, close.date, expiration, put.strike, dte, days.held, open.ivrank,
                                exit.reason, profit, option.margin, open.roc)
    colnames(results.table) <- c("Open Date", "Close Date", "Expiration", "Put Strike", "DTE", "Days Held", "Open IVRank",
                                "Exit Reason", "Profit", "Option Margin", "Open ROC")
    assign.global(results, results.table)
    }) # End creating plot progress bar
}