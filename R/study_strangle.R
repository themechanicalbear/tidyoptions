# Todos:
# Update colnames(results.table) at bottom
# options margin calculation?
# change for loops to seq_along to handle the null data.frame better
# functions should only depend on the values passed to them

strangle <- function(progress.int, t) {
  shiny::withProgress(message = "Progress Bar", detail = "Opening Trades", {
    p.positions <- data.frame()
    c.positions <- data.frame()
    put.results <- data.frame()
    call.results <- data.frame()

    if (exists("results") && is.data.frame(get("results"))) {
      t <- max(results$trade.num)
    } else {
      results <- data.frame()
      t <- 0
    }

    min.date <- min(complete.data$date)
    max.date <- max(complete.data$date)
    trade.data <- dplyr::filter(complete.data, expiration <= max.date, iv.rank >= low.iv, iv.rank <= high.iv)
    first.day <- dplyr::filter(first.day, date >= min.date, date <= (max.date - 20))

    for (i in unique(first.day$date))  {
      incProgress(progress.int)
      t <- t + 1
      start.data <- dplyr::filter(trade.data, date == i)
      if (nrow(start.data) > 0) {
        put.trade <- start.data %>%
          dplyr::filter(call.put == "P") %>%
          dplyr::mutate(dte.diff = abs(o.dte - dte)) %>%
          dplyr::filter(dte.diff == min(dte.diff), delta >= p.delta, delta <= p.delta.lim) %>%
          dplyr::filter(delta == min(delta)) %>%
          dplyr::mutate(put.option.margin = (100 * ((.2 * price) - (price - strike) + mid.price)))

        call.trade <- start.data %>%
          dplyr::filter(call.put == "C", dte == put.trade$dte, delta <= c.delta, delta >= c.delta.lim) %>%
          dplyr::filter(delta == max(delta)) %>%
          dplyr::mutate(call.option.margin = (100 * ((.2 * price) - (strike - price) + mid.price)))

        if (nrow(call.trade) > 0 && nrow(put.trade) > 0)  {
          put.option.margin <- put.trade$put.option.margin
          call.option.margin <- call.trade$call.option.margin
          option.margin <- ifelse(put.option.margin > call.option.margin, put.option.margin + (100 * call.trade$mid.price),
                                  call.option.margin + (100 * put.trade$mid.price))
          opening.stock.margin <- 30 * call.trade$price

          call.trade <- dplyr::mutate(call.trade, trade.num = t, type = "call", open.stock.price = price,
                               num.shares = round(100 * (option.margin / opening.stock.margin), digits = 0))
          put.trade <- dplyr::mutate(put.trade, trade.num = t, type = "put", open.stock.price = price,
                              num.shares = round(100 * (option.margin / opening.stock.margin), digits = 0))
          c.positions <- dplyr::bind_rows(c.positions, call.trade)
          p.positions <- dplyr::bind_rows(p.positions, put.trade)
        }
      }
    }
  }) # End opening trades progress bar
  shiny::withProgress(message = "Progress Bar", detail = "Closing Trades", value = .5, {
    # Loop through all trades and find the results
    for (i in 1:nrow(c.positions))  {
      incProgress(progress.int)
      ocp <- c.positions[i, mid.price]
      opp <- p.positions[i, mid.price]
      od <- c.positions[i, date]
      e <- c.positions[i, expiration]
      cs <- c.positions[i, strike]
      ps <- p.positions[i, strike]
      j <- c.positions[i, trade.num]
      ns <- p.positions[i, num.shares]
      osp <- p.positions[i, open.stock.price]

      # Closing trades possible ----
      profit.target <- (ocp + opp) * prof.targ
      loss.limit <- (ocp + opp) * loss.lim
      all.possible.closes <- complete.data %>%
        dplyr::filter(date >= od, date <= e, expiration == e)

      # Close at expiration ----
      put.close <- all.possible.closes %>%
        dplyr::filter(date == e, strike == ps, call.put == "P") %>%
        dplyr::mutate(trade.num = j, exit.reason = "Expiration", close.price = mid.price, hold.profit = ns * (price - osp),
               num.shares = ns)
      put.results <- dplyr::bind_rows(put.results, put.close, fill = TRUE)
      call.close <- all.possible.closes %>%
        dplyr::filter(date == e, strike == cs, call.put == "C") %>%
        dplyr::mutate(trade.num = j, exit.reason = "Expiration", close.price = mid.price, hold.profit = ns * (price - osp),
               num.shares = ns)
      call.results <- dplyr::bind_rows(call.results, call.close, fill = TRUE)

      # Close the Strangle if the profit or loss target is met ----
      possible.call.close <- all.possible.closes %>%
        dplyr::filter(strike == cs, call.put == "C")
      possible.put.close <- all.possible.closes %>%
        dplyr::filter(strike == ps, call.put == "P")
      possible.close <- dplyr::bind_rows(possible.call.close, possible.put.close, fill = TRUE)
      possible.close <- possible.close %>%
        dplyr::group_by(date) %>%
        dplyr::mutate(curr.price = sum(mid.price)) %>%
        dplyr::filter(curr.price <= (ocp + opp) - profit.target | curr.price >= loss.limit)
      if (nrow(possible.close) > 0) {
        possible.close <- possible.close %>%
          dplyr::filter(date == min(date)) %>%
          dplyr::mutate(trade.num = j, exit.reason = ifelse(curr.price <= (ocp + opp) - profit.target, "Profit target", "Loss limit"),
                 close.price = ifelse(curr.price <= (ocp + opp) - profit.target, ((ocp + opp) - profit.target) / 2,
                                      loss.limit / 2), hold.profit = ns * (price - osp), num.shares = ns)
        put.close <- dplyr::filter(possible.close, call.put == "P")
        call.close <- dplyr::filter(possible.close, call.put == "C")
        put.results <- dplyr::bind_rows(put.results, put.close, fill = TRUE)
        call.results <- dplyr::bind_rows(call.results, call.close, fill = TRUE)
      }

      # Close by gamma days input ----
      if (g > 0) {
        gamma.date <- as.Date(e, origin = "1970-01-01") - g
        ifelse(weekdays(gamma.date, abbreviate = FALSE) == "Sunday",
               gamma.date <- gamma.date - 2, gamma.date)
        ifelse(weekdays(gamma.date, abbreviate = FALSE) == "Saturday",
               gamma.date <- gamma.date - 1, gamma.date)
        g.put.close <- all.possible.closes %>%
          dplyr::filter(date == gamma.date, expiration == e, strike == ps, call.put == "P") %>%
          dplyr::mutate(trade.num = j, exit.reason = "Gamma risk", close.price = mid.price, hold.profit = ns * (price - osp),
                 num.shares = ns)
        put.results <- dplyr::bind_rows(put.results, g.put.close, fill = TRUE)
        g.call.close <- all.possible.closes %>%
          dplyr::filter(date == gamma.date, expiration == e, strike == cs, call.put == "C") %>%
          dplyr::mutate(trade.num = j, exit.reason = "Gamma risk", close.price = mid.price, hold.profit = ns * (price - osp),
                 num.shares = ns)
        call.results <- dplyr::bind_rows(call.results, g.call.close, fill = TRUE)
      }
    }

    # Filter down results to the first exit for each trade as multiple targets could be hit ----
    put.results <- put.results %>%
      dplyr::group_by(trade.num) %>%
      dplyr::filter(rank(date, ties.method = "first") == 1) %>%
      dplyr::ungroup()
    call.results <- call.results %>%
      dplyr::group_by(trade.num) %>%
      dplyr::filter(rank(date, ties.method = "first") == 1) %>%
      dplyr::ungroup()

    # Merge the opening and closing data frames to calculate profit loss ----
    merge.call.results <- merge(call.results, c.positions, by = "trade.num")
    call.results <- merge.call.results %>%
      dplyr::mutate(profit.loss = 100 * (mid.price.y - close.price), year = year(date.y)) %>%
      dplyr::select(trade.num, symbol = symbol.x, expiration = expiration.x,
             close.date = date.x, call.strike = strike.x, call.close.price = close.price,
             open.date = date.y, price = price.y, call.open.price = mid.price.y, call.delta = delta.y, dte = dte.y,
             open.rsi = rsi.14.y, open.ivrank = iv.rank.y, call.profit = profit.loss,
             year, exit.reason, hold.profit, num.shares = num.shares.y)
    merge.put.results <- merge(put.results, p.positions, by = "trade.num")
    put.results <- merge.put.results %>%
      dplyr::mutate(profit.loss = 100 * (mid.price.y - close.price), year = year(date.y)) %>%
      dplyr::select(trade.num, symbol = symbol.x, expiration = expiration.x,
             close.date = date.x, put.strike = strike.x, put.close.price = close.price,
             open.date = date.y, price = price.y, put.open.price = mid.price.y, put.delta = delta.y, dte = dte.y,
             open.rsi = rsi.14.y, open.ivrank = iv.rank.y, put.profit = profit.loss,
             year, exit.reason, hold.profit, num.shares = num.shares.y)

    # Add in commission at a rate of $1.5 per trade excluding closing for .05 or less ----
    put.results <- put.results %>%
      dplyr::mutate(put.profit = ifelse(put.close.price <= .05, put.profit - 1.5, put.profit - 3))
    call.results <- call.results %>%
      dplyr::mutate(call.profit = ifelse(call.close.price <= .05, call.profit - 1.5, call.profit - 3))

    # Combine put and call results to enable calculation of totals ----
    trade.results <- merge(put.results, call.results,
                           by = c("trade.num", "symbol", "expiration", "open.date", "close.date", "dte", "open.ivrank",
                                  "open.rsi", "exit.reason", "hold.profit", "year", "price", "num.shares"))
    trade.results <- trade.results %>%
      dplyr::mutate(open.date = as.Date(as.numeric(open.date), origin = "1970-01-01"),
             close.date = as.Date(as.numeric(close.date), origin = "1970-01-01"),
             put.credit = as.numeric(put.open.price) * 100,
             open.ivrank = as.integer(open.ivrank),
             open.rsi = as.integer(open.rsi),
             call.credit = as.numeric(call.open.price) * 100,
             profit = as.numeric(call.profit + put.profit),
             year = year(open.date),
             days.held = as.numeric(close.date) - as.numeric(open.date),
             Profitable = ifelse(profit > 0, "Yes", "No"),
             hold.profit = as.numeric(hold.profit)) %>%
      dplyr::select(symbol, open.date, close.date, expiration, dte, days.held, put.strike, call.strike, exit.reason, profit,
             hold.profit, open.ivrank, open.rsi, put.delta, put.open.price, put.close.price, call.delta, call.open.price,
             call.close.price, Profitable, num.shares)
    results <- dplyr::bind_rows(results, trade.results)
  }) # End closing trades progress bar

  shiny::withProgress(message = "Progress Bar", detail = "Creating Plot", value = .95, {
    results.table <- results %>%
      dplyr::select(-Profitable)
    assign.global(results, results.table)
  }) # End creating plot progress bar
}
