# title: "Strangle Study close everyday and plot results"
# author: "Jason Taylor"

# Todos:
# - plot(aggregate(results[, 34], list(results$days.held), mean)  Interesting plot!
# - Change the custom open dates from csv to save maybe?
# - Add commissions to results, currently commented out

strangle.daily.close <- function(progress.int, t) {
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

    # Set the first possible opening date based on the stock loaded
    min.date <- min(complete.data$date)

    # Set the last day for expiration dates to keep us from opening trades
    # we can't close
    max.date <- max(complete.data$date)
    trade.data <- dplyr::filter(complete.data, expiration <= max.date)
    trade.data <- unique(trade.data)

    # Filter first day month/week data to dates we have options chains for
    first.day <- dplyr::filter(first.day, date >= min.date)

    for (i in unique(first.day$date))  {
      incProgress(progress.int)
      t <- t + 1
      start.data <- dplyr::filter(trade.data, date == i) # Set open date
      # Check for data set and that iv rank is within range
      if (nrow(start.data) > 0 && start.data[1, iv.rank] >= low.iv &&
          start.data[1, iv.rank] <= high.iv) {
        put.data <- dplyr::filter(start.data, call.put == "P")
        put.data <- dplyr::mutate(put.data, dte.diff = abs(o.dte - dte))
        # Closest to DTE
        put.data <- dplyr::filter(put.data, dte.diff == min(dte.diff))
        put.data <- dplyr::filter(put.data, delta >= p.delta)
        put.data <- dplyr::filter(put.data, delta <= p.delta.lim)
        put.trade <- dplyr::filter(put.data, delta == min(delta))

        call.data <- dplyr::filter(start.data, call.put == "C")
        call.data <- dplyr::filter(call.data, dte == put.trade$dte)
        call.data <- dplyr::filter(call.data, delta <= c.delta)
        call.data <- dplyr::filter(call.data, delta >= c.delta.lim)
        call.trade <- dplyr::filter(call.data, delta == max(delta))

        if (nrow(call.trade) > 0 && nrow(put.trade) > 0)  {
          call.trade <- dplyr::mutate(call.trade, trade.num = t, type = "call") # Assign trade num
          put.trade <- dplyr::mutate(put.trade, trade.num = t, type = "put") # Assign trade num
          c.positions <- dplyr::bind_rows(c.positions, call.trade, fill = TRUE)
          p.positions <- dplyr::bind_rows(p.positions, put.trade, fill = TRUE)
        }
      }
    }
    # debug
    print(c.positions)
    # debug

  }) # End opening trades progress bar
  shiny::withProgress(message = "Progress Bar", detail = "Closing Trades", value = .5, {
    # Loop through all trades and find the results
    if (nrow(c.positions) == nrow(p.positions)) {
    for (i in 1:nrow(c.positions))  {
      incProgress(progress.int)
      sym <- c.positions[i, symbol]
      cop <- c.positions[i, mid.price]
      pop <- p.positions[i, mid.price]
      od <- c.positions[i, date]
      e <- c.positions[i, expiration]
      cs <- c.positions[i, strike]
      ps <- p.positions[i, strike]
      iv <- p.positions[i, iv]
      vol <- p.positions[i, volume]
      oi <- p.positions[i, open.interest]
      pd <- p.positions[i, delta]
      cd <- c.positions[i, delta]
      pv <- p.positions[i, vega]
      pg <- p.positions[i, gamma]
      pt <- p.positions[i, theta]
      pr <- p.positions[i, rho]
      cv <- c.positions[i, vega]
      cg <- c.positions[i, gamma]
      ct <- c.positions[i, theta]
      cr <- c.positions[i, rho]
      dte <- p.positions[i, dte]
      rsi <- p.positions[i, rsi.14]
      ivr <- p.positions[i, iv.rank]
      j <- c.positions[i, trade.num]
      # Close position every day so we can plot the entire range
      # of possible outcomes
      cur.date = od + 1
      while (cur.date <= e) {
        j <- j + .01
        # debug
        print(j)
        # debug
        close <- dplyr::filter(trade.data,
                               date == cur.date,
                               expiration == e)
        if (nrow(close) > 0) {
          call.results <- dplyr::filter(close,
                                      strike == cs,
                                      call.put == "C")
          ccp <- call.results$mid.price
          put.results <- dplyr::filter(close,
                                     strike == ps,
                                     call.put == "P")
          pcp <- put.results$mid.price
          profit <- 100 * ((pop - pcp) + (cop - ccp))
          days.held <- as.numeric(cur.date) - as.numeric(od)
          has_profit = ifelse(profit > 0, "Yes", "No")
          this.close <- data.frame(j, sym, od, cur.date, ps, cs, pop, cop, pcp, ccp, e, iv,
                                   vol, oi, pd, cd, pv, cv, pg, cg, pt, ct, pr,
                                   cr, dte, rsi, ivr, profit, days.held, has_profit)
          results <- dplyr::bind_rows(results, this.close)
        }
        cur.date = cur.date + 1
      }
    }
    colnames(results) <- c("trade.num", "symbol", "open.date", "close.date",
                           "put.strike", "call.strike",
                           "open.put.price", "open.call.price", "put.close.price",
                           "call.close.price", "expiration", "iv", "volume",
                           "open.interest", "put.delta", "call.delta",
                           "put.vega", "call.vega", "put.gamma", "call.gamma",
                           "put.theta", "call.theta", "put.rho", "call.rho",
                           "dte", "rsi", "ivr", "profit.loss", "days.held", "has_profit")

    results <- dplyr::mutate(results,
                             trade.num = as.numeric(trade.num),
                             exit.reason = "daily",
                             put.strike = as.numeric(put.strike),
                             open.date = as.Date(as.numeric(open.date),
                                                 origin = "1970-01-01"),
                             close.date = as.Date(as.numeric(close.date),
                                                  origin = "1970-01-01"),
                             dte = as.numeric(dte),
                             open.ivrank = as.numeric(ivr),
                             open.rsi = as.numeric(rsi),
                             call.strike = as.numeric(call.strike),
                             profit = as.numeric(profit.loss),
                             year = year(open.date),
                             days.held = as.numeric(close.date) - as.numeric(open.date),
                             has_profit = ifelse(profit > 0, "Yes", "No"),
                             open.put.price = as.numeric(open.put.price),
                             open.call.price = as.numeric(open.call.price),
                             put.close.price = as.numeric(put.close.price),
                             call.close.price = as.numeric(call.close.price),
                             expiration = as.Date(as.numeric(expiration),
                                                  origin = "1970-01-01"),
                             iv = as.numeric(iv),
                             volume = as.numeric(volume),
                             open.interest = as.numeric(open.interest),
                             put.delta = as.numeric(put.delta),
                             call.delta = as.numeric(call.delta),
                             put.vega = as.numeric(put.vega),
                             call.vega = as.numeric(call.vega),
                             put.gamma = as.numeric(put.gamma),
                             call.gamma = as.numeric(call.gamma),
                             put.theta = as.numeric(put.theta),
                             call.theta = as.numeric(call.theta),
                             put.rho = as.numeric(put.rho),
                             call.rho = as.numeric(call.rho))

    # # Add in commission at a rate of $1.5 per trade excluding closing for .05 or less
    # call.results <- dplyr::mutate(call.results,
    #                               call.profit = ifelse(call.close.price <= .05,
    #                                                    call.profit - 1.5, call.profit - 3))
    }
  }) # End closing trades progress bar
  shiny::withProgress(message = "Progress Bar", detail = "Creating Plot", value = .95, {
    # # Calculate totals for display
    assign("num_t", nrow(results), envir = .GlobalEnv)
    assign("tot_profit", sum(results$profit), envir = .GlobalEnv)
    assign("avg_profit", tot_profit/num_t, envir = .GlobalEnv)
    assign("tot_days", sum(results$days.held), envir = .GlobalEnv)
    assign("avg_days", tot_days/num_t, envir = .GlobalEnv)
    assign("avg_prof_day", avg_profit/avg_days, envir = .GlobalEnv)
    assign("maximum_loss",
           ifelse(min(results$profit) >= 0,
                  0, min(results$profit)), envir = .GlobalEnv)
    assign("percent_winners",
           scales::percent(length(which(results$profit > 0)) / num_t), .GlobalEnv)
    assign("exit.expiration",
           ifelse(scales::percent(length(which(results$exit.reason == "Expiration")) / num_t) == "NaN%",
                  0, scales::percent(length(which(results$exit.reason == "Expiration")) / num_t)),
           envir = .GlobalEnv)
    assign("exit.profit.target",
           ifelse(scales::percent(length(which(results$exit.reason == "Profit target")) / num_t) == "NaN%",
                  0, scales::percent(length(which(results$exit.reason == "Profit target")) / num_t)),
           envir = .GlobalEnv)
    assign("exit.loss.limit",
           ifelse(scales::percent(length(which(results$exit.reason == "Loss limit")) / num_t) == "NaN%",
                  0, scales::percent(length(which(results$exit.reason == "Loss limit")) / num_t)),
           envir = .GlobalEnv)
    assign("exit.gamma.risk",
           ifelse(scales::percent(length(which(results$exit.reason == "Gamma risk")) / num_t) == "NaN%",
                  0, scales::percent(length(which(results$exit.reason == "Gamma risk")) / num_t)),
           envir = .GlobalEnv)
    assign("exit.earnings",
           ifelse(scales::percent(length(which(results$exit.reason == "Earnings")) / num_t) == "NaN%",
                  0, scales::percent(length(which(results$exit.reason == "Earnings")) / num_t)),
           envir = .GlobalEnv)
  }) # End creating plot progress bar

  # Send results to global environment for further processing in main script
  results.table <- results
  assign("results", results, envir = .GlobalEnv)
  assign("results.table", results.table, envir = .GlobalEnv)
  rm(p.positions)
  rm(c.positions)
  rm(put.results)
  rm(call.results)
}
