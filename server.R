# TODO(jason):------------------------------------------------------------------
# - change envir .Global to current shiny envir
# - ensure earnings date is correct due to AM or PM reporting


# Shiny Server------------------------------------------------------------------
shiny::shinyServer(function(input, output, session) {
  shiny::shinyOptions(progress.style = "old")
  main.color <- "#00a65a" # Green

  # Uncomment next line for debugging to console
  # options(shiny.trace = TRUE)
  # The following two lines can be inserted to box in code section for profiling
  # Rprof("boot.out")
  # Rprof(NULL)
  # options(shiny.error = recover)

  output$notificationMenu <- renderMenu({
      notificationItem(text = "Need help?", icon = shiny::icon("user"), status = "info",
                       href = "mailto:jason@themechanicalbear.com")
    })

  # Hide/UnHide Axis variable inputs
  shiny::observe({
    toggle(id = "xvar", condition = input$goPlot)
    toggle(id = "yvar", condition = input$goPlot)
  })

  output$welcome_message <- shiny::renderUI({
    HTML("<p style=\"text-align: center; font-size: 60px; color: #FFFFFF;\">.</p>
         <p style=\"text-align: center; font-size: 20px;\"><strong>Welcome:</strong> Please fill out the study inputs in the left sidebar</p>
         <p style=\"text-align: center; font-size: 20px;\">When ready click&nbsp;<strong>Run Study</strong>&nbsp;to see the results</p>")
  })

  # Insert loading image in the message output
  shiny::observeEvent(input$goPlot, {
    output$loading_image <- shiny::renderImage({
      list(
        src = "www/images/ajax-loader.gif",
        filetype = "image/gif",
        alt = "Loading"
      )
    }, deleteFile = FALSE)
  })

  # Reactive section for building executed trade list
  shiny::observeEvent(input$goPlot, {
    # We reset the results data.frame when inputs are changed
    if (exists("results", envir = .GlobalEnv) && is.data.frame(get("results"))) {
      rm(results, envir = .GlobalEnv)
    }
    assign("openOption", input$openOption, envir = .GlobalEnv)
    if (openOption == "First of Month") {
      data(list = "monthly")
      assign("first_day", monthly, envir = .GlobalEnv)
      assign("inc.amount", .004)}
    else if (openOption == "First of Week") {
      data(list = "mondays")
      assign("first_day", mondays, envir = .GlobalEnv)
      assign("inc.amount", .001)}
    else if (openOption == "Daily") {
      data(list = "open_daily")
      assign("first_day", open_daily, envir = .GlobalEnv)
      assign("inc.amount", .0002)}

    shiny::withProgress(message = "Progress", detail = "Setting up study", value = .05, {
      t <- 0 # Set inital trade number to zero
      progress.int <- inc.amount # Set progress bar increment amount

      # Values defined by the customer in shiny ui
      assign("study", input$study, envir = .GlobalEnv)
      assign("stock", input$stock, envir = .GlobalEnv)
      assign("low_iv", input$open_ivrank[1], envir = .GlobalEnv)
      assign("high_iv", input$open_ivrank[2], envir = .GlobalEnv)
      assign("o_dte", input$open_dte, envir = .GlobalEnv)
      assign("s_dte", input$second_dte, envir = .GlobalEnv)
      assign("c_delta", input$call_delta, envir = .GlobalEnv)
      assign("p_delta", input$put_delta, envir = .GlobalEnv)
      assign("prof_targ", input$proftarg / 100, envir = .GlobalEnv)
      assign("loss_lim", input$loss_lim + 1, envir = .GlobalEnv)
      assign("l_loss_lim", input$l_loss_lim / 100, envir = .GlobalEnv)
      assign("g", input$gamma_days, envir = .GlobalEnv)
      assign("earn_close", input$earn_close, envir = .GlobalEnv)
      assign("min_roc", input$min_roc, envir = .GlobalEnv)
      assign("p_delta_lim", p_delta + .1, envir = .GlobalEnv)
      assign("c_delta_lim", c_delta - .1, envir = .GlobalEnv)
      #assign("stock_list", as.data.frame(symbol_list[-length(symbol_list)],
      #                                   stringsAsFactors = FALSE), envir = .GlobalEnv)
      #assign("stock_list", as.data.frame(symbol_list, stringsAsFactors = FALSE), envir = .GlobalEnv)

      # Load option chain data for stock chosen by customer
      # if (!stock == "ALL") {
      #   data(list = paste0(stock, ".options"))
      # }
      data_set <- get(load(paste0("data/", stock, "_options.RData")))

      # # Opening frequency
      # if (openOption == "First of Month") {
      #   data(list = "open_first_day_month")
      #   assign("first_day", open_first_day_month, envir = .GlobalEnv)}
      # else if (openOption == "First of Week") {
      #   data(list = "open_first_day_week")
      #   assign("first_day", open_first_day_week, envir = .GlobalEnv)}
      # else if (openOption == "Daily") {
      #   data(list = "open_daily")
      #   assign("first_day", open_daily, envir = .GlobalEnv)}
      # else if (openOption == "Earnings") {
      #   data(list = paste0("earnings_dates.", stock))
      #   assign("first_day", earnings_dates, envir = .GlobalEnv)}
      # else if (openOption == "Previous Close") {
      # #Fill in the custom dates .csv for this
      # source("Shared/customopen.R")}
      #
      # # Close prior to earnings?
      # if (earn_close == "Yes")  {
      #   data(list = paste0("earnings_dates.", stock))
      #   assign("earnings_close", earnings_dates, envir = .GlobalEnv)}

    })

# Run function for study selected
    # if (study == "Call Calendar") {
    #   call_calendar(progress.int, t)}
    # if (study == "Poor Mans Cov Call") {
    #   pmcc(progress.int, t)}
    if (study == "Short Put") {
      short_put(progress.int, t)}
    # if (study == "Long Stock") {
    #   LongStock(progress.int, t)}
    if (study == "Strangle") {
      # if (stock == "ALL") {
      #   for (i in 1:nrow(stock_list)) {
      #     data(list = paste0(stock_list[i, ], ".options"))
      #     strangle(progress.int, t)}}
      # else {
        strangle(progress.int, data_set, t)}#}
    if (study == "Straddle") {
      straddle(progress.int, t)}
    # if (study == "Strangle Daily Close") {
    #   strangle_daily_close(progress.int, t)}
    results
  })

  # Function for generating tooltip (hover over) text
  trade_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$trade.num)) return(NULL)

    # Pick out the trade with this trade_num
    trades <- isolate(trades())
    trade <- trades[trades$trade_num == x$trade_num, ]

    paste0("Open: ", trade$open_date, "<br>",
           "Close: ", trade$close_date, "<br>",
           "Call strike: ", trade$call_strike, "<br>",
           "Put strike: ", trade$put_strike, "<br>",
           "DTE: ", trade$dte, "<br>",
           "IVRank: ", round(trade$open_ivrank, digits = 0), "<br>",
           "rsi: ", round(trade$open_rsi, digits = 0), "<br>",
           "Exit: ", trade$exit_reason, "<br>",
           "Profit: $", format(trade$profit, big.mark = ",", scientific = FALSE)
    )
  }

  # HTML output
  shiny::observeEvent(input$goPlot, {
    output$welcome_message <- shiny::renderUI({
      HTML("")
    })
    output$n_trades <- shiny::renderUI({
      str_num_trades <- paste0("Number of trades: ", nrow(results))
      HTML(str_num_trades)
    })
    environment(output_HTML) <- environment()
    output_HTML()
  })

  # Render datatable
  shiny::observeEvent(input$goPlot, {
    # Output of a table to show the trade details
    output$table <- shiny::renderDataTable({results.table},options = list(
      pageLength = 5,
      lengthMenu = c(5, 10, 15),
      scrollX = TRUE
    ))
    # Download table
    output$downloadData <- shiny::downloadHandler(
      filename = function() { paste0(stock, '_1SD_60DTE_Strangle_', as.character(prof_targ), '%_', as.character(loss_lim - 1), 'X_results.csv') },
      content = function(file) {
        write.csv(results.table, file)
      }
    )
  })

  # rbokeh trades plot
  shiny::observeEvent(input$goPlot,{
    output$rbokeh.trades <- renderRbokeh({
      # Lables for axes
      xvar_name <- names(axis_vars)[axis_vars == input$xvar]
      yvar_name <- names(axis_vars)[axis_vars == input$yvar]

      plot_data <- results
      h <- figure(xlab = xvar_name, ylab = yvar_name, title = paste0(openOption, " ", stock, " ", study),
                  width = 1200, legend_location = "top_left", padding_factor = .2) %>%
        ly_points(input$xvar, input$yvar, hover = list(open_date, close_date, call_strike, put_strike, dte,
                                                       open_ivrank, open_rsi, exit_reason, profit),
                  data = plot_data, alpha = 0.5, color = Profitable, size = 5)
      return(h)
    })
  })

  # rbokeh profits plot
  shiny::observeEvent(input$goPlot,{
    output$rbokeh.profits <- renderRbokeh({
      plot_data <- results
      plot_data <- dplyr::mutate(plot_data, cum_sum = cumsum(profit), stock_sum = cumsum(hold_profit))
      g <- figure(xlab = "Open Date", ylab = "Cumulative Profit", width = 1200,
                  title = paste0(openOption, " ", stock, " ", study, " (allows for overlapping positions)"), legend_location = "top_left") %>%
        #ly_points(open_date, cum_sum, hover = list(open_date, close_date, call_strike, put_strike, dte,
        #                                           open_ivrank, open_rsi, exit_reason, profit, cum_sum), data = plot_data,
        #          alpha = 0.5, color = main.color, size = 5) %>%
        ly_lines(open_date, cum_sum, data = plot_data, color = main.color, alpha = 0.3, legend = "Study return", width = 4) %>%
        #ly_lines(open_date, hold_profit, data = plot_data, legend = "Buy & Hold", type = 2, width = 4) %>%
        ly_lines(open_date, stock_sum, data = plot_data, legend = "Stock hold return", type = 2, width = 2) %>%
        #ly_text(open_date, hold_profit, text = hold_profit, data = tail(plot_data, n = 1),
        #        font_style = "normal", font_size = "8pt",
        #        align = "left", baseline = "top") %>%
        ly_text(open_date, cum_sum, text = cum_sum, data = tail(plot_data, n = 1),
                font_style = "normal", font_size = "8pt", color = main.color,
                align = "left", baseline = "top") %>%
        ly_text(open_date, stock_sum, text = stock_sum, data = tail(plot_data, n = 1),
                font_style = "normal", font_size = "8pt", color = "black",
                align = "left", baseline = "top")
      return(g)
    })
  })

  # Render second datatable
  # shiny::observeEvent(input$goPlot,{
  #   # Output of a table to show the trade details
  #   output$table2 <- shiny::renderDataTable({warnings_table}, options = list(pageLength = 15, lengthMenu = c(5, 15, 30)
  #   ))
  #   # Download table
  #   output$downloadData2 <- shiny::downloadHandler(
  #     filename = function() { paste('st_results_table.csv') },
  #     content = function(file) {
  #       write.csv(st_results_table, file)
  #     }
  #   )
  # })

  # Reset default values when inputs change
  shiny::observe({
    if (input$stock == "EEM" || input$stock == "EWZ" || input$stock == "FXI" ||
        input$stock == "GDX" || input$stock == "SLV" || input$stock == "SPY" ||
        input$stock == "XLE")  {
      shiny::updateSelectInput(session, "earn_close", selected = "No")
    }
  })
})
