#' Generating tooltip (hover over) text for plots
#' @description{
#' trade_tooltip takes a results data.frame from a study and selects the row that
#' you are hoveringformats numerics to currency for out put in shiny app
#' }
#' @param x data.frame of trade results
#'
#' @return string of HTML formated values formated from the specific results row
#' to be included in the hover over of a plot
#'
#' @examples
#' trade_tooltip(results)
#'
trade_tooltip <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.null(x$trade.num)) return(NULL)

  # Pick out the trade with this trade.num
  trades <- isolate(trades())
  trade <- trades[trades$trade_num == x$trade_num, ]

  paste0("Open: ", trade$open_date, "<br>",
         "Close: ", trade$close_date, "<br>",
         "Call strike: ", trade$call_strike, "<br>",
         "Put strike: ", trade$put_strike, "<br>",
         "DTE: ", trade$dte, "<br>",
         "IVRank: ", trade$open_ivrank, "<br>",
         "rsi: ", round(trade$open_rsi, digits = 0), "<br>",
         "Exit: ", trade$exit_reason, "<br>",
         "Profit: $", format(trade$profit, big.mark = ",", scientific = FALSE)
  )
}