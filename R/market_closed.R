#' market closed dates
#' @description{
#' mrkt_closed creates a data.frame with the dates the market was closed by reading
#' from .csv file passed to be used in the tidy data process.  Update the .csv prior to
#'  running.
#' }
#' @param file_name String of file to be read in.
#'
#' @return data.frame \code{market_closed} with the each day the market was closed saved
#' as "data/market_closed.RData".
#'
#' @export
#'
#' @importFrom data.table fread
#'
#' @examples
#' market_closed <- mrkt_closed("data/market_closed.csv")

mrkt_closed <- function(file_name) {
  market_closed <- fread(file_name)
  market_closed$date <- as.Date(market_closed$date)
  save(market_closed, file = "data/market_closed.RData")
}