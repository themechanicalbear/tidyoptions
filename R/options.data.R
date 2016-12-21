# title: "options.data.R"
# author: "Jason Taylor"

# todos:
# - need to modify and run the earnings.open.R file to create the necessary .Rdata
# files

#' options.data: Processed option chain data from iVolatility.com
#'
#' options.data provides cleansed option chain data from iVolatility.com.  It's
#' the starting point to performing quantatative options trading analysis and
#' research. Utilities to create the data and utility function to run studies
#' are included in the package.
#'
#' It has three main goals:
#'
#' \itemize{
#' \item Provide cleansed option chain data that is in a standardized complete
#'   format to allow for easy analysis
#' \item Allow the data to be quickly loaded using the data() function
#' \item Provide data packaged to be portable and easy shared
#' }
#'
#' To learn more about options.data, start with the vignettes:
#' \code{browseVignettes(package = "options.data")}
#'
#' @docType package
#' @name options.data
#' @useDynLib options.data
NULL
