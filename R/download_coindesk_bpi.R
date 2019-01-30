#' Download the daily time series data of the CoinDesk Bitcoin Price Index (BPI).
#'
#' This function downloads the daily time series data of the CoinDesk Bitcoin Price Index (BPI),
#' given a specified starting point and ending point.
#'
#' @param start is the starting point of the required daily data sample
#' @param end is the ending point of the required daily  data sample
#' @return a xts object containing the required historical daily data for the CoinDesk Bitcoin Price Index
#'
#' @export
#' @importFrom rjson fromJSON
#' @importFrom xts xts
#'
#' @examples
#' xbp_data <- coindesk_download(start="2011-01-01",end="2017-07-27")


coindesk_download <- function(start="2011-01-01",end="2017-07-27") {
  link <- "http://api.coindesk.com/v1/bpi/historical/close.json?start=" %p% start %p% "&end=" %p% end
  #dat <-RJSONIO::fromJSON(link) # old solution
  dat <-rjson::fromJSON(file=link)
  dat <- xts::xts(as.numeric(dat$bpi), as.Date(names(dat$bpi), format='%Y-%m-%d'))
  colnames(dat)<-"XBP"
  return(dat)
}


