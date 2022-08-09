#' Pipe function to unite two characters
#'
#' @param e1 is the first term to be united
#' @param e2 is the second term to be united
#'
#' @export
#' @examples
#' "United " %p% "States"

`%p%` <- function(e1,e2) {
  # This pipe unites two characters. It works similarly to '+' in python.
  return(paste0(e1,e2))
}


#' Download time series data of a (pure) cryptocurrency pair.
#'
#' This function downloads time series data of a (pure) cryptocurrency pair (i.e. crypto vs crypto) from the Poloniex exchange,
#' given a specified starting point, ending point and candlestick period in seconds
#'
#' @param from is the base currency of the currency pair
#' @param to is the quote currency of the currency pair
#' @param start is the starting point of the required data sample
#' @param end is the ending point of the required data sample
#' @param period is the candlestick period in seconds (valid values are 300, 900, 1800, 7200, 14400, and 86400)
#' @return a dataframe with data of HLOCV of the referred cryptocoin
#'
#' @export
#' @importFrom lubridate ymd
#' @importFrom rjson fromJSON
#' @importFrom plyr mutate
#'
#' @examples
#' eth_data <- poloniex_download(from="USDT", to="ETH", start="2013-01-01", end="2017-05-18")

poloniex_download <- function(from="USDT", to="ETH", start="2001-01-01",end="2017-01-01", period=86400) {
  end <- as.numeric(as.POSIXct(lubridate::ymd(end)))
  start <- as.numeric(as.POSIXct(lubridate::ymd(start)))
  link <- "https://poloniex.com/public?command=returnChartData&currencyPair=" %p% from %p% "_" %p% to %p% "&start=" %p% start %p% "&end=" %p% end %p% "&period=" %p% period
  #data_ts <-jsonlite::fromJSON(link) #old solution
  data_ts <-rjson::fromJSON(file=link)
  data_ts <- do.call(rbind.data.frame, data_ts)
  data_ts <- plyr::mutate(data_ts, date = as.Date(as.POSIXct(as.numeric(data_ts$date)/1000,origin = "1970-01-01")))
  data_ts[,2:8]=sapply(data_ts[, c(2:8)], as.numeric)
  return(data_ts)
}
