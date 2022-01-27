#' Download the daily data of the selected cryptocurrency from coinmarketcap.com
#'
#' This function downloads the daily data of the selected cryptocurrency from coinmarketcap.com
#'
#' @param id is the coinmarketcap ID used for the selected cryptocurrency (see the coinmarketcap API for the full ID list)
#' @param currency is the reference currency used in the crypto pair
#' @param start_date is the starting date (format YYYY-MM-DD) for downloading the selected cryptocurrency daily data
#' @param end_date is the ending date (format YYYY-MM-DD) for downloading the selected cryptocurrency daily data
#' @return table_news is a dataframe with daily open high low close volume and market capitalization
#' @details
#' This function downloads the daily data of the selected cryptocurrencies from coinmarketcap.com
#'
#' @export
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' dat<-download_coinmarketcap_daily(id=2,currency="USD",start_date="2016-09-16",end_date="2020-09-23")
#' head(dat)
#'

download_coinmarketcap_daily <- function(id=1, currency="USD", start_date="2018-09-16", end_date="2020-09-23"){
  start_date=as.numeric(as.POSIXct(start_date))
  end_date  =as.numeric(as.POSIXct(end_date))
  page = paste("https://web-api.coinmarketcap.com/v1/cryptocurrency/ohlcv/historical?id=",id, "&convert=", currency, "&time_start=", start_date, "&time_end=", end_date,sep="")
  list_from_json = jsonlite::fromJSON(page)
  table_news = list_from_json$data$quotes$quote$USD
  table_news$timestamp=as.Date(table_news$timestamp)
  return(table_news)
}

#' Download the weekly data of the selected cryptocurrency from coinmarketcap.com
#'
#' This function downloads the weekly data of the selected cryptocurrency from coinmarketcap.com
#'
#' @param id is the coinmarketcap ID used for the selected cryptocurrency (see the coinmarketcap API for the full ID list)
#' @param currency is the reference currency used in the crypto pair
#' @param start_date is the starting date (format YYYY-MM-DD) for downloading the selected cryptocurrency weekly data
#' @param end_date is the ending date (format YYYY-MM-DD) for downloading the selected cryptocurrency weekly data
#' @return table_news is a dataframe with weekly open high low close volume and market capitalization
#' @details
#' This function downloads the weekly data of the selected cryptocurrencies from coinmarketcap.com
#'
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom stats aggregate
#'
#' @examples
#' dat<-download_coinmarketcap_w(id=2,currency="USD",start_date="2016-09-16",end_date="2020-09-23")
#' head(dat)
#'
download_coinmarketcap_w<-function(id=1,currency="USD",start_date="2018-09-16",end_date="2020-09-23"){
  start_date=as.numeric(as.POSIXct(start_date))
  end_date  =as.numeric(as.POSIXct(end_date))
  page = paste("https://web-api.coinmarketcap.com/v1/cryptocurrency/ohlcv/historical?id=",id, "&convert=", currency, "&time_start=", start_date, "&time_end=", end_date,sep="")
  list_from_json = jsonlite::fromJSON(page)
  table_news = list_from_json$data$quotes$quote$USD
  table_news$timestamp=as.Date(table_news$timestamp)
  Week = as.Date(cut(table_news$timestamp, "week"))
  table_news=stats::aggregate(cbind(open,high,low,close,volume,market_cap) ~ Week, table_news, mean)
  return(table_news)
}
