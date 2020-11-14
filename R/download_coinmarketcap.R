#' Download the daily data of the selected cryptocurrency from coinmarketcap.com
#'
#' This function downloads the daily data of the selected cryptocurrency from coinmarketcap.com
#'
#' @param cryptoname is the selected cryptocurrency
#' @param start_date is the starting date (format YYYYMMDD) for downloading the selected cryptocurrency daily data
#' @param end_date is the ending date (format YYYYMMDD) for downloading the selected cryptocurrency daily data
#' @return table_news is a dataframe with daily open high low close turnover and market capitalization
#' @details
#' This function downloads the daily data of the selected cryptocurrencies from coinmarketcap.com
#'
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' dat<-download_coinmarketcap_daily(cryptoname="ethereum",start_date=20160101,end_date=20170704)
#' head(dat)
#'

download_coinmarketcap_daily <- function(cryptoname="bitcoin", start_date=20130428, end_date=20170705){
  page = xml2::read_html( paste("https://coinmarketcap.com/currencies/", cryptoname,"/historical-data/?start=",start_date,"&end=",end_date, sep="") )
  data_node = rvest::html_nodes(page, "[id='__NEXT_DATA__']")
  json_from_node = rvest::html_text(data_node)
  list_from_json = jsonlite::fromJSON(json_from_node)
  table_news = list_from_json$props$initialState$cryptocurrency$ohlcvHistorical[[1]]$quotes
  return(table_news)
}

#' Download the weekly data of the selected cryptocurrency from coinmarketcap.com
#'
#' This function downloads the weekly data of the selected cryptocurrency from coinmarketcap.com
#'
#' @param cryptoname is the selected cryptocurrency
#' @param dates is a sequence of dates (format YYYYMMDD) for downloading the selected cryptocurrency weekly data
#' @return price is a numeric vector of weekly prices
#' @details
#' This function downloads the weekly data of the selected cryptocurrencies from coinmarketcap.com .
#' Warning: this function is quite time-consuming because it opens a weekly webpage one after the other.
#' It is used here for educational purposes only.
#'
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#'
#' @examples
#' dates<-seq(as.Date("2015/08/09"), by = "week", length.out = 3)
#' dates <- as.numeric( gsub("-", "", dates) )
#' cryptoname <-"Bitcoin"
#' coin<-download_coinmarketcap_weekly(dates, cryptoname)
#' coins_all=cbind(dates, coin)
#' colnames(coins_all)=c("dates",cryptoname)
#' as.data.frame(coins_all)
#'
download_coinmarketcap_weekly <- function(dates, cryptoname){
  price= NULL
  for (page_number in 1:length(dates)) {
    news <- xml2::read_html(paste("https://coinmarketcap.com/historical/", dates[page_number], sep=""))
    news <- rvest::html_nodes(news, "table")
    news <- rvest::html_table(news)
    news <- subset(news[[3]], news[[3]]$Name == cryptoname, select = "Price")
    price = c(price, as.numeric(gsub("[$]", "", news)) )
  }
  return(price)
}
