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
#' @importFrom rvest html_nodes html_table
#' @importFrom magrittr %>%
#'
#' @examples
#' download_coinmarketcap_daily(cryptoname="ethereum", end_date=20170704)
#'

download_coinmarketcap_daily = function(cryptoname="bitcoin", start_date=20130428, end_date=20170705){
  news <- xml2::read_html( paste("https://coinmarketcap.com/currencies/", cryptoname,"/historical-data/?start=",start_date,"&end=",end_date, sep="") )
  table_news <- news %>%  rvest::html_nodes("table")
  table_news <- table_news[[1]] %>% rvest::html_table()
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
#' Warning: this function is quite time-consuming becasue it opens a weekly webpage one after the other.
#' It is used here for educational purposes only.
#'
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @importFrom magrittr %>%
#'
#' @examples
#' dates<-seq(as.Date("2015/08/09"), by = "week", length.out = 3)
#' dates <- as.numeric( gsub("-", "", dates) )
#' cryptoname <-c("bitcoin", "ethereum")
#' coins_all=NULL
#' for (nn in cryptoname) {
#'   print(nn)
#'   pcoin<-download_coinmarketcap_weekly(dates, nn)
#'   coins_all=cbind(coins_all,pcoin)
#' }
#' coins_all=cbind(dates, coins_all)
#' colnames(coins_all)=c("dates",cryptoname)
#' as.data.frame(coins_all)
#'
download_coinmarketcap_weekly <- function(dates, cryptoname){
  price= NULL
  for (page_number in 1:length(dates)) {
    news <- xml2::read_html(paste("https://coinmarketcap.com/historical/", dates[page_number], sep=""))
    id_crypto<-paste0("#id-", cryptoname, " .price")
    price = c(price, news %>%
                rvest::html_nodes(id_crypto) %>%
                rvest::html_text())
  }
  price <- gsub("[$] ", "", price)
  price <- gsub("[$]", "", price)
  price <- as.numeric(price)
  return(price)
}
