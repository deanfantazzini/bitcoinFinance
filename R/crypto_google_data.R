#' Weekly cryptocurrencies price data and relative weekly Google trends data based on ticker searches
#'
#' A dataset containing weekly price data of 5 cryptocurrencies (bitcoin,litecoin,ripple,ethereum,monero)
#' and relative weekly Google trends data based on ticker searches (BTC,ETH,LTC,XMR,XRP)
#'
#' @docType data
#'
#' @usage data(crypto_google_data)
#'
#' @format A dataframe of 145 rows and 11 columns
#'
#' @keywords datasets
#'
#' @source \href{https://coinmarketcap.com/}{CryptoCurrency Market Capitalizations} .
#'
#' @examples
#' data(crypto_google_data)
#'
#' #The dataset was formed using the following code:
#' \dontrun{
#' dates<-seq(as.Date("2015/08/09"), by = "week", length.out = 145)
#' dates <- as.numeric( gsub("-", "", dates) )
#' cryptoname <-c("bitcoin", "litecoin", "ripple", "ethereum", "monero")
#' coins_all=NULL
#' for (nn in cryptoname) {
#'    print(nn)
#'    pcoin<-download_coinmarketcap_weekly(dates, nn)
#'    coins_all=cbind(coins_all,pcoin)
#' }
#' coins_all=cbind(dates, coins_all)
#' colnames(coins_all)=c("dates",cryptoname)
#'
#' library(tidyr)
#' tickers<-c("BTC", "LTC", "XRP", "ETH", "XMR")
#' res <- gtrends(keyword = tickers, time = paste(ymd(first(dates)), ymd(last(dates))) )
#' all_gt <- res$interest_over_time[,1:3]
#' # make the data clean
#' all_gt<- tidyr::spread(all_gt, keyword, hits)
#' # avoid zero searches
#' all_gt[all_gt < 1] <- 0.1
#' # Transform to numbers
#' all_gt=apply(all_gt[2:6],2,as.numeric)
#' crypto_google_data=as.data.frame(cbind(coins_all, all_gt))
#' }
#'
"crypto_google_data"
