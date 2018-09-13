#' Download the time series data of an exchange pair involving bitcoin and a fiat currency.
#'
#' This function downloads the time series data of an exchange pair involving bitcoin and a fiat currency,
#' given the specified quote currency, data type, cryptocurrency exchange, data frequency, time length,
#' spread depth (if requested).
#'
#' @param currency is the quote currency of the currency pair
#' @param data_type is the type of data required. Possible cases are: "volume","rank","price",
#' "price_volume","market_cap","tradespm","volatility","spread","bidask_sum".
#' @param exchange is the cryptocurrency exchange
#' @param freq is the data frequency: "hour", "day", or "week"
#' @param time_length is the time length required: "10m","1h","6h","24h", "3d", "7d", "30d", "6m", "2y", "5y", "all".
#' @param sd is the spread depth. Possible values in BTC are: NULL (default), "10", "100".
#' @return a dataframe with the requested data
#'
#' @export
#' @importFrom utils read.csv
#'
#' @examples
#' dat<-bitcoinity_download(currency="USD", data_type="spread", exchange=NULL,sd="10")
#' head(dat)[1:4]

bitcoinity_download <- function(currency="USD",
                                data_type="price_volume",
                                exchange="bitstamp",
                                freq="hour",
                                time_length="30d",
                                sd=NULL){
  baseurl = "http://data.bitcoinity.org/export_data.csv?"
  if(is.null(exchange)){
    url=paste0(baseurl,"currency=",currency,"&data_type=",data_type,
               "&r=", freq, "&timespan=",time_length)
  }
  if(is.null(exchange)&!is.null(sd)){
    url=paste0(baseurl,"currency=",currency,"&data_type=",data_type,
               "&r=", freq, "&timespan=",time_length, "&sd=", sd)
  }
  if(!is.null(exchange)){
  url=paste0(baseurl,"currency=",currency,"&data_type=",data_type,
             "&exchange=",exchange,"&r=", freq, "&timespan=",time_length)
  }
  if(!is.null(exchange)&!is.null(sd)){
    url=paste0(baseurl,"currency=",currency,"&data_type=",data_type,
               "&exchange=",exchange,"&r=", freq, "&timespan=",time_length, "&sd=", sd)
  }
  x = read.csv(file=url)
  return(x)
}


#' Download the time series data of the depth for price impact, for an exchange pair involving bitcoin and a fiat currency.
#'
#' This function downloads the time series data of the depth for price impact, for an exchange pair involving bitcoin and a fiat currency,
#' given the specified price range in percentage (k), depth sum expressed in BTC or in quote currency, quote currency, data type,
#' cryptocurrency exchange, data frequency, time length.
#'
#' @param bp is the price range (k) for the depth for price impact
#' @param bu can be "b" (BTC) or "c" (currency). If you choose the former, the depth sum is expressed in BTC;
#' if you choose the latter, the depth sum is expressed in the quote currency.
#' @param currency is the quote currency of the currency pair
#' @param data_type is the type of data required. Possible cases are: "volume","rank","price",
#' "price_volume","market_cap","tradespm","volatility","spread","bidask_sum".
#' @param exchange is the cryptocurrency exchange
#' @param freq is the data frequency: "hour", "day", or "week"
#' @param time_length is the time length required: "10m","1h","6h","24h", "3d", "7d", "30d", "6m", "2y", "5y", "all".
#' @return a dataframe with the requested data
#'
#' @export
#' @importFrom utils read.csv
#'
#' @examples
#' aa <- bitcoinity_download_DIK(bp="10",bu="b",currency="USD",data_type="bidask_sum",
#'       exchange="bitstamp",freq="hour",time_length="30d")
#' head(aa)


bitcoinity_download_DIK <- function(bp="10",
                                    bu="b",
                                    currency="USD",
                                    data_type="bidask_sum",
                                    exchange="bitstamp",
                                    freq="hour",
                                    time_length="30d"){
  baseurl <- "http://data.bitcoinity.org/export_data.csv?"
  url <- paste0(baseurl,"bp=",bp,"&bu=",bu,"&currency=",currency,"&data_type=",data_type,
               "&exchange=",exchange,"&r=", freq, "&timespan=",time_length)

  x = read.csv(file=url)
  return(x)
}
