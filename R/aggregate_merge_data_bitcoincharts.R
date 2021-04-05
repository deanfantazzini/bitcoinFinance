#' Aggregate and merge time series data downloaded from bitcoincharts
#'
#' This function aggregates and merges time series data downloaded from bitcoincharts
#'
#' @param data_list is a list containing the dataframes of several time series data downloaded from bitcoincharts
#' @param level_of_aggregation  can be 'seconds', 'minutes' or 'hours'
#' @param aggregate_every defines how many intervals to aggregate (depends on level_of_aggregation)
#' @return results a list containing the merged intraday bitcoin prices (an xts object) and their daily returns (an xts object)
#' @details
#' This function aggregates and merges time series data downloaded from bitcoincharts and provided to the function as a list.
#' It returns a list containing the dataframe with the merged price data according to the desired level of aggregation at the intraday level,
#' as well as a dataframe with the merged daily data returns.
#'
#'
#' @export
#' @importFrom dplyr group_by summarise ungroup %>%
#' @importFrom xts xts make.index.unique
#' @importFrom highfrequency aggregateTS makeReturns
#' @importFrom rlang .data
#'
#'
#' @examples
#' \dontrun{
#' library(highfrequency)
#' localbtcMXN <-bitcoincharts_single_download(name = "localbtcMXN.csv.gz")
#' btcdeEUR <-bitcoincharts_single_download(name = "btcdeEUR.csv.gz")
#' krakenEUR <-bitcoincharts_single_download(name = "krakenEUR.csv.gz")
#' localbtcINR <-bitcoincharts_single_download(name = "localbtcINR.csv.gz")
#' coinbaseUSD <-bitcoincharts_single_download(name = "coinbaseUSD.csv.gz")
#' data5<-list(localbtcMXN =localbtcMXN, btcdeEUR=btcdeEUR, krakenEUR=krakenEUR,
#'              localbtcINR=localbtcINR, coinbaseUSD=coinbaseUSD)
#' data_clean<-aggregate_merge_bictoincharts_data(data_list=data5)
#' }
#'

aggregate_merge_bictoincharts_data <- function(data_list,
                                               level_of_aggregation = 'minutes',
                                                aggregate_every = 15){
        price_ts <- do.call( merge, c(lapply(data_list, function(x){
                            tmp <- x %>% group_by(.data$timestamp) %>% summarise(price = mean(.data$price)) %>% ungroup()
                           tmp <- xts::xts(x = tmp$price, order.by = tmp$timestamp)
                           highfrequency::aggregateTS(tmp, alignBy=level_of_aggregation, alignPeriod=aggregate_every)
                           }), list(all = F) ) ) # consider only common data
        names(price_ts)=names(data_list)

        # calculating daily returns
        daily_returns <- highfrequency::makeReturns(aggregateTS(price_ts, alignBy = 'days', alignPeriod = 1, dropna = T))
        daily_returns <- xts::make.index.unique(daily_returns, drop = T)
        names(daily_returns)=names(data_list)

        results <-list(price_ts=price_ts, daily_returns=daily_returns)
        return(results)
}


