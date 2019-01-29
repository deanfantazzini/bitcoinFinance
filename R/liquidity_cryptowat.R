

#' This is a small function to get a time string
#'
#' This is a small function to get a time string
#'
#' @param separator a character string to separate the terms
#' @return a character time string for the current date and time
#' @details
#' This function delivers a character time string for the current date and time
#'
#'
#' @export
#'
#' @examples
#' get_time_str()


get_time_str <- function(separator="_") {
  # Shorthand for getting a time string
  return(format(Sys.time(), paste("%Y%m%d", "%H%M%S",
                                  sep=separator)))
}




#' Download trades data or order book data from cryptowat.ch
#'
#' This function downloads trades data or order book data from cryptowat.ch
#'
#' @param exchange is the required exchange
#' @param pair is the required exchange rate pair
#' @param query_type can be either "orderbook" or "trades"
#' @param query_options can be either NULL or "limit=100" (in the latter case the limit can vary)
#' @param data_dir is the file path where to save the downloaded data
#' @param file_prefix is a prefix to the filename where the data will be saved.If NULL, date and time will be added.
#' @param logfile is a text file where the situation of the download process will be saved
#' @param register collects the names of the successfully downloaded files and their characteristics
#' @details
#' This function downloads trades data or order book data from cryptowat.ch and save them in a .json file
#'
#'
#' @export
#' @importFrom curl curl_fetch_disk
#' @importFrom curl has_internet
#'
#'
#' @examples
#' \dontrun{
#' cryptowatch_data_download("coinbase","btcusd", "orderbook", data_dir=getwd() )
#' cryptowatch_data_download("coinbase","btcusd", "trades",
#'    query_options="limit=100", data_dir=getwd() )
#' }

cryptowatch_data_download <- function(exchange,
                                      pair,
                                      query_type,
                                      query_options=NULL,
                                      data_dir=NULL,
                                      file_prefix=NULL,
                                      logfile="loader.log",
                                      register="register.csv") {
  cryptowatch_base_url <- "https://api.cryptowat.ch"
  file_prefix <- get_time_str("")

  # Generate the URL
  url_string <- paste(cryptowatch_base_url, "markets",
                      exchange, pair, query_type, sep="/")
  if (!is.null(query_options)) {
    url_string <- paste(url_string, query_options, sep="?")
  }
  # Generate the file path and name
  if (is.null(data_dir)) {
    data_dir <- "."
  }
  if (is.null(file_prefix)) {
    file_prefix <- get_time_str()
  }
  file_name <- paste(file_prefix, exchange, pair, query_type, sep="_")
  file_name <- paste(data_dir,
                     paste0(file_name, ".json"),
                     sep="/")
  print(paste("Generated filename for ", url_string, ": ",
              file_name, sep=""))

  # Log that we have started processing the query
  write(paste(get_time_str(), "The query has started for:", url_string),
        logfile,
        append=TRUE)

  # If there is no connection, report and quit
  if (!has_internet()) {
    write(paste(get_time_str(), "Connection failed for",
                url_string),
          logfile,
          append=TRUE)
    print(paste(get_time_str(), "Connection failed for",
                url_string))
  }
  # If there is connection, download the data and report the status code
  if (has_internet()){
    res <- curl_fetch_disk(url_string, file_name)
    write(paste(get_time_str(),
                "CODE:", res$status,
                "FOR", url_string),
          logfile,
          append=TRUE)
    print(paste(get_time_str(),
                "CODE:", res$status,
                "FOR", url_string))
    file_record <- paste(file_name, file_prefix, exchange, pair,
                         query_type, sep=";")
    write(file_record, register, append=TRUE)
  }
}

#================ END LOADING DATA==================================================




#' Get the file names of data located in a specified directory and with certain requirements
#'
#' This function gets the file names of data located in a specified directory and with certain requirements
#'
#' @param market is the required exchange
#' @param pair is the required exchange rate pair
#' @param type can be either "orderbook" or "trades"
#' @param file_extension is the file extension of the required dataset
#' @param sourceDirectory is the directory path where the data files are located
#' @return needed_files a list of all files
#' @details
#' This function gets the file names of data located in a specified directory and containing
#' certain keywords (for market, currency pair, data type), and file extension
#'
#' @export
#'
#' @examples
#' data_file<-system.file("extdata", package = "bitcoinFinance")
#' filenames_trades <- getFilenames("coinbase","btcusd","trades",".json", data_file )
#' filenames_orderbook <- getFilenames("coinbase","btcusd","orderbook",".json", data_file )

getFilenames <- function(market,
                         pair,
                         type,
                         file_extension,
                         sourceDirectory) {
  # Get the list of all files
  # Then run a sequence of greps to filter them
  all_files <- list.files(path=sourceDirectory)
  needed_files <- all_files[grep(market, all_files)]
  needed_files <- needed_files[grep(pair, needed_files)]
  needed_files <- needed_files[grep(type, needed_files)]
  needed_files <- needed_files[grep(file_extension,
                                    needed_files)]
  return (needed_files)
}




#' Extract the downloaded cryptocurriency trades data from the json files and save them into a csv file
#'
#' This function extracts the downloaded cryptocurriency trades data from the json files and save them into a csv file
#'
#' @param market is the required exchange
#' @param pair is the required exchange rate pair
#' @param sourceDirectory is the directory path where the json trades data files are located
#' @param destDirectory is the directory path where the csv tradesdata files will be saved
#' @details
#' This function extracts the downloaded cryptocurriency trades data from the json files and save them into a csv file
#'
#' @export
#' @importFrom rjson fromJSON
#' @importFrom utils write.csv
#'
#' @examples
#' \dontrun{
#' data_file<-system.file("extdata", package = "bitcoinFinance")
#' prepareTradesFromDirectory("coinbase","btcusd",data_file,data_file)
#' }


prepareTradesFromDirectory <- function(market,
                                       pair,
                                       sourceDirectory,
                                       destDirectory){
  # Get the list of files to process
  filenames <- getFilenames(market,
                            pair,
                            "trades",
                            ".json",
                            sourceDirectory)
  # Add path to source directory to file names
  filepaths <- sapply(filenames,
                      function(x) paste(sourceDirectory,
                                        x,
                                        sep="/"))
  # Fetch data
  # Create a prototype data frame for unique trades
  trades <- as.data.frame(matrix(nrow=0, ncol=5))
  colnames(trades) <- c("file", "id", "stamp",
                        "price", "amount")
  # Now process files, adding the result to the data frame
  for (filename in filepaths) {
    # Get the data form JSON
    raw_json <- rjson::fromJSON(file=filename)$result
    # Convert data to a frame and brush it up
    tmp_trades <- as.data.frame(t(as.data.frame(raw_json)))
    rownames(tmp_trades) <- NULL
    colnames(tmp_trades) <- c("id", "stamp", "price", "amount")
    tmp_trades$file <- as.factor(rep(filename,
                                     nrow(tmp_trades)))
    # Find out which trades should be added to
    # the data frame which contains unique trades
    req_trades <- sapply(tmp_trades$stamp,
                         function(x)
                           is.element(x,
                                      setdiff(tmp_trades$stamp,
                                              trades$stamp)))
    # Add new trades to the data frame
    trades <- rbind(trades, tmp_trades[req_trades, ])
  }
  # Fix time format
  trades$stamp <- as.POSIXct(trades$stamp,
                             origin="1970-01-01")

  # Create the name for the destination file
  destFile <- paste(market, pair, "trades.csv", sep="_")
  destFile <- paste(destDirectory, destFile, sep="/")
  # Save data
  write.csv(trades,
            file=destFile,
            row.names=FALSE)
}


#' Extract the downloaded cryptocurriency order-book data from the json files and save them into a csv file
#'
#' This function extracts the downloaded cryptocurriency order-book data from the json files and save them into a csv file
#'
#' @param market is the required exchange
#' @param pair is the required exchange rate pair
#' @param sourceDirectory is the directory path where the json order-book data files are located
#' @param destDirectory is the directory path where the csv order-book files will be saved
#' @details
#' This function extracts the downloaded cryptocurriency order-book data from the json files and save them into a csv file
#'
#' @export
#' @importFrom rjson fromJSON
#' @importFrom utils write.csv
#'
#' @examples
#' \dontrun{
#' data_file<-system.file("extdata", package = "bitcoinFinance")
#' prepareOrders("coinbase","btcusd",data_file,data_file)
#' }

prepareOrders <- function(market,
                          pair,
                          sourceDirectory,
                          destDirectory) {
  # Get filenames and prepare paths
  filenames <- getFilenames(market,
                            pair,
                            "orderbook",
                            ".json",
                            sourceDirectory)
  filepaths <- sapply(filenames,
                      function(x) paste(sourceDirectory,
                                        x,
                                        sep="/"))
  # Create an empty frame for the list of orders
  orders <- as.data.frame(matrix(nrow=0, ncol=5))
  colnames(orders) <- c("price",
                        "volume",
                        "level",
                        "stamp",
                        "type")
  # Now preprocess the found files
  for (filepath in filepaths){
    print(paste(filepath, "started"))
    # Extract the timestamp from the  filename
    stamp <- substr(basename(filepath), 1, 14)
    stamp <- as.POSIXct(stamp, format="%Y%m%d%H%M%S")


    #orderbookFromJSON(filepath, stamp)
    # Read the data file and retrieve bids and asks from it
    # After retrieving, coerce the data structure to
    # a data frame and brush it up
    raw_json <- rjson::fromJSON(file=filepath)
    asks_json <- raw_json$result$asks
    asks <- as.data.frame(t(as.data.frame(asks_json)))
    rownames(asks) <- NULL
    colnames(asks) <- c("prices", "volumes")
    bids_json <- raw_json$result$bids
    bids <- as.data.frame(t(as.data.frame(bids_json)))
    rownames(bids) <- NULL
    colnames(bids) <- c("prices", "volumes")
    # Ensuring order
    asks <- asks[order(asks$price),]
    asks$level <- rep(1:nrow(asks))
    bids <- bids[rev(order(bids$price)),]
    bids$level <- rep(1:nrow(bids))
    # Adding marks
    asks$stamp <- rep(stamp, nrow(asks))
    bids$stamp <- rep(stamp, nrow(bids))
    asks$type <- rep("ask", nrow(asks))
    bids$type <- rep("bid", nrow(bids))
    asks$stamp <- as.POSIXct(asks$stamp, format="%Y%m%d%H%M%S")
    bids$stamp <- as.POSIXct(bids$stamp, format="%Y%m%d%H%M%S")
    # Uniting into a single frame
    oBook <- rbind(asks, bids)

    orders <- rbind(orders, oBook)
    print(paste(filepath, "done"))
  }
  # Prepare the name for the file destination file
  destFile <- paste(market, pair, "orderbook.csv", sep="_")
  destFile <- paste(destDirectory, destFile, sep="/")
  # Save data
  write.csv(orders,
            file=destFile,
            row.names=FALSE)
}


#' Extract best bids and best asks from the orderbook.csv file
#'
#' This function extract best bids and best asks from the orderbook.csv file
#'
#' @param market is the required exchange
#' @param pair is the required exchange rate pair
#' @param sourceDirectory is the directory path where the csv order-book data files is located
#' (and where the best_orders.csv file will be saved)
#' @details
#' This function extracts best bids and best asks from the orderbook.csv file and save them in the best_orders.csv file
#'
#' @export
#' @importFrom utils write.csv
#' @importFrom utils read.csv
#'
#' @examples
#' \dontrun{
#' data_file<-system.file("extdata", package = "bitcoinFinance")
#' prepareOrders("coinbase","btcusd",data_file,data_file)
#' prepareBestOrders("coinbase","btcusd",data_file )
#' }

prepareBestOrders <- function(market,
                              pair,
                              sourceDirectory){

  # Read the orders file
  orderbookName <- paste(market, pair, "orderbook.csv",
                         sep="_")
  ordersFile <- paste(sourceDirectory, orderbookName, sep="/")
  orders <- read.csv(ordersFile, header=TRUE)
  # Collect the best bids and best asks
  bestOrders <- orders[orders$level==1, ]
  bestOrders$level <- NULL
  # Attach bids to asks
  bestAsks <- bestOrders[bestOrders$type=="ask", ]
  colnames(bestAsks) <- c("askPrice", "askVol", "stamp", "type")
  bestBids <- bestOrders[bestOrders$type=="bid", ]
  colnames(bestBids) <- c("bidPrice", "bidVol", "stamp", "type")
  bestAsks$type <- NULL
  bestBids$type <- NULL
  bestOrders <- merge(bestAsks, bestBids, by="stamp")
  bestOrders$stamp <- as.POSIXct(bestOrders$stamp,
                                 origin="1970-01-01")
  ## Prepare the name for the file destination file
  bestOrdersName <- paste(market, pair,"best_orders.csv", sep="_")
  bestOrdersFile <- paste(sourceDirectory, bestOrdersName, sep="/")
  write.csv(bestOrders,
            file=bestOrdersFile,
            row.names=FALSE)
}

#' Match last trades data with best bids and best asks
#'
#' This function matches last trades data with best bids and best asks
#'
#' @param market is the required exchange
#' @param pair is the required exchange rate pair
#' @param sourceDirectory is the directory path best_orders.csv and trades.csv files are located
#' (and where the matched.csv file will be saved)
#' @details
#' This function matches last trades data (from trades.csv) with best bids and
#'  best asks (from best_orders.csv) and save the matched data in the matched.csv file
#'
#' @export
#' @importFrom utils write.csv
#' @importFrom utils read.csv
#'
#' @examples
#' \dontrun{
#' data_file<-system.file("extdata", package = "bitcoinFinance")
#' prepareTradesFromDirectory("coinbase","btcusd",data_file,data_file)
#' prepareOrders("coinbase","btcusd",data_file,data_file)
#' prepareBestOrders ("coinbase","btcusd",data_file )
#' lastPriceToBestOrders ("coinbase","btcusd",data_file )
#' }

lastPriceToBestOrders <- function(market,
                                  pair,
                                  sourceDirectory) {
  #Read trades file
  tradesName <- paste(market, pair,"trades.csv",sep="_")
  tradesFile <- paste(sourceDirectory, tradesName,sep="/")
  dfTrades <- read.csv(file=tradesFile)
  #Read best orders file
  bestOrdersName <- paste(market, pair,"best_orders.csv", sep="_")
  bestOrdersFile <- paste(sourceDirectory, bestOrdersName, sep="/")
  bestOrders <- read.csv(file=bestOrdersFile)

  # extract the last trade in each sequence of trades with same timestamp
  dfTrades$stamp <- as.POSIXct(dfTrades$stamp,
                               origin="1970-01-01")
  dfTrades$nextTime <- as.POSIXct(c(tail(dfTrades$stamp, -1),0),
                                  origin="1970-01-01")
  dfTrades$isLast <- ifelse(dfTrades$stamp==dfTrades$nextTime, 0, 1)
  dfTrades <- dfTrades[dfTrades$isLast==1, ]
  dfTrades$isLast <- NULL
  dfTrades$nextTime <- NULL
  # Match trades from the trades dataframe to the
  # orderbook data frame
  bestOrders$stamp <- as.POSIXct(bestOrders$stamp,
                                 origin="1970-01-01")
  bestOrders$ptTime <- sapply( bestOrders$stamp,
                               function(x)
                                 max(dfTrades$stamp[dfTrades$stamp<=x]) )
  bestOrders$ptTime <- as.POSIXct(bestOrders$ptTime,
                                  origin="1970-01-01")
  # Merge dataframes by matched timestamps and brush up
  dfTrades$ptTime <- dfTrades$stamp
  dfTrades$stamp <- NULL
  dfOut <- merge(bestOrders, dfTrades, by="ptTime")
  dfOut$id <- NULL
  dfOut$file <- NULL
  # Write result to file
  matchedOrdersName <- paste(market, pair,"matched.csv", sep="_")
  matchedOrdersPath <- paste(sourceDirectory, matchedOrdersName, sep="/")
  write.csv(dfOut,
            file=matchedOrdersPath,
            row.names=FALSE)
}


#' Calculate liquidity measures based on trades
#'
#' This function calculates liquidity measures based on trades
#'
#' @param market is the required exchange
#' @param pair is the required exchange rate pair
#' @param sourceDirectory is the directory path where the  trades.csv file is located
#' @param destDirectory is the directory path where the trades_liquidity_measures.csv file will be saved
#' @return newData is a dataframe with the trade-based liquidity measures
#' @details
#' This function calculates liquidity measures based on trades
#'
#' @export
#' @importFrom utils write.csv
#' @importFrom utils read.csv
#' @importFrom utils head
#'
#' @examples
#' \dontrun{
#' data_file<-system.file("extdata", package = "bitcoinFinance")
#' prepareTradesFromDirectory("coinbase","btcusd",data_file,data_file)
#' prepareOrders("coinbase","btcusd",data_file,data_file)
#' prepareBestOrders ("coinbase","btcusd",data_file )
#' lastPriceToBestOrders ("coinbase","btcusd",data_file )
#' trades_liquidity<-calcTradeBased("coinbase","btcusd",data_file, data_file )
#' }

calcTradeBased <- function(market,
                           pair,
                           sourceDirectory,
                           destDirectory) {
  tradeBasedFile <- paste(market, pair, "trades.csv", sep="_")
  tradeBasedPath <- paste(sourceDirectory, tradeBasedFile, sep="/")
  # Read data and prepare formats
  rawData <- read.csv(tradeBasedPath, header=TRUE)
  rawData$stamp <- as.POSIXct(rawData$stamp,
                              origin="1970-01-01")
  # Add minute stamps
  rawData$newStamp <- as.POSIXlt(rawData$stamp)
  rawData$newStamp$sec <- 0
  rawData$newStamp <- as.POSIXct(rawData$newStamp,
                                 origin="1970-01-01")
  rawData$turnover <- rawData$amount * rawData$price
  rawData$lagPrice <- c(NA, head(rawData$price, -1))
  rawData$squaredDiff <- (rawData$price - rawData$lagPrice)^2
  # Get unique dates and prepare the new dataframe
  stamp <- unique(rawData$newStamp)
  tradeVolumes <- sapply(stamp,
                         function(x)
                           sum(rawData[
                             rawData$newStamp==x,
                             ]$amount))
  volumeDuration <- 1 / tradeVolumes
  turnover <- sapply(stamp,
                     function(x)
                       sum(rawData[
                         rawData$newStamp==x,
                         ]$turnover))
  turnoverDuration <- 1 / turnover
  numberOfTransactions <- sapply(stamp,
                                 function(x)
                                   nrow(rawData[
                                     rawData$newStamp==x,
                                     ]))
  WTt <- 1 / numberOfTransactions
  FRt <- tradeVolumes / WTt
  MartinIndex <- sapply(stamp,
                        function(x)
                          sum(rawData[
                            rawData$newStamp==x,
                            ]$squaredDiff))
  MartinIndex <- MartinIndex / tradeVolumes
  newData <- as.data.frame(cbind(stamp,
                                 tradeVolumes,
                                 volumeDuration,
                                 turnover,
                                 turnoverDuration,
                                 numberOfTransactions,
                                 WTt,
                                 MartinIndex,
                                 FRt))
  # Adjust the time stamps so that they are shown for the
  # minute that has just ended.
  newData$stamp <- as.POSIXlt(newData$stamp,
                              origin="1970-01-01")
  newData$stamp$min <- newData$stamp$min + 1
  newData$stamp <- as.POSIXct(newData$stamp,
                              origin="1970-01-01")
  #Save data
  tradeBasedLiquidity <- paste(market, pair, "trades_liquidity_measures.csv", sep="_")
  write.csv(newData,
            file=paste(destDirectory, tradeBasedLiquidity, sep="/"),
            row.names=FALSE)
  return(newData)
}

#' Calculate liquidity measures  based on best orders
#'
#' This function calculates liquidity measures  based on best orders
#'
#' @param market is the required exchange
#' @param pair is the required exchange rate pair
#' @param sourceDirectory is the directory path where the  best_orders.csv file is located
#' @param destDirectory is the directory path where the best_orders_liquidity_measures.csv file will be saved
#' @return df is a dataframe with liquidity measures based on best orders
#' @details
#' This function calculates liquidity measures  based on best orders
#'
#' @export
#' @importFrom utils write.csv
#' @importFrom utils read.csv
#'
#' @examples
#' \dontrun{
#' data_file<-system.file("extdata", package = "bitcoinFinance")
#' prepareTradesFromDirectory("coinbase","btcusd",data_file,data_file)
#' prepareOrders("coinbase","btcusd",data_file,data_file)
#' prepareBestOrders ("coinbase","btcusd",data_file )
#' lastPriceToBestOrders ("coinbase","btcusd",data_file )
#' trades_liquidity<-calcTradeBased("coinbase","btcusd",data_file, data_file )
#' bestorders_liquidity<-calcBestOrderBased("coinbase","btcusd",data_file, data_file )
#' }
calcBestOrderBased <- function(market,
                               pair,
                               sourceDirectory,
                               destDirectory) {
  # Reading all orders and preparing the dataframe
  bestOrdersBasedFile <- paste(market, pair,"best_orders.csv", sep="_")
  bestOrdersBasedPath <- paste(sourceDirectory, bestOrdersBasedFile,   sep="/")
  df <- read.csv(bestOrdersBasedPath, header=TRUE)
  df$level <- NULL
  # Start computations
  df$Dt <- df$askVol + df$bidVol
  df$Dlogt <- log(df$askVol * df$bidVol)
  df$Dbakst <- (df$askVol * df$askPrice +
                  df$bidVol * df$bidPrice) / 2
  df$Sabst <- df$askPrice - df$bidPrice
  df$LogSabst <- log(df$askPrice - df$bidPrice)
  df$SrelMt <- 2 * (df$askPrice -
                      df$bidPrice) / (df$askPrice + df$bidPrice)
  df$SrelLogt <- log(df$askPrice / df$bidPrice)
  df$LogSrelLogt <- log(df$SrelLogt)
  df$QSt <- df$Sabst / (log(df$askVol) + log(df$bidVol))
  df$LogQSt <- df$SrelLogt / df$Dlogt
  df$LogQSAdjt <- df$LogQSt * (1 + abs(log(df$bidVol
                                           / df$askVol)))
  df$CLt <- df$SrelMt / df$Dbakst
  #Save the data
  bestOrdersBasedLiquidity <- paste(market, pair, "best_orders_liquidity_measures.csv", sep="_")
  write.csv(df,
            file=paste(destDirectory, bestOrdersBasedLiquidity, sep="/"),
            row.names = FALSE)
  return(df)
}

#' Calculate liquidity measures  based on best orders and last trade
#'
#' This function calculates liquidity measures based on best orders and last trade
#'
#' @param market is the required exchange
#' @param pair is the required exchange rate pair
#' @param sourceDirectory is the directory path where the matched.csv file is located
#' @param destDirectory is the directory path where the matched_liquidity_measures.csv file will be saved
#' @return df is a dataframe with liquidity measures based on best orders and last trades
#' @details
#' This function calculates liquidity measures based on best orders and last trade
#'
#' @export
#' @importFrom utils write.csv
#' @importFrom utils read.csv
#'
#' @examples
#' \dontrun{
#' data_file<-system.file("extdata", package = "bitcoinFinance")
#' prepareTradesFromDirectory("coinbase","btcusd",data_file,data_file)
#' prepareOrders("coinbase","btcusd",data_file,data_file)
#' prepareBestOrders ("coinbase","btcusd",data_file )
#' lastPriceToBestOrders ("coinbase","btcusd",data_file )
#' trades_liquidity<-calcTradeBased("coinbase","btcusd",data_file, data_file )
#' bestorders_liquidity<-calcBestOrderBased("coinbase","btcusd",data_file, data_file )
#' bestorders_liquidity_trades<-calcBestOrdersAndTradesBased("coinbase","btcusd",data_file, data_file )
#' }
calcBestOrdersAndTradesBased <- function(market,
                                         pair,
                                         sourceDirectory,
                                         destDirectory) {
  bestOrdersAndTradesFile <- paste(market, pair,"matched.csv", sep="_")
  bestOrdersAndTradesPath <- paste(sourceDirectory, bestOrdersAndTradesFile, sep="/")

  df <- read.csv(bestOrdersAndTradesPath, header=TRUE)
  df$Srelpt <- (df$askPrice - df$bidPrice) / df$price
  df$Sefft <- abs(df$price - (df$askPrice + df$bidPrice) / 2)
  df$Seffrelpt <- df$Sefft / df$price
  df$SeffrelMt <- df$Sefft / ((df$askPrice + df$bidPrice) / 2)
  #save data
  bestOrdersAndTradesLiquidity <- paste(market, pair, "matched_liquidity_measures.csv", sep="_")
  write.csv(df,
            file=paste(destDirectory, bestOrdersAndTradesLiquidity,
                       sep="/"),
            row.names=FALSE)

  return(df)
}
