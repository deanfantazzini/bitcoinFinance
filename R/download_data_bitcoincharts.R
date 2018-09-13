#' Download a pre-specified number of random bitcoin trades time series from api.bitcoincharts.com
#'
#' This function downloads a pre-specified number of random bitcoin trades series  from api.bitcoincharts.com
#'
#' @param number_of_series_needed is the desidered number of (randomly selected) bitcoin trades series
#' @param exceptions is a string vector of the files not to be (potentially) selected from api.bitcoincharts.com due to their large dimensions
#' @param start_date this is the minimum starting date required
#' @param end_date this is the maximum end date required
#' @param save_data if TRUE, then the downloaded trades series will be saved in the bitcoin_data.rds file (in the Working Directory)
#' @return data_list is a list containting the dataframes of several time series data downloaded from bitcoincharts. Each dataframe has
#' the 'timestamp', 'price', 'volume' for the selected bitcoin trades time series.
#' @details
#' This function downloads a pre-specified number of random bitcoin trades time series from api.bitcoincharts.com with a
#' required minimum starting date and a required ending date
#'
#'
#' @export
#' @importFrom XML htmlTreeParse
#' @importFrom utils download.file
#' @importFrom utils read.table
#' @importFrom XML xpathApply
#' @importFrom XML xmlValue
#' @importFrom stringr str_split
#'
#' @examples
#' \dontrun{
#' aa <-bitcoincharts_random_download(number_of_series_needed = 3, save_data=TRUE)
#' str(aa)
#' }
#'
bitcoincharts_random_download <- function(number_of_series_needed = 5,
                                      exceptions = c("okcoinCNY.csv.gz", "coincheckJPY.csv.gz"),
                                      start_date = "2015-08-03", #this is the minimum starting date
                                      end_date = "2017-07-01",    #this is the maximum end date
                                      save_data = FALSE
                                      ){

  link <- 'http://api.bitcoincharts.com/v1/csv/'
  # Read and parse HTML file
  doc.html <- XML::htmlTreeParse(link, useInternal = TRUE)
  # Extract all the paragraphs (HTML tag is a, starting at
  # the root of the document). Unlist flattens the list to
  # create a character vector.
  links <- unlist(XML::xpathApply(doc.html, '//a', xmlValue))

  collected_series <- 0
  data_list <- list()

  for (l in sample(links)) {
    if (grepl('.csv.gz', l) && !(l %in% exceptions) ) {
      link_to_file <- paste0(link, l)
      temp_file <- tempfile()
      print(paste('downloading', l, sep = ' '))
      utils::download.file(link_to_file, temp_file)

      if (length(readLines(gzfile(temp_file), n = 1)) == 0) {
        print(paste(l, 'is an empty file', sep = ' '))
        next
      }

      data <- utils::read.table(gzfile(temp_file), sep = ',', dec = '.', header = F, stringsAsFactors = F)
      unlink(temp_file)
      #check that the data have the minimum length required
      if ( as.Date(as.POSIXct(data[nrow(data), 1], origin="1970-01-01")) < end_date ||
           as.Date(as.POSIXct(data[1, 1], origin="1970-01-01")) > start_date ) {
        print(paste(l, 'has not enough data', sep = ' '))
        next
      }
      names(data) <- c('timestamp', 'price', 'volume')
      data$timestamp <- as.POSIXct(data$timestamp, origin="1970-01-01")
      data_list[[collected_series + 1]] <- data
      names(data_list)[length(data_list)] <- stringr::str_split(l, '\\.', simplify = T)[1, 1]
      collected_series <- collected_series + 1
      print(paste(collected_series, 'series collected', sep = ' '))
    }
    if (collected_series >= number_of_series_needed) break
  }
  #if desidered, save data in the default directory
  if (save_data) saveRDS(data_list, 'bitcoin_data.rds')
  #remove unuseful objects
  rm(collected_series,data, doc.html,l,link,link_to_file,links,temp_file)
  return(data_list)
}



#' Download a pre-specified bitcoin currency pair time series from api.bitcoincharts.com
#'
#' This function downloads a pre-specified bitcoin currency pair time series from api.bitcoincharts.com
#'
#' @param name is the name of the file to be downloaded from api.bitcoincharts.com
#' @param start_date this is the minimum starting date required
#' @param end_date this is the maximum end date required
#' @param save_data if TRUE, then the downloaded trades price series will be saved in the bitcoin_data.rds file (in the Working Directory)
#' @return data is a dataframe containing 'timestamp', 'price', 'volume' for the selected bitcoin trades time series.
#' @details
#' This function downloads a pre-specified bitcoin currency pair time series  from api.bitcoincharts.com with a
#' required minimum starting date and a required ending date
#'
#'
#' @export
#' @importFrom XML htmlTreeParse
#' @importFrom utils download.file
#' @importFrom utils read.table
#'
#'
#' @examples
#' \dontrun{
#' bb <-bitcoincharts_single_download(name = "coinbaseEUR.csv.gz", save_data=TRUE)
#' str(bb)
#' }
#'
bitcoincharts_single_download <- function(name="bitstampUSD.csv.gz",
                                          start_date = "2015-08-03", #this is the minimum starting date
                                          end_date = "2017-07-01",    #this is the maximum end date
                                          save_data = FALSE){

  link <- 'http://api.bitcoincharts.com/v1/csv/'
  link_to_file <- paste0(link, name)
  temp_file <- tempfile()
  print(paste('downloading', name, sep = ' '))
  utils::download.file(link_to_file, temp_file)

  if (length(readLines(gzfile(temp_file), n = 1)) == 0) {
       print(paste(name, 'is an empty file', sep = ' '))
       stop()
  }

  data <- utils::read.table(gzfile(temp_file), sep = ',', dec = '.', header = F, stringsAsFactors = F)
  unlink(temp_file)
  #check that the data have the minimum length required
  if ( as.Date(as.POSIXct(data[nrow(data), 1], origin="1970-01-01")) < end_date ||
       as.Date(as.POSIXct(data[1, 1], origin="1970-01-01")) > start_date ) {
       print(paste(name, 'has not enough data', sep = ' '))
       stop()
  }
  names(data) <- c('timestamp', 'price', 'volume')
  data$timestamp <- as.POSIXct(data$timestamp, origin="1970-01-01")

  #if desired, save data in the default directory
  if (save_data) saveRDS(data, 'bitcoin_data.rds')
  #remove unuseful objects
  rm(link,link_to_file,temp_file)
  return(data)
}
