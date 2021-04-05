#' Download a pre-specified bitcoin currency pair time series from api.bitcoincharts.com
#'
#' This function downloads a pre-specified bitcoin currency pair time series from api.bitcoincharts.com and it is suitable for large files
#'
#' @param address is the directory address where to save the data
#' @param name is the name of the file to be downloaded from api.bitcoincharts.com
#' @param save_data if TRUE, then the downloaded trades price series will be saved in the bitcoin_data.rds file (in the Working Directory)
#' @return data is a dataframe containing 'timestamp', 'price', 'volume' for the selected bitcoin trades time series.
#' @details
#' This function downloads a pre-specified bitcoin currency pair time series  from api.bitcoincharts.com .
#' It is better to use this function for downloading large files instead of bitcoincharts_single_download,
#' which may gives an error (depending on the platform used).
#'
#'
#' @export
#' @importFrom utils download.file
#' @importFrom utils read.table
#'
#'
#' @examples
#' \dontrun{
#' bb <-bitcoincharts_download_large(name = "coinbaseEUR.csv.gz", save_data=TRUE)
#' str(bb)
#' }
#'

bitcoincharts_download_large <- function(address="D:/Downloads/",name="bitchangePLN.csv.gz", save_data = FALSE){
  destfile = paste0(address, name)
  link_to_file = paste0('http://api.bitcoincharts.com/v1/csv/', name)
  utils::download.file(link_to_file,destfile=destfile,method="libcurl")
  data = utils::read.table(gzfile(destfile), sep = ',', dec = '.', header = F, stringsAsFactors = F)
  names(data) = c('timestamp', 'price', 'volume')
  data$timestamp = as.POSIXct(data$timestamp, origin="1970-01-01")
  #if desired, save data in the default directory
  if (save_data) saveRDS(data, 'bitcoin_data.rds')
  return(data)
}

