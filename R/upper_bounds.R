

#' Compute the bitcoin upper bound with user-delivered inputs
#'
#' This function computes the bitcoin upper bound with inputs given by the user and not taken from the web
#'
#' @param HSC.ratio is the US household checking deposits and cash / US personal consumption ratio
#' @param Ecommerce is the total B2C e-commerce sales (in billions)
#' @param bitcoinshare is the bitcoin share of of all on-line shopping
#' @param GDP.ratio is the World GDP / US GDP ratio
#' @param metrics is a vector cointaining the market capitalization of Western Union, MoneyGram and Euronet (in billions)
#' @param price.silver is the price of a silver eagle coin
#' @param total.silver.eagles is the total US silver eagle coints minted since 1986
#' @param TOTBC.u is the total number of bitcoin in circulation
#' @return x the bitcoin upper bound
#' @details
#' This function computes the bitcoin upper bound with inputs given by the user
#'
#'
#' @export
#'
#' @references Woo D., Gordon I., Iaralov V. (2013). Bitcoin: a first assessment. FX and Rates, December 2013, Bank of America Merrill Lynch.
#'
#' @examples
#' #This reproduces the example in Woo et al.(2013).
#' #It uses data up to December 2012.
#'  btc.upper.bound.user(HSC.ratio=0.04, Ecommerce= 224, bitcoinshare=0.1, GDP.ratio=5,
#'  metrics=c(8.5, 0.8, 4.2), price.silver=29, total.silver.eagles=337031982, TOTBC.u=10613175)
#' #  1398.673
#'

btc.upper.bound.user=function(HSC.ratio=0.04, Ecommerce= 224, bitcoinshare=0.1, GDP.ratio=5,
                              metrics=c(9, 0.9, 4.5), price.silver=17,  total.silver.eagles=400000000, TOTBC.u=11000000){

  V_ecomm.u <- HSC.ratio * Ecommerce * bitcoinshare * GDP.ratio
  V_moneytransfer.u <- mean(metrics)
  V_storeofvalue.u=0.6 * price.silver * total.silver.eagles/1000000000

  x <- (V_ecomm.u + V_moneytransfer.u + V_storeofvalue.u)*1000000000/ TOTBC.u
  return(x)
}



#' Compute the bitcoin upper bound with web-delivered inputs
#'
#' This function computes the bitcoin upper bound with inputs taken from the web
#'
#' @param fredkey is the FRED API KEY (requires registration)
#' @param start.date is the starting date (format "%Y-%m-%d") for downloading the US household checking deposits and cash and
#'  US personal consumption data from the FRED database
#' @param end.date is the ending date (format "%Y-%m-%d") for downloading the US household checking deposits and cash and
#'  US personal consumption data from the FRED database
#' @param lastyear represents the last year to be used for computing the upper bound. If NULL, the last available data for each
#' categories will be used (which may be different across categories).Otherwise a common year equal to lastyear
#' will be used for all categories: in this case be sure the the common year is available for all the 3 categories used
#' to compute the upper bound, otherwise you will get an error.
#' @return btc.upper.bound.web is the bitcoin upper bound
#' @details
#' This function computes the bitcoin upper bound with inputs taken from the web
#'
#'
#' @export
#' @importFrom fredr fredr_set_key
#' @importFrom fredr fredr
#' @importFrom plyr mutate
#' @importFrom quantmod yahooQF
#' @importFrom quantmod getQuote
#' @importFrom Quandl Quandl
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @importFrom xts last
#'
#' @examples
#' \dontrun{
#'  #Here insert your valid FRED key
#'  fredkey ="5565e828f27269b6ed4f3c2552d0cc8f"
#'  btc.upper.bound.web(fredkey)
#' }


btc.upper.bound.web=function(fredkey,start.date="2004-01-01",end.date="2014-12-31",
                             lastyear=NULL){
  #==============================================
  #   VEcommerce
  #==============================================
  if (missing(fredkey))
    stop("Need to specify the FRED key!")
  fredr::fredr_set_key(fredkey)

  #Personal consumtion - Billions of Dollars
  df1 <- fredr::fredr(series_id = "PCE",
                      observation_start = as.Date(start.date),
                      observation_end = as.Date(end.date),
                      aggregation_method = "avg",
                      frequency = "a")

  #Checking deposits and cash  - Billions of Dollars
  df2 <- fredr::fredr(series_id = "CDCABSHNO",
                      observation_start = as.Date(start.date),
                      observation_end = as.Date(end.date),
                      aggregation_method = "avg",
                      frequency = "a")

  #US GDP - Billions of Dollars
  df3 <- fredr::fredr(series_id = "GDP",
                      observation_start = as.Date(start.date),
                      observation_end = as.Date(end.date),
                      aggregation_method = "avg",
                      frequency = "a")

  #WORLD GDP - Current Dollars
  df4 <- fredr::fredr(series_id = "MKTGDP1WA646NWDB",
                      observation_start = as.Date(start.date),
                      observation_end = as.Date(end.date),
                      aggregation_method = "avg",
                      frequency = "a")

  #Transform world GDP into billions
  df4 <- plyr::mutate(df4, W.GDP = df4[,3]/1000000000)


  #E-commerce - billions Dollars
  df5 <- fredr::fredr(series_id = "ECOMSA",
                      observation_start = as.Date(start.date),
                      observation_end = as.Date(end.date),
                      aggregation_method = "avg",
                      frequency = "a")
  df5[,3] <- df5[,3]*4/1000


  # Final dataset
  start.date<-as.Date(start.date,'%Y-%m-%d')
  end.date<-as.Date(end.date,'%Y-%m-%d')
  start.year<-as.numeric(format(start.date,'%Y'))
  end.year<-as.numeric(format(end.date,'%Y'))
  datafr <- as.data.frame(cbind(start.year:end.year,df1[,3],df2[,3],df3[,3],df4$W.GDP,df5[,3]))
  datafr <- plyr::mutate(datafr, ratio = datafr[,3]/datafr[,2],
                         GDPratio = datafr[,5]/datafr[,4])
  names(datafr) <- c("year", "C_US","HD_US","GDP_US","GDP_world","Ecommerce",
                     "HSC.ratio", "GDP.ratio")
  datafr

  # Calculation of V_ecomm: if lastyear=null, use the last available date
  if(is.null(lastyear)){
    V_ecomm <- mean(datafr$HSC.ratio) *
      datafr[datafr$year==xts::last(datafr$year), ]$Ecommerce * 0.1 *
      datafr[datafr$year==xts::last(datafr$year), ]$GDP.ratio
  }
  # Calculation of V_ecomm: if lastyear not null, use the provided last year.
  if(!is.null(lastyear)){
    V_ecomm<-mean(datafr$HSC.ratio)*datafr[datafr$year==lastyear, ]$Ecommerce*0.1         *datafr[datafr$year==lastyear, ]$GDP.ratio
  }

  #===================================================
  #   Vmoney transfer (only latest data are available)
  #===================================================
  what_metrics <- quantmod::yahooQF("Market Capitalization")

  tickers <- c("WU", "MGI", "EEFT" )
  # Not all the metrics are returned by Yahoo.
  metrics <- quantmod::getQuote(paste(tickers, sep="", collapse=";"),
                                what=what_metrics)
  # MoneyGram agreed to be acquired for $1 billion in cash by Madison Dearborn Partners (a private equity firm)-no more traded
  metrics$`Market Capitalization`[2]<-1000000000
  #Add tickers as the first column and remove the 1st column which had date stamps
  metrics <- data.frame(Symbol=tickers, metrics[,2:length(metrics)])

  #Change colnames
  colnames(metrics) <- c("Symbol","Market_Cap")
  V_moneytransfer<- mean(metrics[,2])/1000000000

  #==============================================
  #   Value as STORE OF VALUE
  #==============================================
  mysil = Quandl::Quandl("LBMA/SILVER", collapse="annual")
  mysil$Date <- format(mysil$Date, "%Y")

  # Calculation of V_ecomm: if lastyear=null, use the last available date
  if(is.null(lastyear)){
    required.year.silver <- mysil[mysil$Date %in% max(mysil$Date),]
  }
  # Calculation of V_ecomm: if lastyear not null, use the provided last year
  if(!is.null(lastyear)){
    required.year.silver <- mysil[mysil$Date %in% as.character(lastyear),]
  }

  silver<- xml2::read_html("https://en.wikipedia.org/wiki/American_Silver_Eagle")
  table.silver <-rvest::html_nodes(silver,"table")
  table.silver <- rvest::html_table(table.silver[[2]])
  total.silver.eagles <- as.numeric(as.character(
    gsub(',','',table.silver[table.silver$Year %in% "Total","Total"])))
  V_storeofvalue=0.6*required.year.silver$USD*total.silver.eagles/1000000000
  V_storeofvalue

  #==============================================
  #   UPPER BOUND
  #==============================================
  mydata = Quandl::Quandl("BCHAIN/TOTBC", collapse="annual")
  mydata$Date <- format(mydata$Date, "%Y")
  #if lastyear=null, use the last available date
  if(is.null(lastyear)){
    required.year.TOTBC <- mydata[mydata$Date %in% max( mydata$Date),]
  }
  #if lastyear not null, use the provided last year
  if(!is.null(lastyear)){
    required.year.TOTBC <- mydata[mydata$Date %in% as.character(lastyear),]
  }
  btc.upper.bound.web <- (V_ecomm+V_moneytransfer+V_storeofvalue)*1000000000 /
    required.year.TOTBC$Value
  return(btc.upper.bound.web)
}
