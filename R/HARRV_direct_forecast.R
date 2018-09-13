#' Compute the direct forecasts for the HAR-RV model.
#'
#' This function computes the direct forecasts for the HAR-RV model
#'
#' @param dat is a xts object containing intraday 5-minute regularly spaces prices (see example below)
#' @param periods is a vector of integers indicating over how days the realized measures in the model should be aggregated.
#' By default periods = c(1,5,22). It is needed for the computation of the \link[highfrequency]{harModel} of the \code{highfrequency} package
#' @param type is a string referring to the type of HAR model you would like to estimate using \link[highfrequency]{harModel}. By default type = "HARRV".
#' @param transform optionally a string referring to a function that transforms both the dependent and explanatory variables in the \link[highfrequency]{harModel}.
#'  By default transform=NULL, so no transformation is done. Typical other choices in this context would be "log" or "sqrt".
#' @param roll.window is the rolling window size used for estimating the HAR-RV model
#' @param lag_fore is the desired forecasting horizon. Default is 1 (that is, 1-step-ahead direct forecasts)
#' @return roll.fore a xts object containing the h-day-ahead RV forecasts and the realized RV forecasts.
#' @details
#' This function computes the direct forecasts for the HAR-RV model. The benefits of direct forecasts are described in
#' Chevillon (2007, 2016).
#'
#'
#' @export
#' @importFrom xts as.xts lag.xts merge.xts xts
#' @importFrom zoo index
#' @importFrom lubridate days
#' @importFrom zoo rollapply
#' @importFrom highfrequency makeReturns harModel
#' @importFrom stats lm as.formula predict na.omit
#'
#' @references Chevillon, G. (2007). Direct multi-step estimation and forecasting. Journal of Economic Surveys,21(4),746-785.
#' @references Chevillon, G. (2016). Multistep forecasting in the presence of location shifts. International Journal of Forecasting, 32(1), 121-137.
#'
#' @examples
#' \dontrun{
#' aa <-bitcoincharts_single_download(name = "bitstampUSD.csv.gz", save_data=TRUE)
#' data_clean<-aggregate_merge_bictoincharts_data(data_list=list(aa=aa), aggregate_every = 5)
#' dat<-data_clean$price_ts
#' dat<- dat["2013-01-02/2017-07-12"]
#' bb<-HARRV.direct.forecast(dat=dat)
#' tail(bb)
#'  #                     predicted     realized
#'  # 2017-07-07 23:55:00 0.003320551 0.0008078943
#'  # 2017-07-08 23:55:00 0.002369109 0.0010431969
#'  # 2017-07-09 23:55:00 0.002488993 0.0008631436
#'  # 2017-07-10 23:55:00 0.002373416 0.0021875888
#'  # 2017-07-11 23:55:00 0.003152035 0.0039407938
#'  # 2017-07-12 23:55:00 0.004210418 0.0025053951
#' }

HARRV.direct.forecast <- function(dat,periods=c(1,5,22),type="HARRV",
                                                   transform=NULL, roll.window = 1621, lag_fore=1){
  #HAR-RV
  dat_ret = highfrequency::makeReturns(dat)
  btc_harrv<- highfrequency::harModel(data=dat_ret,periods=periods,type=type,h=1,transform=NULL)
  daily_dat<-xts::as.xts(btc_harrv$model)
  names_for_eq <- colnames(daily_dat)
  lagged<-xts::lag.xts(daily_dat[,2:length(names_for_eq)], k=lag_fore-1, na.pad=FALSE)
  lagged<- na.omit( merge.xts(daily_dat[,1], lagged) )
  formula_RV<-stats::as.formula( paste(names_for_eq[1], paste0(names_for_eq[2:length(names_for_eq)], collapse="+"), sep = '~') )

  prediction<-function(series){
    mod <- stats::lm(formula = formula_RV, data = series)
    date_last<-zoo::index(last(series))
    nextOb <-  date_last+lubridate::days(1) # To get the first row that follows the window
    if ( nextOb<=zoo::index(last(daily_dat)) ){   # You won't predict the last one
      # make forecast
      predicted <- predict( mod,newdata=data.frame(lagged[nextOb,c("y","x.RV1","x.RV5","x.RV22")]) )
      dat_pred<-c( predicted ,daily_dat[nextOb,'y'])
      names(dat_pred)=c("predicted", "realized")
    return(dat_pred)
    }else{stop("The data requested for the forecast is not present in the dataset!")}
  }

  roll.fore<-zoo::rollapply( lagged[1:(nrow(daily_dat)-lag_fore),], width=roll.window, FUN=prediction, by.column=F, align='right')
  roll.fore<-xts::xts( roll.fore, index(daily_dat)[(1+lag_fore):nrow(daily_dat)] )
  return(roll.fore)
}


