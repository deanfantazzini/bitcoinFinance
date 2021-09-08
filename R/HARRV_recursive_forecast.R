#' Compute the recursive forecasts for the HAR-RV model.
#'
#' This function computes the recursive forecasts for the HAR-RV model
#'
#' @param dat is a xts object containing intraday 5-minute regularly spaces prices (see example below)
#' @param periods is a vector of integers indicating over how days the realized measures in the model should be aggregated.
#' By default periods = c(1,5,22). It is needed for the computation of the \link[highfrequency]{HARmodel} of the \code{highfrequency} package
#' @param type is a string referring to the type of HAR model you would like to estimate using \link[highfrequency]{HARmodel}. By default type = "HAR".
#' @param transform optionally a string referring to a function that transforms both the dependent and explanatory variables in the \link[highfrequency]{HARmodel}.
#'  By default transform=NULL, so no transformation is done. Typical other choices in this context would be "log" or "sqrt".
#' @param roll.window is the rolling window size used for estimating the HAR-RV model
#' @param h is the desired forecasting horizon. Default is 1 (that is, 1-step-ahead recursive forecasts)
#' @return roll.fore a xts object containing the h-day-ahead RV forecasts and the realized RV forecasts.
#' @details
#' This function computes the recursive forecasts for the HAR-RV model.
#' See Chevillon (2007, 2016) for a discussion about direct and recursive forecasts.
#'
#'
#' @export
#' @importFrom xts xts rbind.xts
#' @importFrom zoo index
#' @importFrom lubridate days
#' @importFrom zoo rollapply coredata
#' @importFrom highfrequency makeReturns HARmodel
#' @importFrom stats lm as.formula predict na.omit window
#'
#' @references Chevillon, G. (2007). Direct multi-step estimation and forecasting. Journal of Economic Surveys,21(4),746-785.
#' @references Chevillon, G. (2016). Multistep forecasting in the presence of location shifts. International Journal of Forecasting, 32(1), 121-137.
#'
#' @examples
#' \dontrun{
#' library(highfrequency)
#' aa <-bitcoincharts_single_download(name = "bitstampUSD.csv.gz", save_data=TRUE)
#' data_clean<-aggregate_merge_bictoincharts_data(data_list=list(aa=aa), aggregate_every = 5)
#' dat<-data_clean$price_ts
#' dat<- dat["2013-01-02/2017-07-12"]
#' bb<-HARRV.recursive.forecast(dat=dat)
#' tail(bb)
#'  #               realized   predicted
#'  #2017-07-07 0.0008078943 0.003320551
#'  #2017-07-08 0.0010431969 0.002369109
#'  #2017-07-09 0.0008631436 0.002488993
#'  #2017-07-10 0.0021875888 0.002373416
#'  #2017-07-11 0.0039407938 0.003152035
#'  #2017-07-12 0.0025053951 0.004210418
#' }

HARRV.recursive.forecast <- function(dat,periods=c(1,5,22),type="HAR",
                                  transform=NULL, roll.window = 1621, h=1){

  #HAR-RV
  dat_ret <- highfrequency::makeReturns(dat)
  btc_harrv <- highfrequency::HARmodel(data=dat_ret,periods=periods,type=type,h=1,transform=NULL, inputType = "returns")
  daily_dat<-xts::xts(btc_harrv$model, order.by = btc_harrv$dates)
  zoo::index(daily_dat)<-as.Date(zoo::index(daily_dat))
  names_for_eq <- colnames(daily_dat)
  formula_RV<-stats::as.formula( paste(names_for_eq[1], paste0(names_for_eq[2:length(names_for_eq)], collapse="+"), sep = '~') )

  prediction_recursive<-function(series){
    mod <- stats::lm(formula = formula_RV, data = series)
    date_last<-zoo::index(last(series))
    nextOb <-  date_last + lubridate::days(1) # To get the first row that follows the window
    if ( nextOb<=zoo::index(last(daily_dat)) ){   # You won't predict the last one
      # t+1
      fore_all<-matrix(NA, ncol = 5, nrow=h)
      predicted <- stats::predict( mod,newdata=data.frame(daily_dat[nextOb,c("y","RV1","RV5","RV22")]) )
      realized<-zoo::coredata(daily_dat[nextOb,"y"])
      fore_all[1,] <-cbind(realized, predicted, coredata(daily_dat[nextOb,c("RV1","RV5","RV22")]) )
      colnames(fore_all) <- c("y","y_hat","RV1","RV5","RV22")
      series_new<-xts::rbind.xts(series, xts::xts( cbind(predicted, daily_dat[nextOb,c("RV1","RV5","RV22")]), nextOb ))

      if (h>1){
        # t+2 : t+h
        for(i in 2:h){
          nextOb_h <-  date_last+lubridate::days(i)
          new_RV5 <-mean(window(series_new[,"y"], start=(nextOb_h-lubridate::days(5)), end=nextOb_h-lubridate::days(1)) )
          new_RV22<-mean(window(series_new[,"y"], start=(nextOb_h-lubridate::days(22)), end=nextOb_h-lubridate::days(1)) )
          realized<-zoo::coredata(daily_dat[nextOb_h,"y"])
          series_new<-xts::rbind.xts(series_new, xts::xts( cbind(realized, predicted, new_RV5, new_RV22), nextOb_h ))
          predicted <- stats::predict( mod,newdata=data.frame(series_new[nextOb_h,c("y","RV1","RV5","RV22")]) )
          fore_all[i,] <- cbind(realized, predicted, coredata(series_new[nextOb_h,c("RV1","RV5","RV22")]) )
          series_new[nextOb_h,]<- xts::xts( cbind(predicted, series_new[nextOb_h,c("RV1","RV5","RV22")]), nextOb_h )
        }
      }
      dat_pred<-xts::xts(fore_all, zoo::index(series_new[(nrow(series_new)-h+1):nrow(series_new), ]))
      dat_pred<-as.numeric( last(dat_pred[,c("y","y_hat")]) )
      names(dat_pred)=c("realized", "predicted")
      return(dat_pred)
    }else{stop("The data requested for the forecast is not present in the dataset!")}
  }

  roll.fore<-zoo::rollapply( daily_dat[1:(nrow(daily_dat)-h),], width=roll.window, FUN=prediction_recursive, by.column=F, align='right')
  roll.fore<-xts::xts( roll.fore, zoo::index(daily_dat)[(1+h):nrow(daily_dat)] )
  return(roll.fore)
}
