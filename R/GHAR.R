#' Compute the GHAR model for realized covariances and returns the 1-day-ahead VaR forecasts
#' and the 1-day-ahead Realized Covariance forecast.
#'
#' This function computes the GHAR model for realized covariances and returns the 1-day ahead
#' VaR forecasts (for an equally-weighted portfolio), as well as the the 1-day-ahead Realized Covariance forecast.
#'
#' @param cleaned.rc is a list containing the realized covariances
#' @param daily_ret is an xts object containing the cryptocurrency daily returns. It should be compatible
#' with the list containing the realized covariances
#' @param weights is a vector of weights for the porfolio VaR
#' @param quantiles the desidered quantiles for the VaR estimates
#' @return results a list containing the 1-day-ahead VaR forecasts, 1-day-ahead Realized Covariance forecast,
#' and the GHAR.fit object computed with the systemfit package
#' @details
#' This function computes the GHAR model for realized covariances by Cech and Baruník (2017)
#' and returns the 1-day ahead #' VaR forecasts (for an equally-weighted portfolio - default case),
#' the 1-day-ahead Realized Covariance forecast, and the GHAR.fit object computed with the systemfit package
#'
#'
#' @export
#' @importFrom xts xts
#' @importFrom xts last
#' @importFrom zoo zoo
#' @importFrom zoo index
#' @importFrom zoo coredata
#' @importFrom zoo rollmean
#' @importFrom systemfit createSystemfitModel
#' @importFrom systemfit systemfit
#' @importFrom stats qnorm
#' @importFrom lubridate days
#' @importFrom stats as.formula
#' @importFrom stats predict
#'
#' @references Cech, Frantisek, and Jozef Baruník (2017). On the Modelling and Forecasting of Multivariate Realized Volatility: Generalized Heterogeneous Autoregressive (GHAR) Model. Journal of Forecasting 36(2), 181-206.
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
#' cleaned.rc <- rc_pd(data_clean$price_ts)
#' GHAR_fit <- GHAR.model(cleaned.rc, data_clean$daily_returns, weights =rep(0.2,5))
#' }
#'

GHAR.model <- function(cleaned.rc, daily_ret, weights, quantiles = c(0.05, 0.1)){

  if( NROW(cleaned.rc)!= NROW(daily_ret) )
    stop("List of Realized Covariances and and the xts object of daily returns are not conformable")

  if(missing(weights))
    stop("The porfolio weights are missing!")

  #Compute Cholesly factors
  chol_m <- do.call( rbind, lapply(as.list(1:length(cleaned.rc)), function(x) {
    tmp <- chol(cleaned.rc[[x]])
    tmp[upper.tri(tmp, diag = T)]
  }) )

  gc(reset = T)

  # Create matrix of data using Cholesly factors
  chol_m <- zoo::zoo(x = chol_m, order.by = zoo::index(daily_ret))
  # Adding new day for prediction purposes below
  rollmean_lag_value_1 <- 5
  rollmean_lag_value_2 <- 22
  chol_m <- rbind(chol_m,
                zoo::zoo(x = matrix(rep(NA, ncol(chol_m)), nrow = 1),
                    order.by = xts::last(zoo::index(daily_ret)) + lubridate::days(1)))
  chol_m_lag1 <- zoo::zoo(x = zoo::coredata(chol_m)[-nrow(chol_m), ],
                   order.by = zoo::index(chol_m)[-1])
  chol_m_lag5 <- zoo::rollmean(chol_m_lag1, k = rollmean_lag_value_1, align = 'right')
  chol_m_lag22 <- zoo::rollmean(chol_m_lag1, k = rollmean_lag_value_2, align = 'right')

  data_m <- merge(chol_m, chol_m_lag1, chol_m_lag5, chol_m_lag22, all = FALSE)
  data_df <- data.frame(data_m)
  # Adjusting daily returns data accordingly
  daily_ret <- daily_ret[-(1:rollmean_lag_value_2-1)]


  # Create GHAR formula objects for systemfit function
  GHAR <- systemfit::createSystemfitModel(nEq = ncol(chol_m), nRegEq = 3, 100)
  # creating GHAR formulas
  formula_list <- lapply(as.list(1:ncol(chol_m)), function(x) {
    names_for_eq <- names(data_m)[0:3 * ncol(chol_m) + x]
    stats::as.formula(paste(names_for_eq[1], paste(names_for_eq[-1], collapse = '+'), sep = '~'))
  })
  GHAR$formula <- formula_list
  #Inserting data into GHAR object
  GHAR$data <- data_df[-nrow(data_df), ]


  #Estimate SUR model
  GHAR.fit <- systemfit(GHAR$formula, 'SUR', data = GHAR$data)

  #compute 1-step ahead forecast
  pred_chol <- matrix(0, ncol(daily_ret), ncol(daily_ret))
  pred_chol[upper.tri(pred_chol, diag = T)] <- as.numeric(stats::predict(GHAR.fit, newdata = data_df[nrow(data_df), ]))
  pred_cov_1step <- t(pred_chol) %*% pred_chol

  # VaR values at 5% and 10%
  VaR_1step <- qnorm( quantiles ) * sqrt(t(weights) %*% pred_cov_1step %*% weights)
  results <-list(VaR_1step=VaR_1step, pred_cov_1step=pred_cov_1step, GHAR.fit=GHAR.fit)
  return(results)
}
