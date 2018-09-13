#' Compute a Lasso-VAR model for realized covariance matrices with a rolling window and compute 1-step-ahead VaR forecasts
#'
#' This function computes a Lasso-VAR model for realized covariance matrices with a rolling window and computes 1-step-ahead VaR forecasts
#'
#' @param cleaned.rc is a list containing the realized covariances
#' @param daily_ret is an xts object containing the cryptocurrency daily returns. It should be compatible
#' with the list containing the realized covariances
#' @param weights is a vector of weights for the rolling porfolio VaR forecasts
#' @param quantiles the desidered quantiles for the 1-step-ahead VaR estimates
#' @param es.alpha is the desidered probability level for the 1-step-ahead ES estimate
#' @param roll.window a numerical scalar specifying the dimension of the rolling estimation window
#' @return data_to_plot a (zoo object) matrix containing the realized portfolio returns, and
#' the relative 1-day-ahead VaR forecasts
#' @details
#' This function computes the Lasso-VAR model for realized covariance matrices by
#'  Callot et al. (2017) with a rolling window and compute the 1-step VaR forecasts
#'
#'
#' @export
#' @importFrom corpcor sm2vec vec2sm
#' @importFrom zoo zoo
#' @importFrom zoo index
#' @importFrom zoo coredata
#' @importFrom stats qnorm dnorm
#' @importFrom ggplot2 autoplot
#' @importFrom lassovar forecast.lassovar
#'
#' @references Callot, Laurent AF, Anders B. Kock, and Marcelo C. Medeiros. "Modeling and Forecasting Large Realized Covariance Matrices and Portfolio Choice." Journal of Applied Econometrics 32.1 (2017): 140-158.
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
#' lassovar_roll <- lassovar.RC.1step.rolling.VaR.forecast(cleaned.rc, data_clean$daily_returns,
#'              weights =rep(0.2,5), quantiles = c(0.05, 0.1), roll.window = 662, es.alpha=0.025)
#' library(xts)
#' ggplot2::autoplot(as.xts(lassovar_roll), facet = NULL)
#' }
#'


lassovar.RC.1step.rolling.VaR.forecast <- function(cleaned.rc, daily_ret, weights, quantiles = c(0.05, 0.1),
                                                   es.alpha=0.025, roll.window = 250){

  if( NROW(cleaned.rc)!= NROW(daily_ret) )
    stop("List of Realized Covariances and and the xts object of daily returns are not conformable")
  if(missing(weights))
    stop("The porfolio weights are missing!")

  # Transform RC matrices into vectors: note that the function `sm2vec` takes
  # a symmetric matrix and puts the lower triagonal entries into a vector
  RC_vec <- do.call( rbind, lapply(as.list(1:length(cleaned.rc)), function(x) {
    tmp <- corpcor::sm2vec(cleaned.rc[[x]], diag=TRUE)
  }) )

  gc(reset = T)

  # Create a time-indexed zoo object
  RC_vec <- zoo::zoo(x = RC_vec, order.by = zoo::index(daily_ret))
  # Estimate Lasso-VAR model and compute a 1-step ahead forecast
  RC_vec_lasso<-lassovar::forecast.lassovar(RC_vec, fc.train=roll.window, horizon = 1, lags = 22,  fc.window ="fix")

  #Portfolio daily returns
  daily_return <- zoo::coredata(daily_ret) %*% weights

  calc_VaR_lasso <- function(i) {
      pred_cov_m <- RC_vec_lasso$pred[i,]
      pred_cov_m <- corpcor::vec2sm(pred_cov_m , diag=TRUE)
      pred_cov_m <- matrix_pd(pred_cov_m)
      # VaR values
      sigma.hat <- sqrt(t(weights) %*% pred_cov_m %*% weights)
      VaR_est <- suppressWarnings( qnorm( quantiles ) * sigma.hat )
      # ES value
      mu.hat <- 0
      ES_est = as.numeric( -(mu.hat + sigma.hat*dnorm(qnorm(es.alpha))/es.alpha) )
      return( c(VaR_est,ES_est) )
  }

  VaR_values <- do.call( rbind, lapply(as.list(1:nrow(RC_vec_lasso$pred)), calc_VaR_lasso) )
  returns_to_plot <- daily_return[-(1:(nrow(daily_return) - NROW(VaR_values))), ]

  data_to_plot <- zoo::zoo(cbind(returns_to_plot, VaR_values),
                           order.by = zoo::index(daily_ret)[-(1:(nrow(daily_return) - NROW(VaR_values)))])
  colnames(data_to_plot)=c("Realized returns", paste0("VaR",quantiles), paste0("ES",es.alpha))
  return(data_to_plot)
}

