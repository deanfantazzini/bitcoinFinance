#' Compute the GHAR model with a rolling window and compute the 1-step-ahead VaR forecasts
#'
#' This function computes the GHAR model with a rolling window and compute the 1-step-ahead VaR forecasts
#'
#' @param cleaned.rc is a list containing the realized covariances
#' @param daily_ret is an xts object containing the cryptocurrency daily returns. It should be compatible
#' with the list containing the realized covariances
#' @param weights is a vector of weights for the rolling porfolio VaR forecasts
#' @param quantiles are the desidered probability levels for the 1-step-ahead VaR estimates
#' @param es.alpha is the desidered probability level for the 1-step-ahead ES estimate
#' @param roll.window a numerical scalar specifying the dimension of the rolling estimation window
#' @return data_to_plot a (zoo object) matrix containing the realized portfolio returns, and
#' the relative 1-day-ahead VaR forecasts
#' @details
#' This function computes the GHAR model by Cech and Baruník (2017) with a
#' rolling window and compute the 1-step VaR forecasts
#'
#'
#'
#' @export
#' @importFrom xts last
#' @importFrom zoo zoo
#' @importFrom zoo index
#' @importFrom zoo coredata
#' @importFrom zoo rollmean
#' @importFrom systemfit createSystemfitModel
#' @importFrom systemfit systemfit
#' @importFrom stats qnorm dnorm
#' @importFrom lubridate days
#' @importFrom stats as.formula
#' @importFrom stats predict
#' @importFrom ggplot2 autoplot
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
#' GHAR_roll <- GHAR.1step.rolling.VaR.forecast(cleaned.rc, data_clean$daily_returns,
#'          weights =rep(0.2,5), quantiles = c(0.05, 0.1), roll.window = 250, es.alpha=0.025)
#' library(xts)
#' ggplot2::autoplot(as.xts(GHAR_roll), facet = NULL)
#' }
#'


GHAR.1step.rolling.VaR.forecast <- function(cleaned.rc, daily_ret, weights, quantiles = c(0.05, 0.1),
                                            es.alpha=0.025, roll.window = 250){

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

  # Create GHAR formula objects for systemfit function
  GHAR <- systemfit::createSystemfitModel(nEq = ncol(chol_m), nRegEq = 3, 100)
  # creating GHAR formulas
  formula_list <- lapply(as.list(1:ncol(chol_m)), function(x) {
    names_for_eq <- names(data_m)[0:3 * ncol(chol_m) + x]
    stats::as.formula(paste(names_for_eq[1], paste(names_for_eq[-1], collapse = '+'), sep = '~'))
  })
  GHAR$formula <- formula_list

  #Portfolio daily returns
  daily_return <- zoo::coredata(daily_ret) %*% weights

  # Given that it is possible that the model cannot be estimated numerically,
  # we used tryCatch
  calc_VaR <- function(i) {
    out <- tryCatch(
      {
        GHAR$data <- data_df[(i - (roll.window-1)):i, ]
        fit <- systemfit(GHAR$formula, 'SUR', data = GHAR$data)

        pred_chol <- matrix(0, ncol(daily_ret), ncol(daily_ret))
        pred_chol[upper.tri(pred_chol, diag = T)] <-
          as.numeric(stats::predict(fit, newdata = data_df[(i + 1), ]))
        pred_cov_m <- t(pred_chol) %*% pred_chol

        # VaR values
        sigma.hat <- sqrt(t(weights) %*% pred_cov_m %*% weights)
        VaR_est <- qnorm( quantiles ) * sigma.hat
        # ES value
        mu.hat <- 0
        ES_est = as.numeric( -(mu.hat + sigma.hat*dnorm(qnorm(es.alpha))/es.alpha) )
        return( c(VaR_est,ES_est) )
      },
      error = function(cond) {
        return(NA)
      },
      finally = { print(i) }
    )
  }
  VaR_values <- do.call(rbind, lapply(as.list(roll.window:(nrow(data_df) - 1)), calc_VaR))
  returns_to_plot <- daily_return[-(1:(nrow(daily_return) - NROW(VaR_values))), ]

  data_to_plot <- zoo::zoo(cbind(returns_to_plot, VaR_values),
                           order.by = zoo::index(daily_ret)[-(1:(nrow(daily_return) - NROW(VaR_values)))])
  colnames(data_to_plot)=c("Realized returns", paste0("VaR",quantiles), paste0("ES",es.alpha))

  return(data_to_plot)
}
