#' Compute the optimal power-law parameter and the portfolio weights according to the approach proposed by Kristoufek (2013)
#'
#' This function computes the optimal power-law parameter and the portfolio weights according to the approach proposed by Kristoufek (2013)
#' using in-sample data
#'
#' @param V a zoo object containing the Google Trends search terms (cryptocurrency ticker or other type)
#' @param alpha is a vector of possible values for the power-law parameter alpha (usually between -2 and +2)
#' @param ret_data a zoo object containing the return data of the selected cryptocurrencies
#' @param rf is the risk-free rate needed for the computation of the \link[PerformanceAnalytics]{SharpeRatio}
#' @param type one of "StdDev" or "VaR" or "ES" to use as the denominator for computing the \link[PerformanceAnalytics]{SharpeRatio}
#' @return a list containing the following objects:
#' \describe{
#'   \item{sd_all}{a vector of all standard deviations obtained in-sample for each value of alpha}
#'   \item{min_sd}{value of alpha that min the standard deviation in-sample}
#'   \item{sr_all}{a vector of all Sharpe Ratios obtained in-sample for each value of alpha}
#'   \item{min_sr}{value of alpha that max the Sharpe Ratio in-sample}
#'   \item{ws_last_best_sd}{portfolio weights computed with the alpha that minimized the std.dev. and the latest Google Trends data}
#'   \item{ws_last_best_sr}{portfolio weights computed with the alpha that maximized the Sharpe ratios and the latest Google Trends data}
#' }
#'
#' @details
#' This function computes the optimal power-law parameter alpha that min the std. dev or max the Sharpe ratio for the given dataset
#' and returns the optimal portfolio weights using the latest Google Trends data.
#'
#' @export
#' @importFrom zoo zoo index as.zoo
#' @importFrom PerformanceAnalytics SharpeRatio
#' @importFrom QuantTools returns
#' @importFrom stats sd
#'
#' @references Kristoufek, L. (2013). Can Google Trends search queries contribute to risk diversification?. Scientific reports, 3, 2713.
#'
#' @examples
#' \dontrun{
#' library(PerformanceAnalytics)
#' data(crypto_google_data)
#' R <- crypto_google_data[,2:6]
#' R<- zoo::zoo( apply(R, 2, QuantTools::returns), order.by=lubridate::ymd(crypto_google_data[,1]))
#' R<-R[2:nrow(R),]
#' alpha <- seq(from = -2, to = 2, by = 0.1)
#' V=zoo::zoo(crypto_google_data[,7:11], order.by=lubridate::ymd(crypto_google_data[,1]) )
#' V<-V[2:nrow(V),]
#' google_port=W_sd_sr(V, alpha, ret_data=R,  rf=0.003, type="StdDev")
#' #Plots
#' plot(y = google_port$sd_all,
#'      x = alpha,
#'      xlab = "Discrimination parameter",
#'      ylab = "StdDev",pch=21)
#' plot(y = google_port$sr_all,
#'      x = alpha,
#'      xlab = "Discrimination parameter",
#'      ylab = "SharpeRatio")
#'      }
#'

W_sd_sr <- function(V, alpha, ret_data, rf=0.005, type="StdDev") {
  V<-as.zoo(V)
  ret_data<- as.zoo(ret_data)
  sd_all=NULL
  sr_all=NULL
  for (i in 1:length(alpha)){
    ws <-  V^(-alpha[i])/apply(V^(-alpha[i]),1,FUN = "sum")
    port_ret <-zoo::zoo(apply(ret_data*ws,1,sum), order.by=zoo::index(ret_data) )
    sdev <- sd(port_ret, na.rm = TRUE)
    sd_all<-c(sd_all, sdev)
    sr <- PerformanceAnalytics::SharpeRatio(port_ret, Rf = rf, FUN=type)
    sr_all<-c(sr_all, sr)
  }
  min_sd<-alpha[which.min(sd_all)]
  min_sr<-alpha[which.max(sr_all)]
  ws_last_best_sd<- last( V^(-min_sd)/apply(V^(-min_sd),1,FUN = "sum") )
  ws_last_best_sr<- last( V^(-min_sr)/apply(V^(-min_sr),1,FUN = "sum") )
  return(list(sd_all=sd_all, min_sd, sr_all=sr_all, min_sr, ws_last_best_sd=ws_last_best_sd, ws_last_best_sr=ws_last_best_sr))
}


#' Compute the optimal power-law parameter and the portfolio weights according to the approach proposed by Kristoufek (2013) - [out-of-sample]
#'
#' This function computes the optimal power-law parameter and the portfolio weights according to the approach proposed by Kristoufek (2013)
#' using out-of-sample data. Default lag for portfolio weights is 1, but it can be modified by the user.
#'
#' @param V a zoo object containing the Google Trends search terms (cryptocurrency ticker or other type)
#' @param alpha is a vector of possible values for the power-law parameter alpha (usually between -2 and +2)
#' @param ret_data a zoo object containing the return data of the selected cryptocurrencies
#' @param rf is the risk-free rate needed for the computation of the \link[PerformanceAnalytics]{SharpeRatio}
#' @param type one of "StdDev" or "VaR" or "ES" to use as the denominator for computing the \link[PerformanceAnalytics]{SharpeRatio}
#' @param lag_ws the lag to be applied to the portfolio weights for computing the out-of-sample analysis
#' @return a list containing the following objects:
#' \describe{
#'   \item{sd_all}{a vector of all standard deviations obtained in-sample for each value of alpha}
#'   \item{min_sd}{value of alpha that min the standard deviation in-sample}
#'   \item{sr_all}{a vector of all Sharpe Ratios obtained in-sample for each value of alpha}
#'   \item{min_sr}{value of alpha that max the Sharpe Ratio in-sample}
#' }
#'
#' @details
#' This function computes the optimal power-law parameter alpha that min the std. dev or max the Sharpe ratio for the given dataset,
#' using (pseudo) out-of-sample data. The portfolio weights (computed for a given alpha) are lagged by a quantity
#' defined by \code{lag_ws}.
#'
#' @export
#' @importFrom zoo zoo index as.zoo
#' @importFrom PerformanceAnalytics SharpeRatio
#' @importFrom QuantTools returns
#' @importFrom stats sd
#'
#' @references Kristoufek, L. (2013). Can Google Trends search queries contribute to risk diversification?. Scientific reports, 3, 2713.
#'
#' @examples
#' \dontrun{
#' library(PerformanceAnalytics)
#' data(crypto_google_data)
#' R <- crypto_google_data[,2:6]
#' R<- zoo::zoo( apply(R, 2, QuantTools::returns), order.by=lubridate::ymd(crypto_google_data[,1]))
#' R<-R[2:nrow(R),]
#' alpha <- seq(from = -2, to = 2, by = 0.1)
#' V=zoo::zoo(crypto_google_data[,7:11], order.by=lubridate::ymd(crypto_google_data[,1]) )
#' V<-V[2:nrow(V),]
#' google_port_out=W_sd_sr_out(V, alpha, ret_data=R, rf=0.003, type="StdDev", lag_ws=4)
#' #Plots
#' plot(y = google_port_out$sd_all,
#'      x = alpha,
#'      xlab = "Discrimination parameter",
#'      ylab = "StdDev",pch=21)
#' plot(y = google_port_out$sr_all,
#'      x = alpha,
#'      xlab = "Discrimination parameter",
#'      ylab = "SharpeRatio")
#'      }
#'

W_sd_sr_out <- function(V, alpha, ret_data, rf=0.005, type="StdDev", lag_ws=1) {
  V<-as.zoo(V)
  ret_data<- as.zoo(ret_data)
  sd_all=NULL
  sr_all=NULL
  for (i in 1:length(alpha)){
    ws <-  V^(-alpha[i])/apply(V^(-alpha[i]),1,FUN = "sum")
    ws<-zoo::zoo(ws[1:(NROW(ws)-lag_ws),],zoo::index(ws)[(1+lag_ws):NROW(ws)])
    port_ret <-zoo::zoo(apply(ret_data*ws,1,sum), order.by=zoo::index(ws) )
    sdev <- sd(port_ret,  na.rm = TRUE)
    sd_all<-c(sd_all, sdev)
    sr <- PerformanceAnalytics::SharpeRatio(port_ret, Rf = rf, FUN=type)
    sr_all<-c(sr_all, sr)
  }
  min_sd<-alpha[which.min(sd_all)]
  min_sr<-alpha[which.max(sr_all)]
  return(list(sd_all=sd_all, min_sd=min_sd, sr_all=sr_all, min_sr=min_sr))
}
