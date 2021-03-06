% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GHAR.R
\name{GHAR.model}
\alias{GHAR.model}
\title{Compute the GHAR model for realized covariances and returns the 1-day-ahead VaR forecasts
and the 1-day-ahead Realized Covariance forecast.}
\usage{
GHAR.model(cleaned.rc, daily_ret, weights, quantiles = c(0.05, 0.1))
}
\arguments{
\item{cleaned.rc}{is a list containing the realized covariances}

\item{daily_ret}{is an xts object containing the cryptocurrency daily returns. It should be compatible
with the list containing the realized covariances}

\item{weights}{is a vector of weights for the porfolio VaR}

\item{quantiles}{the desidered quantiles for the VaR estimates}
}
\value{
results a list containing the 1-day-ahead VaR forecasts, 1-day-ahead Realized Covariance forecast,
and the GHAR.fit object computed with the systemfit package
}
\description{
This function computes the GHAR model for realized covariances and returns the 1-day ahead
VaR forecasts (for an equally-weighted portfolio), as well as the the 1-day-ahead Realized Covariance forecast.
}
\details{
This function computes the GHAR model for realized covariances by Cech and Baruník (2017)
and returns the 1-day ahead #' VaR forecasts (for an equally-weighted portfolio - default case),
the 1-day-ahead Realized Covariance forecast, and the GHAR.fit object computed with the systemfit package
}
\examples{
\dontrun{
library(highfrequency)
localbtcMXN <-bitcoincharts_single_download(name = "localbtcMXN.csv.gz")
btcdeEUR <-bitcoincharts_single_download(name = "btcdeEUR.csv.gz")
krakenEUR <-bitcoincharts_single_download(name = "krakenEUR.csv.gz")
localbtcINR <-bitcoincharts_single_download(name = "localbtcINR.csv.gz")
coinbaseUSD <-bitcoincharts_single_download(name = "coinbaseUSD.csv.gz")
data5<-list(localbtcMXN =localbtcMXN, btcdeEUR=btcdeEUR, krakenEUR=krakenEUR,
             localbtcINR=localbtcINR, coinbaseUSD=coinbaseUSD)
data_clean<-aggregate_merge_bictoincharts_data(data_list=data5)
cleaned.rc <- rc_pd(data_clean$price_ts)
GHAR_fit <- GHAR.model(cleaned.rc, data_clean$daily_returns, weights =rep(0.2,5))
}

}
\references{
Cech, Frantisek, and Jozef Baruník (2017). On the Modelling and Forecasting of Multivariate Realized Volatility: Generalized Heterogeneous Autoregressive (GHAR) Model. Journal of Forecasting 36(2), 181-206.
}
