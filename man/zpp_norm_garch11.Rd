% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zpp.R
\name{zpp_norm_garch11}
\alias{zpp_norm_garch11}
\title{Compute the closed-form normal ZPP with GARCH(1,1) errors}
\usage{
zpp_norm_garch11(prices, n.days = 250)
}
\arguments{
\item{prices}{is a numeric vector containing the prices series}

\item{n.days}{is the number of time-steps}
}
\value{
closed.zpp.garch is the estimated closed-form normal ZPP
}
\description{
This function computes the closed-form normal ZPP assuming the price-differenced series follows a model with constant mean and GARCH(1,1) errors
}
\examples{
library(rugarch)
eth_data<-poloniex_download(from="USDT",to="ETH",start="2001-01-01",end="2017-05-18",period=86400)
prices<-eth_data$weightedAverage
zpp.norm.garch11<-zpp_norm_garch11(prices)
zpp.norm.garch11
}
