% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/upper_bounds.R
\name{btc.upper.bound.user}
\alias{btc.upper.bound.user}
\title{Compute the bitcoin upper bound with user-delivered inputs}
\usage{
btc.upper.bound.user(
  HSC.ratio = 0.04,
  Ecommerce = 224,
  bitcoinshare = 0.1,
  GDP.ratio = 5,
  metrics = c(9, 0.9, 4.5),
  price.silver = 17,
  total.silver.eagles = 4e+08,
  TOTBC.u = 1.1e+07
)
}
\arguments{
\item{HSC.ratio}{is the US household checking deposits and cash / US personal consumption ratio}

\item{Ecommerce}{is the total B2C e-commerce sales (in billions)}

\item{bitcoinshare}{is the bitcoin share of of all on-line shopping}

\item{GDP.ratio}{is the World GDP / US GDP ratio}

\item{metrics}{is a vector cointaining the market capitalization of Western Union, MoneyGram and Euronet (in billions)}

\item{price.silver}{is the price of a silver eagle coin}

\item{total.silver.eagles}{is the total US silver eagle coints minted since 1986}

\item{TOTBC.u}{is the total number of bitcoin in circulation}
}
\value{
x the bitcoin upper bound
}
\description{
This function computes the bitcoin upper bound with inputs given by the user and not taken from the web
}
\details{
This function computes the bitcoin upper bound with inputs given by the user
}
\examples{
#This reproduces the example in Woo et al.(2013).
#It uses data up to December 2012.
 btc.upper.bound.user(HSC.ratio=0.04, Ecommerce= 224, bitcoinshare=0.1, GDP.ratio=5,
 metrics=c(8.5, 0.8, 4.2), price.silver=29, total.silver.eagles=337031982, TOTBC.u=10613175)
#  1398.673

}
\references{
Woo D., Gordon I., Iaralov V. (2013). Bitcoin: a first assessment. FX and Rates, December 2013, Bank of America Merrill Lynch.
}
