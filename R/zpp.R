#' Compute the general version of the ZPP
#'
#' This function computes the general version of the ZPP by simulating a number of trajectories (total number is given
#' by \code{scenarios}) over \code{n.days} time-steps, given an estimated GARCH model
#'
#' @param prices is a numeric vector containing the prices series
#' @param mean.model is a list specifying the ARMA order for \code{\link{ugarchspec}}
#' @param variance.model is a list containing the variance model specification for \code{\link{ugarchspec}}
#' @param distribution.model is the conditional density to use for the innovations. See \code{\link{ugarchspec}} for details
#' @param n.days is the number of time-steps
#' @param scenarios is the total number fo trajectories
#' @return zpp is the estimated ZPP
#'
#' @export
#' @importFrom rugarch ugarchspec
#' @importFrom rugarch ugarchfit
#' @importFrom rugarch ugarchsim
#' @importFrom matrixStats colCumsums
#' @importFrom matrixStats colMins
#' @importFrom utils tail
#'
#' @examples
#' btc_data<-dat<-bitcoinity_download(currency="USD", data_type="price",
#'                exchange="coinbase",time_length = "2y")
#' prices<-btc_data$avg
#' zpp<-zpp_general(prices)
#' zpp

zpp_general <- function(prices, mean.model = list(armaOrder=c(0,0)), variance.model = list(garchOrder=c(1,1)),
                        distribution.model = "norm", n.days=250, scenarios=20000 ){
  price.difference <- diff(prices)
  # Fit GARCH model and simulate shocks
  garch.spec = ugarchspec(variance.model,   mean.model, distribution.model)
  garch.fit = ugarchfit(spec=garch.spec, data=price.difference ,
                        solver.control=list(trace = FALSE))
  garch.sim <- ugarchsim(garch.fit, n.sim = n.days, n.start = 0,
                         m.sim = scenarios, startMethod = c("unconditional") )

  # Extract simulated shocks
  garch.sim.shocks <- matrix( unlist(garch.sim@simulation[2]) , ncol = scenarios, byrow = TRUE)
  # Compute simulated trajectories
  last.price<-tail(prices,n=1)

  garch.sim.prices  <-  last.price + matrixStats::colCumsums(garch.sim.shocks)
  #Find the minimum value taken by the simulated price and checks whether it was smaller than zero
  # I multiply by 1 to transform the vector from logical to numeric 1/0.
  # finally count how many trajectoies crossed the zero barrier and compute the zpp
  zpp <- sum( (matrixStats::colMins(garch.sim.prices)<0)*1 ) / ncol(garch.sim.prices)
  return(zpp)
}


#' Compute the closed-form normal ZPP
#'
#' This function computes the closed-form normal ZPP assuming the price series follows a random walk with drift with normal innovations
#'
#' @param prices is a numeric vector containing the prices series
#' @param n.days is the number of time-steps
#' @return zpp.norm is the estimated closed-form normal ZPP
#'
#' @export
#' @importFrom stats pnorm var
#'
#' @examples
#' btc_data<-dat<-bitcoinity_download(currency="USD", data_type="price",
#'                exchange="coinbase",time_length = "2y")
#' prices<-btc_data$avg
#' zpp.norm<-zpp_norm(prices)
#' zpp.norm

zpp_norm <- function(prices, n.days=250 ){
  price.difference <- diff(prices)
  last.price<-tail(prices,n=1)
  var.diff<-var(price.difference)
  mu <- mean(price.difference)
  closed.zpp.norm=2*pnorm( -last.price, mean=mu*n.days, sd=sqrt(var.diff*n.days) )
  return(closed.zpp.norm)
}


#' Compute the closed-form normal ZPP with GARCH(1,1) errors
#'
#' This function computes the closed-form normal ZPP assuming the price-differenced series follows a model with constant mean and GARCH(1,1) errors
#'
#' @param prices is a numeric vector containing the prices series
#' @param n.days is the number of time-steps
#' @return closed.zpp.garch is the estimated closed-form normal ZPP
#'
#' @export
#' @importFrom stats pnorm var
#' @importFrom rugarch ugarchspec ugarchfit  ugarchfit ugarchforecast
#'
#' @examples
#' library(rugarch)
#' btc_data<-dat<-bitcoinity_download(currency="USD", data_type="price",
#'                exchange="coinbase",time_length = "2y")
#' prices<-btc_data$avg
#' zpp.norm.garch11<-zpp_norm_garch11(prices)
#' zpp.norm.garch11

zpp_norm_garch11=function(prices, n.days=250){
  price.difference <- diff(prices)
  garch11.spec = rugarch::ugarchspec(variance.model = list(garchOrder=c(1,1)),
                                     mean.model = list(armaOrder=c(0,0)))
  garch11.fit = rugarch::ugarchfit(spec=garch11.spec, data=price.difference ,  solver.control=list(trace = FALSE))
  th <- garch11.fit@fit$coef
  last.price<-tail(prices,n=1)
  mu.g=th[1]
  omega=th[2]
  alpha=th[3]
  beta=th[4]
  # sigma_tpiu1 = omega/(1-alpha-beta)
  sigma_tpiu1 = (rugarch::ugarchforecast(garch11.fit, n.ahead=1)@forecast$sigmaFor)^2
  amat=seq(from = n.days-1, by = -1, length.out = n.days-1)
  ab=alpha+beta
  bmat=c(1,ab^seq(1, n.days-2, by=1))
  var_tot=omega*(t(amat)%*%bmat)+sigma_tpiu1*(1-(alpha+beta)^n.days)/(1-(alpha+beta))
  closed.zpp.garch=2*pnorm( -last.price, mean=mu.g*n.days, sd=sqrt(var_tot) )
  return(closed.zpp.garch)
}
