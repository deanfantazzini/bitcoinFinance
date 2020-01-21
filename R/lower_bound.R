

#' Compute the bitcoin lower bound with user-delivered inputs
#'
#' This function computes the bitcoin lower bound with inputs given by the user and not taken from the web
#'
#' @param block.reward is the block reward (currently 12.5 BTC/block)
#' @param hashing.power.miner is the hashing power employed by a miner. Hayes (2017) sets this value to 1000 GH/s even though
#'  the actual hashing power of a miner is likely to deviate greatly from this value.
#' @param Difficulty is the difficulty which is expressed in units of GH/block
#' @param price.kWh is the $electricity price per kWh in dollars
#' @param W.GHs is the energy consumption efficiency of the producer’s hardware expressed in W per GH/s
#' @return price.bitcoin is the bitcoin lower bound
#' @details
#' This function computes the bitcoin lower bound with inputs given by the user
#'
#'
#' @export
#'
#' @references Hayes, A. S. (2017). Cryptocurrency Value Formation: An empirical study leading to a cost of production model for valuing Bitcoin. Telematics and Informatics, forthcoming.
#' @references Fantazzini, D., Nigmatullin, E., Sukhanovskaya, V., Ivliev, S., (2016) Everything you always wanted to know about bitcoin modelling but were afraid to ask. Part 1. Applied Econometrics, 44, 5-24.
#'
#' @examples
#' #This reproduces the example in Fantazzini et al. (2016)
#'  btc.lower.bound.user(Difficulty=60883825480, price.kWh=0.135,W.GHs=0.75)
#'  #  588.3616
#'

btc.lower.bound.user=function(block.reward = 12.5, hashing.power.miner = 10^12, Difficulty = 559970892890,
                              price.kWh=0.125, W.GHs=0.25){

  theta<-(24*3600)/(2^(32))
  BTCday  <- theta*block.reward*hashing.power.miner/Difficulty
  cost.mining.day <- price.kWh*24*W.GHs/(hashing.power.miner/10^12)

  price.bitcoin <- cost.mining.day/BTCday
  return(price.bitcoin)
}




#' Compute the bitcoin lower bound with web-delivered inputs
#'
#' This function computes the bitcoin lower bound with (some) inputs taken from the web
#'
#' @param block.reward is the block reward (currently 12.5 BTC/block)
#' @param hashing.power.miner is the hashing power employed by a miner. Hayes (2017) sets this value to 1000 GH/s even though
#'  the actual hashing power of a miner is likely to deviate greatly from this value.
#' @param W.GHs is the energy consumption efficiency of the producer’s hardware expressed in W per GH/s
#' @return price.bitcoin is the bitcoin lower bound
#' @details
#' This function computes the bitcoin lower bound with some inputs taken from the web: a) the latest difficulty (expressed in units of GH/block),
#' b) the electricity price per kWh in dollars (given by the latest US average retail price - residential sector)
#'
#'
#' @export
#' @importFrom Quandl Quandl
#'
#' @references Hayes, A. S. (2017). Cryptocurrency Value Formation: An empirical study leading to a cost of production model for valuing Bitcoin. Telematics and Informatics, forthcoming.
#'
#' @examples
#'
#' \dontrun{
#'  btc.lower.bound.web(W.GHs=0.2)
#'  }

btc.lower.bound.web=function(block.reward = 12.5, hashing.power.miner = 10^12, W.GHs=0.25){

  Difficulty<-Quandl("BCHAIN/DIFF")
  Difficulty<-Difficulty$Value[1]
  price.kWh <- Quandl("EIA/ELEC_PRICE_US_RES_M")
  price.kWh<-price.kWh$Value[1]
  price.kWh<-price.kWh/100 #in cents

  theta<-(24*3600)/(2^(32))
  BTCday  <- theta*block.reward*hashing.power.miner/Difficulty
  cost.mining.day <- price.kWh*24*W.GHs/(hashing.power.miner/10^12)

  price.bitcoin <- cost.mining.day/BTCday
  return(price.bitcoin)
}


#' Compute the theoretical no-arbitrage market price of an altcoin based on the SHA-256 algorithm
#'
#' This function computes the theoretical no-arbitrage market price of an altcoin based on the SHA-256 algorithm
#'
#' @param block.reward.bitcoin is the block reward for bitcoin (currently 12.5 BTC/block)
#' @param hashing.power.miner is the hashing power employed by a miner. Hayes (2017) sets this value to 1000 GH/s even though
#'  the actual hashing power of a miner is likely to deviate greatly from this value.
#' @param Difficulty.bitcoin is the difficulty of the Bitcoin system which is expressed in units of GH/block
#' @param block.reward.altcoin is the block reward for the SHA-256 based altcoin
#' @param Difficulty.altcoin is the difficulty of the altcoin system which is expressed in units of GH/block
#' @return epsilon_star is no-arbitrage market price of an altcoin based on the SHA-256 algorithm
#' @details
#' This function computes the no-arbitrage market price of an altcoin based on the SHA-256 algorithm
#'
#'
#' @export
#'
#' @references Hayes, A. S. (2017). Cryptocurrency Value Formation: An empirical study leading to a cost of production model for valuing Bitcoin. Telematics and Informatics, forthcoming.
#' @references Fantazzini, D., Nigmatullin, E., Sukhanovskaya, V., Ivliev, S., (2016) Everything you always wanted to know about bitcoin modelling but were afraid to ask. Part 1. Applied Econometrics, 44, 5-24.
#'
#' @examples
#' #no-arbitrage market price of Bitcoin Cash using data published on the 16/09/2017
#'  btc.arbitrage.user(block.reward.bitcoin = 12.5, hashing.power.miner = 10^12,
#'  Difficulty.bitcoin = 922724699726,
#'  block.reward.altcoin = 12.5,  Difficulty.altcoin = 109634813602)

btc.arbitrage.user=function(block.reward.bitcoin = 12.5, hashing.power.miner = 10^12,
                            Difficulty.bitcoin = 922724699726,
                            block.reward.altcoin = 12.5,  Difficulty.altcoin = 109634813602){
  theta<-(24*3600)/(2^(32))
  BTCday  <- theta*block.reward.bitcoin*hashing.power.miner/Difficulty.bitcoin
  denominator <- (block.reward.altcoin*hashing.power.miner*theta)/Difficulty.altcoin
  epsilon_star <- BTCday/ denominator
  return(epsilon_star)
}
