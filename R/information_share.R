
#' Computes the information shares according to the approach by Brandvold et al. (2015)
#'
#' This function computes the information shares according to the approach by Brandvold et al. (2015)
#'
#' @param dat is a dataframe where the first column contains the date in \code{Y-m-d} format, while the remaining
#' columns the cryptocurrency prices from different exchanges
#' @param pi is a vector containing the activity shares of all exchanges. The activity share
#' is defined as the fraction of trades that happened on a single exchange, or simply, the
#'  probability that a trade took place on that exchange. See Brandvold et al. (2015) for details.
#' @param opt_method is a character string specifying the optimization algorithm for inner loop optimization with
#'  the \code{\link[alabama]{auglag}} function from the alabama package. Default is "nlminb".
#' @return fin_est is a dataframe with information shares (first columns) and the parameters PSI (second columns)
#' which are the covariances between the fundamental news component and the idiosyncratic
#' component for each exchange
#' @details
#' This function computes the information shares according to the approach by Brandvold et al. (2015) and
#' returns a dataframe with information shares (first column) and the parameters PSI (second column)
#' which represent the covariances between the fundamental news component and the idiosyncratic
#' component for each exchange
#'
#' @export
#' @importFrom xts xts
#' @importFrom stats cov
#' @importFrom stats cor
#' @importFrom stats var
#' @importFrom alabama auglag
#' @importFrom stats lag runif
#'
#' @examples
#' data_file<-system.file("extdata", "btcusd_IS.csv", package = "bitcoinFinance")
#' dat<-read.csv(file = data_file,header = TRUE,sep = ";",dec = ".")
#'
#' #Vector of activity shares based on trading volumes and trades frequency
#' pivector<-c(0.33,0.06,0.48,0.11,0.02)
#' information_shares(dat,pi=pivector, opt_method="nlminb")
#'
#' # Robustness check:
#' # Vector of activity shares for simplicity set to 1/n for all 5 exchanges
#' # n<-ncol(dat)-1
#' # pivector<-c(rep(1/n,n))
#' # information_shares(dat,pi=pivector, opt_method="nlminb")



information_shares <- function(dat, pi, opt_method="nlminb") {
  if(sum(pi)!=1)
    stop("The sum of activity shares should be 1!")

  date<-as.Date(dat[,1],format="%Y-%m-%d")
  dat<-cbind(date, dat[,-1])
  dat<-dat[order(dat$date),]
  dat<-xts::xts(dat[,-1],order.by=dat[,1]) #the date column disappears

  # The loop creates the market price for each of the exchanges
  markets <- matrix(0,nrow = nrow(dat),ncol = ncol(dat))

  for (i in 1:ncol(dat))
  {
    markets[,i]<-as.matrix(x = dat[,-i]) %*% c(rep(1/(ncol(dat)-1),ncol(dat)-1))
  }
  rm(i)
  colnames(markets)<-paste(c(colnames(dat)),c(rep.int("mrkt",ncol(dat))),sep=".")
  dat<-cbind(dat, markets)
  dat<-log(dat)

  # Matrix of log returns of observes prices
  dat.diff<-diff(x = dat,lag = 1,differences = 1,
                  arithmetic = TRUE,log = FALSE,na.pad = TRUE)

  #This is the number of individual exchanges
  n<-ncol(dat.diff)/2

  #Various covariances that appear in the constraints
  cov.ji<-matrix(0,nrow = n,ncol = 1)
  cov.ji.1<-matrix(0,nrow = n,ncol = 1)
  cov.ji.2<-matrix(0,nrow = n,ncol = 1)
  cov.ij.2<-matrix(0,nrow = n,ncol = 1)
  rho.ii.1<-matrix(0,nrow = n,ncol = 1)

  #This loop fills the vectors of constraints
  for (i in 1:n)
  {
    cov.ji[i,]<-cov(x = dat.diff[,i],y = dat.diff[,i+n],use = "pairwise.complete.obs")
    cov.ji.1[i,]<-cov(x = lag(dat.diff[,i]),y = dat.diff[,i+n],use = "pairwise.complete.obs")
    cov.ji.2[i,]<-cov(x = lag(lag(dat.diff[,i])),y = dat.diff[,i+n],use = "pairwise.complete.obs")
    cov.ij.2[i,]<-cov(x = dat.diff[,i],y = lag(lag(dat.diff[,i+n])),use = "pairwise.complete.obs")
    rho.ii.1[i,]<-cor(x = dat.diff[,i],y = lag(dat.diff[,i]),use = "pairwise.complete.obs")
  }

  #Estimation of sigma^2
  sigma2 <- var(x = as.matrix(x = dat.diff[,c(seq(1,n,1))] %*% c(rep(1/n,n))),use = "pairwise.complete.obs")

  #Intialize the building blocks of the optimization problem
  #objective function
  fn <- function(x){
    abs(pi %*% x[1:n])
  }

  #gradient of the objective function
  gr <- function(x){
    gradient<-rep(NA,4*n)
    for (k in 1:(4*n)){
      if (k<=n){gradient[k]<-(pi[k]*(pi %*% x[1:n]))/(abs(pi %*% x[1:n]))}else
      if (k>n){gradient[k]<-0}
    }
    gradient
  }

  #Equality constraint. See textbook for details
  heq <- function(x){
   equality<-rep(NA,3*n)
    for (j in 1:(3*n)){
      if (j<=n){equality[j]<- x[n+j]+x[j]+2*x[2*n+j]+sigma2-cov.ji[j]}else
      if (j>n && j<=(2*n)){equality[j]<- x[j]*(-1)-x[n+j]-cov.ji.2[j-n]-cov.ji.1[j-n]}else
      if (j>(2*n)){equality[j]<- (2*rho.ii.1[j-2*n]+1)*(x[j-2*n]+x[j+n])-cov.ij.2[j-2*n]+sigma2*rho.ii.1[j-2*n]}
    }
    equality
  }

  #Inequality constraints
  hin <- function(x){
    inequality<-rep(1,4*n)
    for (m in ((3*n)+1):(4*n)){
      inequality[m]<- x[m]
    }
    inequality
  }

  set.seed(13)
  par<-runif(4*n) #We can set initial values beyond constraints
  res <- alabama::auglag(par=par, fn=fn, gr=gr, hin=hin, heq=heq, control.outer = list(method=opt_method))

  #Compute the information share for each exchange
  inf.share<-c(rep.int(0,times=n))
  for (v in (1:n)){
    inf.share[v]<-pi[v]*(1+(res$par[v])/(sigma2))
  }
  fin_est=as.data.frame( cbind(inf.share, res$par[1:n]) )
  colnames(fin_est)=c("Information shares", "PSI_coefficients")
  return(fin_est)
}
