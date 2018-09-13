#' Compute the realized covariance of an xts object containing bitcoin intraday prices and regularize them if not positive definite
#'
#' This function computes the realized covariance of an xts object containing bitcoin intraday prices and regularize them if not positive definite
#'
#' @param price_xts is a xts object containing intraday bitcoin prices (2 series or more)
#' @return final_rc a list containing the (potentially regularized) realized covariances
#' @details
#' This function computes the realized covariance of an xts object containing
#' bitcoin intraday prices and regularize them if not positive definite, as done in
#' Hautsch, N., L. Kyj, and R. Oomen (2012)
#'
#'
#' @export
#' @importFrom highfrequency rCov
#' @importFrom zoo index
#'
#' @references Hautsch, N., L. Kyj, and R. Oomen (2012). A blocking and regularization approach
#' to high-dimensional realized covariance estimation. Journal of Applied Econometrics, 27, 625–645
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
#' }
#'

rc_pd <- function(price_xts){
  # calculating daily variance-covariance matrices and put them in a list
  var_covar_m <- lapply( split(price_xts, f = as.Date(zoo::index(price_xts))), function(x) {
    cov <- highfrequency::rCov(x, cor = FALSE, align.by = NULL, align.period = NULL, makeReturns = T)
  })

  # not all estimated var-covar matrices are positive semidefinite
  # detecting those that need help
  rc_pd_clean <- function(x, var_covar_matrix) {
    # Check wether matrix ill conditioned
    ill <- kappa(var_covar_matrix[[x]]) > 10*ncol(var_covar_matrix[[x]])

    # Getting the eigenvalues and vectors of the RC
    ev   <- eigen(var_covar_matrix[[x]])
    eval <- ev$value
    evec <- ev$vector

    # Check whether the RC is positive definite.I use a small positive epsilon 1e-8
    # to avoid ill-conditioned matrices

    NPD <- !(all(eval > 0.00000001))

    # Regularization
    if(ill | NPD ){
      # Smallest positive eigenvalue
      lmp  <- min(eval[eval>0.00000001])
      evreg <- eval*(eval>lmp) + lmp*(eval<=lmp)
      # PD covariance matrix
      cxpd <- evec %*% diag(evreg) %*% t(evec)
    }  else {
       cxpd <- var_covar_matrix[[x]]
    }

    return(cxpd)
  }

  final_rc <- lapply(as.list(1:length(var_covar_m)), rc_pd_clean, var_covar_matrix = var_covar_m)
  return(final_rc)
}
