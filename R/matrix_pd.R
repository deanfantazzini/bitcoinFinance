#' Check whether a matrix is positive definite and regularize it if not positive definite
#'
#' This function checks whether a matrix is positive definite and regularizes it if not positive definite
#'
#' @param sym_matrix is a symmetric matrix
#' @return cxpd a (potentially regularized) positive definite a symmetric \code{n} x \code{n} matrix
#' @details
#' This function checks whether a matrix is positive definite and regularizes it if not positive definite,
#' as done in  Hautsch, N., L. Kyj, and R. Oomen (2012)
#'
#'
#' @export
#'
#' @references Hautsch, N., L. Kyj, and R. Oomen (2012). A blocking and regularization approach
#' to high-dimensional realized covariance estimation. Journal of Applied Econometrics, 27, 625–645
#'
#' @examples
#' a = rbind(
#'    c(4,  0.5,1),
#'    c(0.5, 3 ,0),
#'    c(1,   0, -2)
#'    )
#' a
#' matrix_pd(a)



matrix_pd <- function(sym_matrix) {
  # Check wether matrix ill conditioned
  ill <- kappa(sym_matrix) > 10*ncol(sym_matrix)

  # Getting the eigenvalues and vectors of the RC
  ev   <- eigen(sym_matrix)
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
    cxpd <- sym_matrix
  }

  return(cxpd)
}
