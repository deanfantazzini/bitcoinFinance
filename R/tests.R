#' Compute a sequence of  multivariate ARCH tests
#'
#' This function computes a sequence of multivariate ARCH tests
#'
#' @param resids is a dataframe containing the standardized residuals from a multivariate model
#' @param max.lag is the the max lag used to compute the multivariate ARCH test
#' @return results is a list with the lag, statistic and p-values
#' @details
#' This function computes a sequence of multivariate ARCH tests using a modified version of
#' the original arch.multi internal function of the vars package
#'
#' @export
#' @importFrom stats embed pchisq pf resid
#'
#' @examples
#' \dontrun{
#'  Sigma <- matrix(c(10,3,3,2),2,2)
#'  dat.norm<- MASS::mvrnorm(n = 1000, rep(0, 2), Sigma)
#'  recursive.arch.multi(dat.norm,20)
#' }

recursive.arch.multi <- function(resids, max.lag)
{
  pvalues	<-	numeric(max.lag);
  tstat   <-  numeric(max.lag);
  for (i in 1:max.lag)
  {
    test.arch <-arch.multi(resids, i)
    tstat[i]	<-test.arch$statistic;
    pvalues[i]	<-test.arch$p.value;
  }
  lag		<-	1:max.lag;
  pvalues	<-	pvalues;
  results<-cbind(lag,tstat,pvalues)
  return(results);
}


#' Compute a sequence of  multivariate Portmanteau tests
#'
#' This function computes a sequence of multivariate Portmanteau tests
#'
#' @param resids is a dataframe containing the standardized residuals from a multivariate model
#' @param var.p.lag is the number of lags in the estimated multivariate VAR(p) model
#' @param max.lag is the the max lag used to compute the multivariate Portmanteau  test
#' @return results is a list with the lag, statistic and p-values
#' @details
#' This function computes  a sequence of multivariate Portmanteau tests using a modified version of
#' the original portmanteau.multi internal function of the vars package
#'
#' @export
#' @importFrom stats embed pchisq pf resid
#'
#' @examples
#' \dontrun{
#'  Sigma <- matrix(c(10,3,3,2),2,2)
#'  dat.norm<- MASS::mvrnorm(n = 1000, rep(0, 2), Sigma)
#'  var.2c <- vars::VAR(dat.norm, p = 1, type = "const")
#'  res.var<-resid(var.2c)
#'  recursive.portmanteau.multi(res.var,1,20)
#' }

recursive.portmanteau.multi <- function(resids, var.p.lag, max.lag)
{
  pvalues	<-	numeric(max.lag);
  tstat   <-	numeric(max.lag);
  for (i in 1:max.lag)
  {
    port.test  <- portmanteau.multi(resids, var.p.lag, i)
    tstat[i]	 <- port.test$PT1$statistic;
    pvalues[i] <- port.test$PT1$p.value;
  }
  lag		=	1:max.lag;
  pvalues	=	pvalues;
  results<-cbind(lag,tstat,pvalues)
  return(results);
}
