##============================================================================================
##
## ATTENTION: these are utility functions extracted from the "vars" package by Bernhard Pfaff.
## Some of them were partially modified and corrected.
##
##============================================================================================


##
## Convenience function for computing lagged x
##
".matlag1" <-
  function(x, lag = 1){
    totcols <- ncol(x)
    nas <- matrix(NA, nrow = lag, ncol = totcols)
    x <- rbind(nas, x)
    totrows <- nrow(x)
    x <- x[-c((totrows - lag + 1) : totrows), ]
    return(x)
  }
##
## Convenience function for computing lagged residuals
##
".matlag2" <-
  function(x, lag = 1){
    K <- ncol(x)
    obs <- nrow(x)
    zeromat <- matrix(0, nrow = obs, ncol = K * lag)
    idx1 <- seq(1, K * lag, K)
    idx2 <- seq(K, K * lag, K)
    for(i in 1:lag){
      lag <- i + 1
      res.tmp <- embed(x, lag)[, -c(1 : (K * i))]
      zeromat[-c(1 : i), idx1[i] : idx2[i]] <- res.tmp
    }
    resids.l <- zeromat
    return(resids.l)
  }

##
## Multivariate Portmanteau Statistic
##
"portmanteau.multi" <-
  function(resids, var.p.lag, lags.pt){

    if(ncol(resids)==1){
      stop("\n The matrix of residuals contains only 1 column!!!\n")
    }

    K=ncol(resids)
    obs=nrow(resids)
    C0 <- crossprod(resids) / obs
    C0inv <- solve(C0)
    tracesum <- rep(NA, lags.pt)
    for(i in 1 : lags.pt){
      Ut.minus.i <- .matlag1(resids, lag = i)[-c(1 : i), ]
      Ut <- resids[-c(1 : i), ]
      Ci <- crossprod(Ut, Ut.minus.i) / obs
      tracesum[i] <- sum(diag(t(Ci) %*% C0inv %*% Ci %*% C0inv))
    }
    vec.adj <- obs - (1 : lags.pt)
    Qh <- obs * sum(tracesum)
    Qh.star <- obs^2 * sum(tracesum / vec.adj)

    ##Asymptotic version of the test
    STATISTIC <- Qh
    names(STATISTIC) <- "Chi-squared"
    PARAMETER <- (K^2 * (lags.pt - var.p.lag))
    names(PARAMETER) <- "df"
    PVAL <- 1 - pchisq(STATISTIC, df = PARAMETER)
    METHOD <- "Portmanteau Test (asymptotic)"
    PT1 <- list(statistic = STATISTIC, parameter = PARAMETER, p.value = PVAL, method = METHOD)
    class(PT1) <- "htest"

    ## Small sample adjustment of the test
    STATISTIC <- Qh.star
    names(STATISTIC) <- "Chi-squared"
    PARAMETER <- (K^2 * (lags.pt - var.p.lag))
    names(PARAMETER) <- "df"
    PVAL <- 1 - pchisq(STATISTIC, df = PARAMETER)
    METHOD <- "Portmanteau Test (adjusted)"
    PT2 <- list(statistic = STATISTIC, parameter = PARAMETER, p.value = PVAL, method = METHOD)
    class(PT2) <- "htest"

    result <- list(PT1 = PT1, PT2 = PT2)
    return(result)
  }

##
## Breusch-Godfrey and Edgerton-Shukur Test
##
"BG.multi" <-
  function(resids, ylagged, nstar, lags.bg){

    if(ncol(resids)==1){
      stop("\n The matrix of residuals contains only 1 column!!!\n")
    }

    K=ncol(resids)
    obs=nrow(resids)
    ylagged=as.matrix(ylagged)
    resids.l <- .matlag2(resids, lag = lags.bg)
    if(ylagged==0){
      regressors <- as.matrix(resids.l)
      lm0 <- lm(resids ~ -1 + regressors)
      lm1 <- lm(resids ~ -1)
    } else{
      regressors <- as.matrix(cbind(ylagged, resids.l))
      lm0 <- lm(resids ~ -1 + regressors)
      lm1 <- lm(resids ~ -1 + ylagged)
    }

    sigma.1 <- crossprod(resid(lm1)) / obs
    sigma.0 <- crossprod(resid(lm0)) / obs

    ## BG asymptotic tests
    LMh.stat <- obs * (K - sum(diag(crossprod(solve(sigma.1), sigma.0))))
    STATISTIC <- LMh.stat
    names(STATISTIC) <- "Chi-squared"
    PARAMETER <- lags.bg * K^2
    names(PARAMETER) <- "df"
    PVAL <- 1 - pchisq(STATISTIC, df = PARAMETER)
    METHOD <- "Breusch-Godfrey LM test"
    LMh <- list(statistic = STATISTIC, parameter = PARAMETER, p.value = PVAL, method = METHOD)
    class(LMh) <- "htest"

    ## Small sample correction by Edgerton Shukur
    R2r <- 1 - det(sigma.0) / det(sigma.1)
    m <- K * lags.bg
    q <- 0.5 * K * m - 1
    n <- nstar
    N <- obs - n - m - 0.5 * (K - m + 1)
    r <- sqrt((K^2 * m^2 - 4)/(K^2 + m^2 - 5))
    LMFh.stat <- (1 - (1 - R2r)^(1 / r))/(1 - R2r)^(1 / r) * (N * r - q) / (K * m)
    STATISTIC <- LMFh.stat
    names(STATISTIC) <- "F statistic"
    PARAMETER1 <- lags.bg * K^2
    names(PARAMETER1) <- "df1"
    PARAMETER2 <- floor(N * r - q)
    names(PARAMETER2) <- "df2"
    PVAL <-   1 - pf(LMFh.stat, PARAMETER1, PARAMETER2)
    METHOD <- "Edgerton-Shukur F test"
    LMFh <- list(statistic = STATISTIC, parameter = c(PARAMETER1, PARAMETER2), p.value = PVAL, method = METHOD)
    class(LMFh) <- "htest"

    return(list(LMh = LMh, LMFh = LMFh))
  }

##
## multivariate ARCH test
##
"arch.multi" <-
  function(x, lags.multi){

    if(ncol(x)==1){
      stop("\n The matrix of residuals contains only 1 column!!!\n")
    }

    K=ncol(x)
    obs=nrow(x)
    col.arch.df <- 0.5 * K * (K + 1)
    arch.df <- matrix(NA, nrow = obs, ncol = col.arch.df)
    for( i in 1 : obs){
      temp <- outer(x[i,], x[i,])
      arch.df[i,] <- temp[lower.tri(temp, diag=TRUE)]
    }
    lags.multi <- lags.multi + 1
    arch.df <- embed(arch.df, lags.multi)
    archm.lm0 <- lm(arch.df[ , 1:col.arch.df] ~ 1)
    archm.lm0.resids <- resid(archm.lm0)
    omega0 <- cov(archm.lm0.resids)
    archm.lm1 <- lm(arch.df[ , 1 : col.arch.df] ~ arch.df[ , -(1 : col.arch.df)])
    archm.lm1.resids <- resid(archm.lm1)
    omega1 <- cov(archm.lm1.resids)
    R2m <- 1 - (2 / (K * (K + 1))) * sum(diag(omega1 %*% solve(omega0)))
    n <- nrow(archm.lm1.resids)
    STATISTIC <- 0.5 * n * K * (K+1) * R2m
    names(STATISTIC) <- "Chi-squared"
    lags.multi <- lags.multi - 1
    PARAMETER <- lags.multi * K^2 * (K + 1)^2 / 4
    names(PARAMETER) <- "df"
    PVAL <- 1 - pchisq(STATISTIC, df = PARAMETER)
    METHOD <- "ARCH (multivariate)"
    result <- list(statistic = STATISTIC, parameter = PARAMETER, p.value = PVAL, method = METHOD)

    class(result) <- "htest"
    return(result)
  }

returns = function( x, type = 'r', n = 1 ) {
  if( !is.numeric( x ) ) return( x )
  N = length( x )
  if( n >= N ) stop( 'not enough data' )
  returns = switch( type,
                    r = c( rep( 0, n ), x[ -( 1:n ) ] / x[ -( N:( N - n + 1 ) ) ] - 1 ),
                    l = c( rep( 1, n ), log( x[ -( 1:n ) ] / x[ -( N:( N - n + 1 ) ) ] ) )
  )
}
