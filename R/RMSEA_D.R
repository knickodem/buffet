#' Compute RMSEA_D
#'
#' Function to compute RMSEA_D (Savalei et al., 2023) from nested multigroup models in lavaan (Rosseel, 2012).
#' The function also computes the 90% confidence interval and performs an equivalence test based on a cutoff value.
#' Incorporates code from Savalei et al., 2023 supplemental materials found here: https://osf.io/2ycd6/
#'
#' @param mod0 lavaan object for the less constrained model
#' @param mod1 lavaan object for the more constrained model (i.e., mod1 is nested within mod0)
#' @param cutoff The cutoff value for which to perform an equivalence test using
#'  the 90% confidence interval. When the upper bound of the 90% CI is below
#'  \code{cutoff} the null hypothesis that RMSEA_D > cutoff is statistically significant at alpha = .05


RMSEA_D <- function(mod0, mod1, cutoff = .10){

  # suggested to use default chisqure difference from LRT rather than
  # computing by hand from fit indices: https://groups.google.com/g/lavaan/c/6kiW0a_Y64A/m/0HUnVGaZAwAJ
  comp <- lavaan::lavTestLRT(mod0, mod1)
  D <- comp$`Chisq diff`[[2]]
  dfD <- comp$`Df diff`[[2]]
  n = lavaan::inspect(mod0, "ntotal")
  g = lavaan::inspect(mod0, "ngroups")


  if(D < dfD){
    RMSEA_D <- 0
    ci <- c(NA, NA, NA)
    warning("D = ", D, " and dfD = ", dfD, "; when D < dfD, RMSEAd is set to 0.")
    # Warning is based on statement from Savalei et al (2023) on p.3.
  } else{

    RMSEA_D <- sqrt((D - dfD) / (dfD*(n - g)/g))

    ci <- RMSEA.CI(D, dfD, n, g, cutoff)

  }
  return(data.frame(RMSEA_D = RMSEA_D, Lower_CI = ci[[1]], Upper_CI = ci[[2]], p = ci[[3]]))
}

# adapted from https://osf.io/ne5ar
RMSEA.CI <- function(T, df, N, G, cutoff = .10){

  cutoff2 <- cutoff/2

  #functions taken from lavaan (lav_fit_measures.R)
  lower.lambda <- function(lambda) {
    (pchisq(T, df=df, ncp=lambda) - (1-cutoff2))
  }
  upper.lambda <- function(lambda) {
    (pchisq(T, df=df, ncp=lambda) - cutoff2)
  }

  #RMSEA CI
  lambda.l <- try(uniroot(f = lower.lambda, lower = 0, upper = T)$root, silent=TRUE)
  if(inherits(lambda.l, "try-error")) { lambda.l <- NA; RMSEA.CI.l<-NA
  } else { if(lambda.l < 0){
    RMSEA.CI.l = 0
  } else {
    RMSEA.CI.l <- sqrt(lambda.l*G/((N-1)*df))
  }
  }

  N.RMSEA <- max(N, T*4)
  lambda.u <- try(uniroot(f=upper.lambda, lower=0,upper=N.RMSEA)$root,silent=TRUE)
  if(inherits(lambda.u, "try-error")) { lambda.u <- NA; RMSEA.CI.u<-NA
  } else { if(lambda.u<0){
    RMSEA.CI.u=0
  } else {
    RMSEA.CI.u<-sqrt(lambda.u*G/((N-1)*df))
  }
  }

  # computing  p-value
  eps0<-df*cutoff^2/G
  nonc<-eps0*(N-G)
  pval<-pchisq(T,df=df,ncp=nonc)

  RMSEA.CI <- c(RMSEA.CI.l,RMSEA.CI.u, pval)
  return(RMSEA.CI)
}

