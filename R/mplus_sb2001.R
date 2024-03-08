#' Satorra-Bentler (2001) Scaled Chisquare
#'
#' Calculate the Satorra-Bentler (2001) scaled (mean-adjusted) chisquare difference test
#' for nested models run in Mplus and imported using MPlusAutomation.
#' Models should be estimated with MLM, MLR, or WLSM (https://statmodel.com/chidiff.shtml)
#'
#' @param m0 nested model
#' @param m1 comparison model
#' @param add.index should change (delta) in fit index values (CFI, RMSEA, SRMR) be added to the output?
#'
#' @return single row dataframe
#'
#' @export

mplus_sb2001 <- function(m0, m1, add.index = TRUE, digits = 2){

  d0 <- m0$summaries$ChiSqM_DF
  c0 <- m0$summaries$ChiSqM_ScalingCorrection
  T0 <- m0$summaries$ChiSqM_Value
  d1 <- m1$summaries$ChiSqM_DF
  c1 <- m1$summaries$ChiSqM_ScalingCorrection
  T1 <- m1$summaries$ChiSqM_Value

  cd <- (d0 * c0 - d1 * c1) / (d0 - d1)

  Td <- (T0 * c0 - T1 * c1) / cd
  df <- d0 - d1
  p <- pchisq(Td, df, lower.tail = FALSE)

  out <- data.frame(delta.x2 = Td,
                    delta.df = df,
                    delta.p = p)

  if(add.index == TRUE){
  # fit index differences
  cfi <- m0$summaries$CFI - m1$summaries$CFI
  rmsea <- m0$summaries$RMSEA_Estimate - m1$summaries$RMSEA_Estimate
  srmr <- m0$summaries$SRMR - m1$summaries$SRMR

  out <- cbind(out, data.frame(delta.cfi = cfi,
                              delta.rmsea = rmsea,
                              delta.srmr = srmr))

  }
  out <- apply(out, 2, function(x) format(round(.x, digits), nsmall = digits))
  return(out)
}
