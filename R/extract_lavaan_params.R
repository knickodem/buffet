#' @title Extract lavaan parameters.
#'
#' @description
#' Extracts standardized or unstandardized parameter estimates from a \code{lavaan} object.
#'
#' @param object A \code{lavaan} object (e.g., \code{\link[lavaan]{cfa}},
#' \code{\link[lavaan]{sem}}).
#' @param std If "no" (default), unstandardized estimates are returned.
#' Otherwise specify the type of standardization ("std.all", "std.lv", or "std.nox")
#' which is passed to \code{\link[lavaan]{standardizedSolution}}.
#' @param params A single character or character vector of parameters to be
#' included in the output. Default is "all" parameters.
#' @param ... additional arguments passed to \code{\link[lavaan]{parameterEstimates}} or
#' \code{\link[lavaan]{standardizedSolution}}.
#'
#' @details
#' Similar to \code{\link[broom]{tidy.lavaan}} but includes option for filtering
#' which parameters to include in the output. Additionally, uses
#' \code{\link[lavaan]{standardizedSolution}} instead of
#' \code{\link[lavaan]{parameterEstimates}} so standard errors and tests are based on
#' the standardized estimates rather than the unstandardized estimates.
#'
#' @return a data.frame containing parameter estimates, confidence interval,
#' standard error, test statistic, and p-value.
#'
#' @examples
#' library(lavaan)
#' HS.model <- ' visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9 '
#' fit <- cfa(HS.model, data = HolzingerSwineford1939)
#'
#' # all unstandardized parameters
#' extract_lavaan_params(fit)
#'
#' # only standardized factor loadings and correlations
#' extract_lavaan_params(fit, std = "std.all", parameters = c("=~", "~~"))
#'
#' @export

extract_lavaan_params <- function(object, std = "no", params = "all", ...){

  if(params == "all"){

    params = c("=~", "~~", "~*~", "~1", "~", "|", "<~")

  }

  if(std == "no"){

    TheParams <- parameterEstimates(object, standardized = FALSE, ...) %>%
      filter(op %in% params)

  } else if(std %in% c("std.all", "std.lv", "std.nox")){

    TheParams <- standardizedSolution(object, type = std, ...) %>%
      filter(op %in% params)

  } else {
    stop("std argument must be 'no' for unstandardized estimates
         or one of 'std.all', std.lv', or 'std.nox' for standardized estimates")
  }


  return(TheParams)
}
