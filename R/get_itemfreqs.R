#' Item Frequencies
#'
#' Gather item frequencies (including NAs) for multiple items
#' with similar response options.
#'
#' @param data a \code{data.frame} containing the variables (i.e., items)
#' @param items <[`tidy-select`][dplyr::dplyr_tidy_select]> The columns in `data`
#' to gather response frequencies. Default is to use all columns.
#' @param group optional column name in `data` by which to group response frequencies.
#' @param NAto0 logical; convert NAs in frequency table to 0s?
#'
#' @return data.frame
#'
#' @examples
#' data("bfi", package = "psych")
#' get_itemfreqs(data = bfi, items = A1:A5)
#'
#' @import dplyr
#'
#' @export
#' @md

get_itemfreqs <- function(data, items = names(data),
                          group = NULL, NAto0 = TRUE){

  freqs <- data |>
    select({{items}}, {{group}}) |>
    tidyr::pivot_longer(cols = {{items}}, names_to = "Item", values_to = "Response") |>
    group_by({{group}}, Item, Response) |>
    summarize(n = n(), .groups = "drop") |>
    tidyr::spread(Response, n)

  if(NAto0 == TRUE){

    freqs <- freqs |>
      mutate(across(
        .cols = where(is.numeric), ~tidyr::replace_na(.x, 0)
        ))
  }
  return(freqs)
}
