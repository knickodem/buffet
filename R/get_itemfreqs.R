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

# ----   Proportions and Frequencies   ----

get_itemprops <- function(data, items, labels = NULL){

  tbl_props <- map_dfr(.x = items,
                       ~data |>
                         janitor::tabyl(!!sym(.x)) |>
                         mutate(item = .x) |>
                         rename_with(~ "response", 1)) |>
    mutate(item = as_factor(item) |> fct_rev())
  if(!is.null(labels)){
    tbl_props <- tbl_props |>
      mutate(response = factor(response, levels = 1:length(labels),
                               labels = labels))
  }
  return(tbl_props)
}

####   Plotting  ####

plot_itemprops <- function(itemprops, group = NULL){

  if(!is.null(group)){
    plot <- itemprops %>%
      ggplot(aes(x = percent, y = .data[[group]], fill = response, label = n)) +
      geom_bar(stat = "identity") +
      geom_text(size = 4, position = position_stack(vjust = .5)) +
      scale_fill_brewer(name = "Response",
                        palette = "Set1",
                        guide = guide_legend(reverse = TRUE)) +
      labs(y = NULL, x = "Proportion") +
      theme_bw(base_size = 18) +
      facet_wrap(~item)

  } else {
    plot <- itemprops |>
      ggplot(aes(x = percent, y = item, fill = response, label = n)) +
      geom_bar(stat = "identity") +
      geom_text(size = 4, position = position_stack(vjust = .5)) +
      scale_fill_brewer(name = "Response",
                        palette = "Set1",
                        guide = guide_legend(reverse = TRUE)) +
      labs(y = NULL, x = "Proportion") +
      theme_bw(base_size = 18)
  }


  return(plot)
}
