#' Create codebook for a dataset
#'
#' The function extracts and organizes variable attributes into a dataframe that can be used for other R functions or exported.
#'
#' @param data a \code{data.frame}
#' @param export.type current options are \code{"excel"} and \code{"csv"}
#' @param export.name character string indicating file path and file name for exported codebook. File extention is added by the function and should not be included here.
#' @param return.object should the codebook dataframe be returned?
#'
#' @details
#' If \code{export.type = NULL} & \code{return.object = FALSE}, the function does not output anything.
#'
#' @return data.frame
#'
#' @export
create_codebook <- function(data,
                            export.type = NULL, export.name = "MyCodebook",
                            return.object = FALSE){

  ## Extract Values and Labels
  ValueLabels <- get_labels(data)

  ## Putting in wide format
  VLW <- ValueLabels %>%
    mutate(VL = paste(Value, Label, sep = " = ")) %>%
    group_by(Item) %>%
    summarize(Value_Label = paste(VL, collapse = "; ")) %>%
    ungroup()

  ## Extract Item Stem (i.e. label), then joining values and labels
  Stems <- lapply(data, function(x) attr(x,"label")) %>%
    tibble::enframe("Item","Stem")

  ## Joining Stems, Values, and Labels (gets us 90% of the way to the final codebook)
  Codebook <- left_join(Stems, VLW, by = "Item") %>%
    mutate(Stem = as.character(Stem))

  if(!is.null(export.type)){
    if(export.type == "excel"){

      ## Exporting to excel file
      openxlsx::write.xlsx(x = Codebook, file = paste0(export.name, ".xlsx"), asTable = FALSE)
      message("Codebook exported to file ", export.name, ".xlsx")

    } else if(export.type == "csv"){

      ## Exporting to csv file
      write.csv(Codebook,file = paste0(export.name, ".csv"), row.names = FALSE, na = "")
      message("Codebook exported to file ", export.name, ".csv")

    }
  }

  if(return.object == TRUE){
    return(Codebook)
  }
}

#' Extracting labels from factor and labelled variables
#'
#' @param data a \code{data.frame} with observations in rows and variables in columns
#'
#' @return a \code{data.frame} with a row for each value-label pair
#'
#' @export

get_labels <- function(data){

  if(sum(sapply(data, haven::is.labelled)) > 0){

    ## Select labelled variables, extract labels, and transform to long format dataframe
    llbls_temp <- lapply(select_if(data, haven::is.labelled), function(x) attr(x,"labels"))

    # if values are a mix of numeric and character
    if(length(unique(sapply(llbls_temp, class))) != 1){

      llbls_num <- llbls_temp[sapply(llbls_temp, class) == "numeric"] %>%
        tibble::enframe("Item","Value") %>%
        mutate(Label = lapply(Value, function(x) attr(x,"names"))) %>%
        tidyr::unnest(cols = c(Value, Label)) %>%
        mutate(Value = as.character(Value)) # need to convert to character for combining

      llbls <- llbls_temp[sapply(llbls_temp, class) == "character"] %>%
        tibble::enframe("Item","Value") %>%
        mutate(Label = lapply(Value, function(x) attr(x,"names"))) %>%
        tidyr::unnest(cols = c(Value, Label)) %>%
        bind_rows(llbls_num) %>%
        mutate(Item = factor(Item, levels = names(llbls_temp))) %>%
        arrange(Item)

    } else {

      llbls <- llbls_temp %>%
        tibble::enframe("Item","Value") %>%
        mutate(Label = lapply(Value, function(x) attr(x,"names"))) %>%
        tidyr::unnest(cols = c(Value, Label))
    }

  } else {
    llbls <- NULL
  }

  if(sum(sapply(data, is.factor)) > 0){

    ## Extract labels and transform to long format dataframe
    flbls <- lapply(select_if(data, is.factor), levels) %>%
      tibble::enframe("Item","Label") %>%
      mutate(Value = lapply(Label, function(x) 1:length(x))) %>%
      tidyr::unnest(cols = c(Value, Label))

  } else {
    flbls <- NULL
  }

  if(!is.null(llbls) & !is.null(flbls)){
    if(class(llbls$Value) == "character"){
      flbls$Value <- as.character(flbls$Value)
    }

    all_labels <- bind_rows(llbls, flbls) %>%
      mutate(Item = factor(Item, levels = names(data)) %>%
               forcats::fct_drop()) %>%
      arrange(Item)

    return(all_labels)

  } else if(!is.null(llbls)){
    return(llbls)
  } else if(!is.null(flbls)){
    return(flbls)
  }
}
