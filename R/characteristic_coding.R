#' Coding between person characteristics in longitudinal data
#'
#' When data is collected across multiple time points, some characteristics are expected to
#' be stable over time, but respondents may give inconsistent and contradictory responses.
#' This function offers an avenue for examining inconsistencies and four methods for
#' resolving conflicts to produce a single value for each respondent across all time points.
#' The methods are particularly useful in the presence of check-all-that-apply type responses
#' at each time point.

#### Coding demographic responses for each subject within or across waves #####
## Note: Removes subjects with NA for all responses across all waves
## options for Approach argument are:
# "all_responses" - Produces df with a row for each subject x wave x demographic category response
# "within_wave" - Determines all response categories endorsed within each wave (a row for each subject x wave)
# The remaining 3 categories produces one code across waves. The output is a df with one row for each subject
# "multiples"  - If subject gave multiple responses within or across waves, code as Multiple
# "priority" - Same as default unless subject gave response specified in PriorityResponse argument at any point, then code as PriorityResponse
# "mode"     - Use the mode response, with ties coded as Multiple; can also prioritize response by specifying PriorityResponse argument
characteristic_coding <- function(data, ItemPrefix, RecodeValues = NULL, NewName = ItemPrefix, Approach, PriorityResponse = NULL){

  ## Produces df with a row for each subject x wave x demographic category response
  Coding <-  data %>%
    select(SUBJECT_ID, Wave, contains(ItemPrefix)) %>%  # Keep only subject ID, wave, and Sexual Orientation variables
    gather(newCol, Response, contains(ItemPrefix)) %>%  # Pivot data to longer format with one row for each subject x wave x gender combination
    mutate(newCol = gsub(ItemPrefix, "", newCol))       # Removing redundant string characters

  if(!is.null(RecodeValues)){

    Coding <- mutate(Coding,
                     newCol = recode(newCol, !!!RecodeValues))  # Specifying demographic category names rather than relying on previous column names
  }

  if(Approach == "all_responses"){

    Output <- Coding %>%
      rename({{NewName}} := newCol)

  } else if(Approach == "within_wave"){

    ## Determines all response categories endorsed within each wave (a row for each subject x wave)
    Output <- Coding %>%
      mutate(Response = ifelse(Response == 1, newCol, NA)) %>%                     # Changing values of 1 to the name it represents
      spread(newCol, Response) %>%                                                # Pivot data to wider format with one row for each subject x wave combination
      unite(col = "newCol", -SUBJECT_ID, -Wave, remove = TRUE, na.rm = TRUE) %>%  # remove = FALSE allows us to examine if the uniting worked
      mutate(newCol = ifelse(newCol == "", NA, newCol)) %>%                       # If subject responded NA to all categories for all waves, code as NA rather than ""
      rename({{NewName}} := newCol)                                               # Note: Combinations are currently in the order in which the variable appears in the dataset

  } else {

    ## Summarizes each subject's responding pattern across waves
    AcrossWavesPrep <- Coding %>%
      filter(!is.na(Response)) %>%                    # Removing subjects with NA across all waves
      group_by(SUBJECT_ID, newCol) %>%
      summarize(n_waves = n(), .groups = "drop_last") # number of waves a subject identified as a certain characteristic


    if(Approach == "multiples"){

      # Combines any multiple responses as Multiple
      Output <- AcrossWavesPrep %>%
        summarize(Demo = case_when(n() == 1 ~ newCol,                        # gave the same response across waves
                                   TRUE ~ "Multiple"), .groups = "keep") %>% # If subject > 1 response, they were categorized as Multiple
        unique() %>%                                                         # removes redundant rows; produces unique code for each subject
        rename({{NewName}} := Demo)


    } else if(Approach == "priority"){

      if(is.null(PriorityResponse)){

        stop("Must specify PriorityResponse when Approach = `priority`.")


      } else if(length(PriorityResponse) == 1){


        # Prioritizing a response
        Output <- AcrossWavesPrep %>%
          summarize(Demo = case_when(sum(newCol == PriorityResponse) > 0 ~ PriorityResponse, # If subject gave PriorityResponse at any point, code ast PriorityResponse
                                     n() == 1 ~ newCol,                                      # gave the same response across waves
                                     TRUE ~ "Multiple"), .groups = "keep") %>%               # gave multiple response across waves
          unique() %>%                                                                       # removes redundant rows; produces unique code for each subject
          rename({{NewName}} := Demo)

      } else {} ## NEED TO FIGURE OUT HOW TO IMPLEMENT FEATURE WHERE PriorityResponse CAN BE c("Priority1", "Priority2"). vECTOR FOR PriorityResponse CAN VARY IN LENGTH


    } else if(Approach == "mode"){

      # The Mode response with ties coded as multiple
      if(is.null(PriorityResponse)){

        Intermediate <- AcrossWavesPrep %>%
          summarize(Demo = case_when(n() == 1 ~ newCol,                             # gave the same response across waves
                                     sum(n_waves == max(n_waves)) > 1 ~ "Multiple", # ties - gave multiple response with equal frequency
                                     n_waves == max(n_waves) ~ newCol,              # gave multiple response across waves but one at a higher frequency
                                     TRUE ~ "Temp"), .groups = "keep")              # temporary code given to rows that will be removed


      } else if(length(PriorityResponse) == 1){

        # The Mode response with ties coded as multiple and prioritizing PriorityResponse
        Intermediate <- AcrossWavesPrep %>%
          summarize(Demo = case_when(sum(newCol == PriorityResponse) > 0 ~ PriorityResponse,  # If subject gave PriorityResponse at any point, code as PriorityResponse
                                     n() == 1 ~ newCol,                                       # gave the same response across waves
                                     sum(n_waves == max(n_waves)) > 1 ~ "Multiple",           # ties - gave multiple response with equal frequency
                                     n_waves == max(n_waves) ~ newCol,                        # gave multiple response across waves but one at a higher frequency
                                     TRUE ~ "Temp"), .groups = "keep")                        # temporary code given to rows that will be removed

      } else{} ## NEED TO FIGURE OUT HOW TO IMPLEMENT FEATURE WHERE PriorityResponse CAN BE c("Priority1", "Priority2"). vECTOR FOR PriorityResponse CAN VARY IN LENGTH

      Output <- Intermediate %>%
        filter(Demo != "Temp") %>%
        unique() %>%                     # removes redundant rows; produces unique code for each subject
        rename({{NewName}} := Demo)

    } else {stop("For a single demographic code for each subject across all waves set Approach to `multiples`, `priority`, or `mode`.")}


  }

  return(Output)
}
