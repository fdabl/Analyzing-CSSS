#' The length of a string (in characters).
#'
#' @param unprocessed Dataframe containing the unprocessed data
#' @export
process_acsss_data <- function(unprocessed) {
  processed <- unprocessed %>%
    dplyr::mutate(
      Iteration = interaction(Year, Location),
      Country_University = as.character(Country_University),
      Country_University = ifelse(
        Country_University %in% c('US', 'USA', 'United States'), 'USA', Country_University
      )
    )

  return(processed)
}
