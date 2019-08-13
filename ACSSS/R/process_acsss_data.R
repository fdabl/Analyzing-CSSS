#' The length of a string (in characters).
#'
#' @param unprocessed Dataframe containing the unprocessed data
#' @export
process_acsss_data <- function(unprocessed) {
  processed <- unprocessed %>%
    dplyr::mutate(
      Iteration = as.character(interaction(Year, Location)),
      Iteration_name = gsub("[.]", " ", Iteration),
      Iteration = gsub(" ", "", Name, fixed = T),
      Country_University = as.character(Country_University),
      Country_University = ifelse(
        Country_University %in% c('US', 'USA', 'United States'), 'USA', Country_University
      )
    )

  return(processed)
}
