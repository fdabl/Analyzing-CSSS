#' The length of a string (in characters).
#'
#' @param unprocessed Dataframe containing the unprocessed data
#' @export
process_acsss_data <- function(unprocessed) {
  processed <- unprocessed %>%
    dplyr::mutate(
      Iteration = as.character(interaction(Year, Location)),
      Iteration_display = gsub("[.]", " ", Iteration),
      Iteration = gsub(" ", "", Iteration, fixed = T),
      Country_University = as.character(Country_University),
      Country_University = ifelse(
        Country_University %in% c('US', 'USA', 'United States'), 'USA', Country_University
      ),
      Prestige = ifelse(Prestige == "Top 50", "Top 50", "Not Top 50"),
      Position = ifelse(tolower(Position) %in% c("student", "postdoc", "professor"),
                        "academia",
                        "not academia"),
      Discipline_isced = ifelse(Discipline_isced %in%
                                c("Education",
                                  "Arts",
                                  "Social and behavioural science",
                                  "Journalism and information",
                                  "Business and administration"),
                                "Social Science", "Physical and Natural Science")
    )

  return(processed)
}
