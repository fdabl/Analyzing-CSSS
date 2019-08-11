#' The length of a string (in characters).
#'
#' @param path path to the ACSSS study data file
#' @export
load_acsss_data <- function(path) {
  dat <- read.csv(path, na.strings = '') %>%
    dplyr::mutate(
      Bio_sentiment = SentimentAnalysis::analyzeSentiment(as.character(Biography))$SentimentQDAP,
      Iteration = interaction(Year, Location),
      Country_University = as.character(Country_University),
      Country_University = ifelse(
        Country_University %in% c('US', 'USA', 'United States'), 'USA', Country_University
      )
    )
  return(dat)
}
