#' The length of a string (in characters).
#'
#' @param df dataframe containing the ACSSS study data
#' @param iter the year/location to build edges for
#' @export
#' @import dplyr
#' @importFrom DiagrammeR create_edge_df
#' @importFrom tidyr separate
build_edge_dataframe <- function(
  df,
  iter,
  disc.col.palette = NULL,
  missing.disc.col = "#888888"
  ) {

  if (is.null(disc.col.palette)) {
    disc.col.palette <- c(
      "Humanities" = "#771155",
      "Journalism and information" = "#AA4488",
      "Law" = "#CC99BB",
      "Social services" = "#114477",
      "#4477AA", "Arts" = "#77AADD",
      "Physical sciences" = "#117777",
      "Life sciences" = "#44AAAA", "#77CCCC",
      "Agriculture and forestry and fishery" = "#117744", "#44AA77",
      "Environmental protection" = "#88CCAA",
      "Computing" = "#777711",
      "Engineering and engineering trades" = "#AAAA44",
      "Mathematics and statistics" = "#DDDD77",
      "Architecture and building" = "#774411", "#AA7744",
      "Business and administration" = "#DDAA77",
      "Social and behavioural sciences" =  "#771122", "#AA4455",
      "Health" = "#DD7788"
    ) # end disc.col.palette
  } # end if(is.null...


  edge.df.temp <- df %>%
    # Filter by the year
    filter(Iteration == iter) %>%
    # Select only project and person info
    dplyr::select(Title, name1 = Name, Year) %>%
    # Re-join the original dataframe
    left_join(df, by = c("Title", "Year")) %>%
    # Remove duplicate people
    filter(name1 != Name) %>%
    # Select all relevant info
    dplyr::select(Title, name1, name2 = Name, Topic, Topic_isced, Year) %>%
    arrange(name1) %>%
    mutate(name1 = as.factor(name1),
           name2 = as.factor(name2),
           name.id1 = as.numeric(droplevels(name1)),
           name.id2 = as.numeric(droplevels(name2)),
           name.link = paste(name.id1, name.id2, sep = "_"))

  edge.df <- edge.df.temp %>%
    dplyr::select(name.id1, name.id2) %>%
    mutate(name.link = ifelse(name.id1<name.id2, paste(name.id1, name.id2, sep = "_"), paste(name.id2, name.id1, sep = "_"))) %>%
    dplyr::select(name.link) %>%
    unique() %>%
    left_join(edge.df.temp, by = "name.link") %>%
    tidyr::separate(col = Topic_isced, into = c("topic.disc1", "topic.disc2"),
             sep = "[;,]", extra = "drop", remove = FALSE, fill = "right")

  edge.df$edge.color <- disc.col.palette[edge.df$topic.disc1]
  edge.df$edge.color[is.na(edge.df$edge.color)] <- missing.disc.col

  edge.df <- create_edge_df( ## from the DiagrammeR package
    from = edge.df$name.id1,
    to = edge.df$name.id2,
    color = edge.df$edge.color,
    dir = rep("none", length(edge.df$name.id1)))

  return(edge.df)
}
