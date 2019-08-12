#' The length of a string (in characters).
#'
#' @param df dataframe containing the ACSSS study data
#' @param iter the year/location to build nodes for
#' @export
#' @import dplyr
#' @importFrom DiagrammeR create_node_df
#' @importFrom tidyr separate
build_node_dataframe <- function(
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

  # Load dataframe for a particular year
  partic.df <- df %>%
    filter(Iteration == as.character(iter)) %>%
    select(Name, Discipline_isced, Nationality, Gender, Position, Country_University, Prestige, Location) %>%
    unique() %>%
    mutate(
      Name = as.factor(Name),
      name.id = as.numeric(droplevels(Name))) %>%
    # Separate listed disciplines into separate columns
    tidyr::separate(col = Discipline_isced, into = c("disc1", "disc2"),
             sep = "[;,]", extra = "drop", remove = FALSE, fill = "right")

  # Set the disciplinary color palette
  partic.df$disc.color <- disc.col.palette[partic.df$disc1]
  # Set the color for missing disciplines
  partic.df$disc.color[is.na(partic.df$disc.color)] <- missing.disc.col

  # Build final node dataframe using the DiagrammeR function
  node.df <- DiagrammeR::create_node_df(
    n = nrow(partic.df),
    node_id = partic.df$name.id,
    label = partic.df$Name,
    fillcolor = partic.df$disc.color,
    year = as.character(iter),
    discp = partic.df$Discipline_isced,
    nat = partic.df$Nationality,
    pos.var = partic.df$Position,
    cntry = partic.df$Country_University,
    prstg = partic.df$Prestige,
    gender = partic.df$Gender
  )

  return(node.df)
}
