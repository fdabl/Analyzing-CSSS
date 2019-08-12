#' Herfindahl-Hirschman Index (HHI)
#' Sum of market shares of groups, where an increase in value indicates increasing
#' concentration of alters in one large group while a decrease in value suggests a
#' more equal distribution of alters among smaller groups.
#'
#' sum of the square(number of alters in given group/total alters) for all possible groups
#'
#' Range
#' + 0 is completely heterophilious
#' + 10,000 represents perfect homophily
#'
#' @param nodes the dataframe containing the network nodes
#' @param edges the dataframe contianing the network edges
#' @param column the name of the column (as a string) to calculate homophily for
#' @export
get_hhi <- function(nodes, edges, column) {
  results <- as.data.frame(unique(nodes$id))
  results$hhi <- NULL
  for(i in unique(nodes$id)) {
    cur.edges <- edges[edges$from == i, ]
    cur.edges <- cur.edges[-1]
    colnames(cur.edges) <- c("from", "id", "rel","color", "dir")
    cur.edges <- left_join(cur.edges, nodes, by = "id")

    if(!0 %in% dim(cur.edges) & !is.null(dim(cur.edges))) {
      tab <- as.data.frame(table(cur.edges[ ,column]))
      shares <- tab[tab$Freq != 0, ]

      # The hhi function prefers that shares sum to 100
      shares$prop <- round(shares$Freq / sum(shares$Freq) * 100, 0)

      d <- shares[ , 'prop']
      hhi <- sum(d ^ 2)
      results$hhi[i] <- hhi
    }
  }
  return(results)
}

#' Krackhardt and Stern E-I Index
#' Relative density of within-group connections compare to number of extra-group connections
#'
#' number of ties in different groups - number of ties in same group/ number of total ties
#'
#' Range
#' + -1 is completely homophilious
#' + 1 is completely heterophilious
#'
#' @param nodes the dataframe containing the network nodes
#' @param edges the dataframe contianing the network edges
#' @param column the name of the column (as a string) to calculate homophily for
#' @export
get_ei <- function(nodes, edges, column) {
  results <- as.data.frame(unique(nodes$id))
  results$ei <- 0
  for(i in unique(nodes$id)) {
    cur.edges <- edges[edges$from == i, ]
    cur.edges <- cur.edges[-1]
    colnames(cur.edges) <- c("from", "id", "rel","color", "dir")
    cur.edges <- left_join(cur.edges, nodes, by = "id")

    if(!is.null(dim(cur.edges))) {
      check <- as.character(nodes[nodes$id == i, column])
      num.same <- nrow(cur.edges[cur.edges[ ,column] == check, ])
      num.dif <- nrow(cur.edges[cur.edges[ ,column] != check, ])
      results$ei[i] <- as.numeric((num.same - num.dif)/nrow(cur.edges))
    }
  }
  return(results)
}

#' Percent Similar
#' Percentage of alters that share the same trait as ego
#' Finds the number of similar alters/total number of alters
#'
#' Range
#' + 0 is completely heterophilious
#' + 1 is completely homophilious
#'
#' @param nodes the dataframe containing the network nodes
#' @param edges the dataframe contianing the network edges
#' @param column the name of the column (as a string) to calculate homophily for
#' @export
get_perc_sim <- function(nodes, edges, column) {
  results <- as.data.frame(unique(nodes$id))
  results$sim <- 0
  for(i in unique(nodes$id)) {
    cur.edges <- edges[edges$from == i, ]
    cur.edges <- cur.edges[-1]
    colnames(cur.edges) <- c("from", "id", "rel","color", "dir")
    cur.edges <- left_join(cur.edges, nodes, by = "id")

    if(!is.null(dim(cur.edges))) {
      check <- as.character(nodes[nodes$id == i, column])
      num.same <- nrow(cur.edges[cur.edges[ ,column] == check, ])
      results$sim[i] <- num.same / nrow(cur.edges)
    }
  }
  return(results)
}
