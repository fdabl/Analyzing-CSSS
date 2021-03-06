% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/homophily_functions.R
\name{get_perc_sim}
\alias{get_perc_sim}
\title{Percent Similar
Percentage of alters that share the same trait as ego
Finds the number of similar alters/total number of alters}
\usage{
get_perc_sim(nodes, edges, column)
}
\arguments{
\item{nodes}{the dataframe containing the network nodes}

\item{edges}{the dataframe contianing the network edges}

\item{column}{the name of the column (as a string) to calculate homophily for}
}
\description{
Range
+ 0 is completely heterophilious
+ 1 is completely homophilious
}
