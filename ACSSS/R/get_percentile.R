#' Given a distribution and a value, x, returns the percentile value
#' of x given the distirbution
#'
#' @param dist the distribution from which to compute the ecdf
#' @param x the value for which we want to know the percentile
#' @export
get_percentile <- function(dist, x) {
  ecdf_out <- ecdf(dist)
  return(ecdf_out(x))
}
