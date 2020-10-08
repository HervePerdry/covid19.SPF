#' Data aggregation
#'
#' @param x a data frame
#' @param cols the columns to aggregate
#' @param by grouping elements
#' @param FUN function used to aggregate the data
#' @param ... further parameters passed to `aggregate` or to `FUN`
#'
#' @details This function is a convenient wrapper for \code{\link{aggregate}}.
#' It is implemented as `aggregate( x[,cols], x[, by], FUN, ...)`.
#' 
#' @return a data frame, with the data in `x[,cols]\` aggregated according 
#' to the levels of the factors in `x[,by]`.
#' 
#' 
#' @examples
#' agg(departements, cols = c("superficie", "population"), by = "région")
#' 
#' @export
agg <- function(x, cols, by, FUN =sum, ...) {
  aggregate(x[, cols, drop = FALSE],  x[, by, drop = FALSE], FUN, ...)
}
