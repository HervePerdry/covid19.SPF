#' Rolling mean
#'
#' @param x a numeric vector
#' @param n an integer
#' @param ... extra parameters for \code{filter}
#' 
#' @return The rolling mean of \code{x} based on \code{n} points. 
#' This is just a wrapper for \code{as.vector(filter(x, rep(n,1/n), ...))}.
#' @export
#'
#' @examples

rolling.mean <- function(x, n = 7) as.vector(filter(x, rep(1/n,n)))
