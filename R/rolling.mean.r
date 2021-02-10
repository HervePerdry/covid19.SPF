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

rolling.mean <- function(x, n = 7, align = c("center", "left", "right")) {
  align <- match.arg(align)
  if(align == "center")
    as.vector(filter(x, rep(1/n,n)))
  else if(align == "right") 
    as.vector(filter(x, rep(1/n,n), sides = 1))
  else
    c( as.vector(filter(x, rep(1/n,n), sides = 1))[-seq(1,n-1)], rep(NA, n-1) )
}
