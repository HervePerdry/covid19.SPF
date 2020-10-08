#' @export
xtfrm.dept <- function(x) {
  x <- ifelse( x == "2A", "20", ifelse(x == "2B", "20.5", x) )
  as.numeric(x)
}
