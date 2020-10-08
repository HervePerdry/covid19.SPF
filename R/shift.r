
shift <- function(x, n) {
  if(n == 0) 
   x
  else if(n < 0)
   c( rep(NA,-n), x[ seq(1, length(x)+n) ] )
  else
   c( x[-(1:n)], rep(NA,n) )
}
