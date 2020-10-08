#' Add an "admissions" column
#'
#' @param X a data frame
#' @details Add a column with a the number of admissions, 
#' computed as \code{diff(X$hosp) + diff(X$rad) + diff(X$dc)}.
#' Special care is given to the case of missing days.
#' @return An identical data frame with an "admissions" column.
#' @export
#'
#' @examples
add.admissions <- function(X) {
  if(!all( c("hosp", "rad", "dc") %in% names(X) ))
    stop("Can't compute admissions")

  d <- diff(X$jour)
  if(any(d <= 0))
    stop("Entries not in chronological order or duplicate entries")

  adm <- diff(X$hosp) + diff(X$rad) + diff(X$dc)
  adm <- unlist( mapply(function(x,n) rep(x/n,n), adm, d) )
  jou <- X$jour[1] + seq_along(adm)
  merge( X, data.frame(jour = jou, admissions = adm), by = "jour", all.x = TRUE, all.y = TRUE)
}
