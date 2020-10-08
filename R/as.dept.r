#' Creates a object of class \code{dept}
#'
#' @param x a character vector
#'
#' @details The class \code{dept} allow to handle french départements. 
#' Methods have been implemented (in particular for \code{xtfrm}) 
#' to ensure that départements \code{"2A"} and \code{"2B"} are inserted between
#' \code{"19"} and \code{"21"} when sorting an object of class \code{dept}.
#' 
#' @return an object of class \code{dept}
#' @export
#'
as.dept <- function(x) {
  structure(as.character(x), class = "dept")
}

#' @export
print.dept <- function(x, ...) {
  print(unclass(x), ...)
  invisible(x)
}

#' @export
`[.dept` <- function(x, i) {
  class(x) <- "character"
  structure(x[i], class = "dept")
}

#' @export
as.character.dept <- function(x, ...) {
  unclass(x)
}

# ceci est nécessaire pour pouvoir insérer un dept dans un data frame (avec data.frame() )
#' @export
as.data.frame.dept <- function (x, row.names = NULL, optional = FALSE, ..., nm = deparse1(substitute(x))) {
  force(nm)
  class(x) <- "character"
  as.data.frame(x, row.names = row.names, optional = optional, ..., nm = nm)
}

# ceci est nécessaire pour les facteurs créés à partir d'un dept
# aient le bon ordre ! (et que aggregate donne le bon ordre par ricochet)
# en effet les niveaux d'un facteur sont créés à partir de unique(x) (réordonné)
#' @export
unique.dept <- function(x, ...) {
  structure( unique(unclass(x), ...), class = "dept")
}

