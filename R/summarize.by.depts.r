#' Summarizing covid data by region
#'
#' @param x a data frame
#' @param departements a vector of departments, defining a region
#' @param n length of rolling average
#'
#' @details This function summarizes data obtained from \code{read.spf("nouveaux")},
#' aggregating data from the specified departements and adding a n-last-days rolling 
#' average. Default is \code{n = 7}.
#' 
#' @return a data frame 
#' 
#' 
#' @examples
#' Data <- read.spf("nouveaux")
#' # summarizes for department = 75
#' Data.75 <- summarize.by.depts(Data, 75)
#' tail(Data.75, 10)
#' # for Ile-de-France
#' idf <- departements$numéro[ departements$code_région == 11 ] 
#' Data.idf <- summarize.by.depts(Data, idf)
#' tail(Data.idf, 10)
#' # plot for last 100 days
#' X <- tail(Data.idf, 100)
#' plot(X$jour, X$incid_hosp, log = "y", type = "o")
#' lines(X$jour, X$rolling_hosp, lwd = 2, col = "red")
#' 
#' @export
summarize.by.depts <- function(x, departements, n = 7) {
  U <- subset(x, dep %in% departements)
  U <- agg(U, c("incid_hosp", "incid_rea", "incid_dc", "incid_rad"), "jour")
  U$rolling_rea  <- rolling.mean(U$incid_rea, n, "right")
  U$rolling_hosp <- rolling.mean(U$incid_hosp, n, "right")
  U$rolling_dc   <- rolling.mean(U$incid_dc, n, "right")
  U$rolling_rad  <- rolling.mean(U$incid_rad, n, "right")
  U
}

