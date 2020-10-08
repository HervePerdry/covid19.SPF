#' Read Santé Publique France data
#'
#' @param file which file to read
#'
#' @details This function reads one the files 
#' described at \url{https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/}
#' and do some cleaning.
#' @return A data frame
#' @export
#'
#' @examples
#' cov <- read.spf("nouveaux")
#' cov <- agg(cov, 3:6, 2)
#' plot(cov$jour, cov$incid_rea, log = "y", type = "o")
#' lines(cov$jour, rolling.mean(cov$incid_rea), col = "red", lwd = 2)
#' 
read.spf <- function( file = c("-", "nouveaux", "classe-age", "etablissements") ) { 
  file <- match.arg(file)
  if(file == "-") 
    file <- ""
  else
    file <- paste0(file, "-")

  reg <- paste0("https.*donnees-hospitalieres-", file, "covid19-\\d\\d\\d\\d-\\d\\d-\\d\\d-\\d\\dh\\d\\d\\.csv")

  x <- scan("https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/", character(), quiet = TRUE, sep = "\n")
  n <-  grep( paste0("url=", reg), x, perl = TRUE)
  url <- URLdecode(regmatches(x[n], regexpr(reg, x[n], perl = TRUE)))
  cat("Downloading \n", url, "\n")
  T <- read.csv2(url)
  T$jour <- as.Date(T$jour)

  # il y a quelques lignes sans indication de département...
  if("dep" %in% names(T)) {
    T <- T[ T$dep != "", ] 
    T$dep <- as.dept(T$dep)
  }
  # il y a des inversions de jour !
  # -> classement propre
  if(file == "classe-age-")
    T <- T[ order(T$jour, T$reg, T$cl_age90), ]
  else if(file == "nouveaux-") 
    T <- T[ order(T$jour, T$dep), ]
  else if(file == "etablissements-")
    T <- T[ order(T$jour, T$dep), ]
  else
    T <- T[ order(T$jour, T$dep, T$sexe), ]
  rownames(T) <- NULL
  T
}

