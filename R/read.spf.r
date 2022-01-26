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
#' covid <- read.spf("nouveaux")
#' covid <- agg(covid, 3:6, 2)
#' covid$rolling_hosp <- rolling.mean(covid$incid_hosp, 7, "right")
#' covid$rolling_rea  <- rolling.mean(covid$incid_rea,  7, "right")
#' covid$rolling_dc   <- rolling.mean(covid$incid_dc,   7, "right")
#' covid$rolling_rad  <- rolling.mean(covid$incid_rad,  7, "right")
#' 
#' 
#' plot(covid$jour, covid$incid_dc, type = "h")
#' lines(covid$jour, covid$rolling_dc, col = "red", lwd = 2)
#' 
read.spf <- function( file = c("-", "nouveaux", "classe-age", "etablissements") ) { 
  file <- match.arg(file)

  if(file == "-") 
    file <- ""
  else
    file <- paste0(file, "-")

#  reg <- paste0("https.*donnees-hospitalieres-", file, "covid19-\\d\\d\\d\\d-\\d\\d-\\d\\d-\\d\\dh\\d\\d\\.csv")
#
#  x <- scan("https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/", character(), quiet = TRUE, sep = "\n")
#  n <-  grep( paste0("url=", reg), x, perl = TRUE)
#  url <- URLdecode(regmatches(x[n], regexpr(reg, x[n], perl = TRUE)))

  if(file == "") {
    url <- "https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7"
  } else if(file == "nouveaux-") {
    url <- "https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c"
  } else if(file == "classe-age-") {
    url <- "https://www.data.gouv.fr/fr/datasets/r/08c18e08-6780-452d-9b8c-ae244ad529b3"
  } else {
    url <- "https://www.data.gouv.fr/fr/datasets/r/41b9bd2a-b5b6-4271-8878-e45a8902ef00"
  }

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

