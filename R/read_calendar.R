
#' Read calendar data
#'
#' @param path Path to a file.
#'
#' @return a \code{data.table}
#' @export
#' 
#' @importFrom data.table setDT :=
#' @importFrom janitor clean_names
#' @importFrom readxl read_excel
#'
read_calendar <- function(path) {
  dat <- read_excel(path = path, skip = 3)
  dat <- clean_names(dat)
  setDT(dat)
  dat[, duree_prolongation_jour := as.numeric(difftime(date_de_fin_avec_prolongation, date_de_fin_sans_prolongation, units = "day"))]
  dat[, duree_prolongation_mean := as.numeric(duree_prolongation_semaine) * 7 + 1]
  dat[]
}



