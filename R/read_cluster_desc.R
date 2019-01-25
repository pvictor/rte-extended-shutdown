
#' Read Cluster Description
#'
#' @param path Path to Excel file
#'
#' @return a \code{data.table}
#' @export
#' 
#' @importFrom readxl read_excel
#' @importFrom janitor clean_names
#' @importFrom data.table setDT :=
#' @importFrom zoo na.locf
#'
#' @examples
#' \dontrun{
#' 
#' cluster_desc <- read_cluster_desc("HypothesesRTE_CHO-4145.xlsx")
#' 
#' }
#' 
read_cluster_desc <- function(path) {
  clusdesc <- read_excel(path = path, sheet = 2, skip = 5)
  clusdesc <- janitor::clean_names(clusdesc)
  setDT(clusdesc)
  
  # column with all missing values
  clusdesc[, x7 := NULL]
  # column with code groups
  setnames(clusdesc, "x8", "corresp_groupes")
  # rows with missing values
  clusdesc <- clusdesc[!is.na(nom)]
  
  # type of groups
  clusdesc[is.na(pcn_mw), type_groupe := nom]
  clusdesc[, type_groupe := zoo::na.locf(type_groupe)]
  
  clusdesc <- clusdesc[!is.na(pcn_mw)]
  clusdesc
}
