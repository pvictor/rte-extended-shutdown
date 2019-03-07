
#' Compute Kipr coefficient
#'
#' @param calendar Calendar data read with \code{\link{read_calendar}}.
#' @param clusters_desc Clusters / groups description read with \code{\link{read_cluster_desc}}.
#'
#' @return a \code{data.table}
#' @export
#' 
#' @importFrom data.table copy setorder := uniqueN 
#'
#' @examples
#' \dontrun{
#' # Calendar data
#' calendar <- read_calendar(path = "REF_Planning_5_ans_mars_2018.xlsx")
#' 
#' # Clusters description
#' clusters <- read_cluster_desc("HypothesesRTE_CHO-4145.xlsx")
#' 
#' 
#' # Kipr computation
#' kipr <- compute_kipr(calendar, clusters)
#' kipr
#' }
compute_kipr <- function(calendar, clusters_desc) {
  
  calendar <- copy(calendar)
  clusters_desc <- copy(clusters_desc)
  
  weekcal <- build_weekcal()
  
  weekcal[, .id := 1]
  calendar[, .id := 1]
  
  week_groups <- weekcal[calendar[, list(
    .id,
    group = tranche,
    shutdown_start = as.Date(date_de_fin_sans_prolongation) + 1,
    shutdown_end = as.Date(date_de_fin_avec_prolongation)
  )], on = ".id", allow.cartesian = TRUE]
  
  week_groups[, n_overlaps := n_overlaps(week_start, week_end, shutdown_start, shutdown_end)]
  
  weekclus <- merge(
    x = week_groups, 
    y = clusters_desc[, list(group = corresp_groupes, type_groupe, pcn_mw)],
    by = "group", all.x = TRUE, all.y = FALSE
  )
  weekclus <- weekclus[!is.na(type_groupe)]
  weekclus[, pcn_mw := as.numeric(pcn_mw)]
  
  setorder(weekclus, week, group, -n_overlaps)
  weekclus <- unique(weekclus, by = c("week", "group"))
  
  coef_kipr <- weekclus[, list(
    kipr = sum(pcn_mw * n_overlaps / 7) / sum(pcn_mw) * 100,
    n_days = sum(n_overlaps), group_power = sum(pcn_mw),
    n = .N, n_group = uniqueN(group)
  ), by = list(week, week_start, week_end, type_groupe)]
  setorder(coef_kipr, week_start, type_groupe)
  coef_kipr[]
}
