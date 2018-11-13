
#' Create clusters
#'
#' @param calendar Calendar data read with \code{read_calendar}.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{setSimulationPath} 
#'
#' @export
#'
#' @importFrom antaresRead simOptions
#' @importFrom antaresEditObject createCluster
#' @importFrom lubridate hours days
#' @importFrom stats setNames
#' @importFrom stringr str_replace_all
create_clusters <- function(calendar, opts = simOptions()) {
  
  # Modulation data
  modulation_list <- lapply(
    X = setNames(
      object = unique(calendar$tranche), 
      nm = unique(calendar$tranche)
    ),
    FUN = function(cluster) {
      dat <- calendar[tranche == cluster]
      if (nrow(dat) == 0) {
        matrix(
          data = c(
            rep(1, times = 8760 * 3),
            rep(0, times = 8760 * 1)
          ),
          ncol = 4
        )
      } else {
        datetime_study <- seq(from = as.POSIXct("2018-07-01", tz = "UTC"), length.out = 8760, by = "1 hour")
        datetime_study <- as.character(datetime_study)
        datetime_prolongation <- lapply(
          X = seq_len(nrow(dat)), 
          FUN = function(i) {
            if (dat$date_de_fin_sans_prolongation[i] > dat$date_debut[i]) {
              res <- seq(
                from = dat$date_debut[i], 
                to = dat$date_de_fin_sans_prolongation[i] - hours(1), 
                by = "1 hour"
              )
              as.character(res)
            }
          }
        )
        datetime_prolongation <- unlist(datetime_prolongation)
        capacity_modulation <- (!datetime_study %in% datetime_prolongation) * 1
        matrix(
          data = c(
            rep(1, times = 8760 * 2),
            capacity_modulation,
            rep(0, times = 8760 * 1)
          ),
          ncol = 4
        )
      }
    }
  )
  
  # Preprop data
  data_list <- lapply(
    X = setNames(
      object = unique(calendar$tranche), 
      nm = unique(calendar$tranche)
    ),
    FUN = function(cluster) {
      dat <- calendar[tranche == cluster]
      if (nrow(dat) == 0) {
        matrix(
          data = c(
            rep(1, times = 365 * 2),
            rep(0, times = 365 * 3),
            rep(1, times = 365 * 1)
          ),
          ncol = 6
        )
      } else {
        date_study <- seq(from = as.Date("2018-07-01"), length.out = 365, by = "1 day")
        date_reprise <- which(as.character(date_study) %in% as.character(dat$date_de_fin_sans_prolongation - days(1)))
        duree_prolongation_mean <- dat$duree_prolongation_mean[as.character(dat$date_de_fin_sans_prolongation) %in% as.character(date_study + days(1))]
        res <- matrix(
          data = c(
            rep(1, times = 365 * 2),
            rep(0, times = 365 * 3),
            rep(1, times = 365 * 1)
          ),
          ncol = 6
        )
        res[date_reprise, 2] <- duree_prolongation_mean
        res[date_reprise, 4] <- 1
        return(res)
      }
    }
  )
  
  for (cluster in unique(calendar$tranche)) {
    opts <- createCluster(
      opts = opts,
      area = "area", 
      cluster_name = str_replace_all(string = cluster, pattern = "[^[:alnum:]]", replacement = "_"), 
      add_prefix = FALSE,
      group = "nuclear",
      unitcount = 1L,
      nominalcapacity = 1000,
      `min-stable-power` = 100,
      `must-run` = FALSE,
      `min-down-time` = 1L,
      `min-up-time` = 168L,
      `volatility.planned` = 1,
      prepro_data = data_list[[cluster]], 
      prepro_modulation = modulation_list[[cluster]]
    )
  }

  invisible(opts)
}


