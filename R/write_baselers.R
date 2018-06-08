#' write baselers data to file
#'
#' @param nsim number of simulations
#' @param seed seed
#'
#' @export
#'
write_baselers <- function(nsim = 10000, seed = 1) {

  baselers <- simulate_baselers(nsim = nsim, seed = seed)

  write_csv(x = baselers, path = "inst/extdata/baselers.txt")

  save(baselers, file = "data/baselers.RData")

  message("data/baselers.RData and inst/extdata/baselers.txt saved!")
}
