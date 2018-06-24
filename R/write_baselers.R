#' write baselers data to file
#'
#' @param nsim number of simulations
#' @param seed seed
#'
#' @export
#'
#' @importFrom readr write_csv
write_baselers <- function(nsim = 10000, seed = 1) {

  baselers <- simulate_baselers(nsim = nsim, seed = seed)

  # if(file.exists("inst/extdata/baselers.txt")) {file.remove("inst/extdata/baselers.txt")}

  readr::write_csv(x = baselers, path = "inst/extdata/baselers.txt")

  # if(file.exists("data/baselers.RData")) {file.remove("data/baselers.RData")}

  save(baselers, file = "data/baselers.RData")

  message("data/baselers.RData and inst/extdata/baselers.txt saved!")

}
