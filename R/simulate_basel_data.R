#' Simulate baselers dataset
#'
#' This function can be used to simulate nsim participants. It draws samples from
#' a mulitvariate normal distribution with a prespecified correlation matrix (corMat_f
#' for women and corMat_m for men). Most (but not all) of the means (but not the
#' correlations) are based on actual data of the swiss population. Details can be
#' found in the data helpfile you can call with ?baselers if the package is loaded.
#'
#' @param nsim integer. The number of participants to simulate (same number of men
#' and women). If an uneven number is specified, the returned dataset will contain
#' one additional row.
#' @param corMat_f double matrix. A symmetric correlation matrix. If not specified
#' the default matrix is used.
#' @param corMat_m double matrix. A symmetric correlation matrix. If not specified
#' the default matrix is used.
#' @param seed integer. Is passed to set.seed, specify the same number to replicate
#' results. If not provided, results will vary randomly.
#'
#' @export
#'
#' @return A tibble with nsim rows, containing continous and categorical simulated
#' data of inhabitants of Basel.
#' @import dplyr
#' @importFrom stats runif
simulate_baselers <- function(nsim = 100, corMat_f = NULL, corMat_m = NULL,
                              seed = NULL){

  # some sources:
  # age: https://www.bfs.admin.ch/bfs/de/home/statistiken/kataloge-datenbanken/tabellen.assetdetail.3202980.html
  # weight, height: https://www.laenderdaten.info/durchschnittliche-koerpergroessen.php
  # tattoo: https://www.migrosmagazin.ch/tattoos-ohne-grenzen
  # household_income: https://www.srf.ch/news/schweiz/mit-7100-franken-pro-monat-ueberleben-oder-etwa-doch-weniger
  # no_consultations: https://www.krankenkasse-vergleich.ch/news/das-schweizer-gesundheitssystem-in-der-oecd-statistik, https://www.bfs.admin.ch/bfs/de/home/statistiken/kataloge-datenbanken/tabellen.assetdetail.250255.html

  if (!is.null(seed) && is.numeric(seed)){
    set.seed(seed = seed)
  }

  # continuous variables:
  # age, household_income (per month), weight (kg), height (cm), no_children,
  # happiness (0 to 10), fitness (0 to 10), food_expenses, alcohol_expenses, tattoos,
  # rhine_swimming (no per month), data_use (no of times phone is checked per day),
  # no_consultations, anual_hiking_hours

  # means and sds for females and males
  mu_f <- c(43.14, 7112, 64, 164, 1.54, 6, 5, 438, 25, 3, 4, 88, 4.5, 60)
  # stddev_f <- c(20, 1200, 8, 10, 1.25, 2, 2, 80, 10, 2.8, 2.5, 20, 2, 20)

  mu_m <- c(41.01, 7112, 84.9, 178, 1.54, 6, 5, 438, 40, 3.8, 4, 88, 3, 60)
  # stddev_m <- c(20, 1200, 12, 11, 1.25, 2, 2, 80, 15, 2.8, 2.5, 20, 1.5, 25)


  if (is.null(corMat_f)){
    # correlation matrix females
    corMat_f <- matrix(c(   1,   .3,  -.1, -.15,   .2,   .1, -.25,  -.1,    0, -.45, -.15, -.23,   .5,    0,
                           .3,    1,    0,    0,  -.1,  .15,    0,   .5,   .2, -.08,    0,    0, -.05,    0,
                          -.1,    0,    1,   .6,    0,    0,  -.3,    0,  .15,    0,    0,    0,  .15, -.15,
                         -.15,    0,   .6,    1,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
                           .2,  -.1,    0,    0,    1,    0,    0,   .4,    0,    0,  -.1,    0,    0,    0,
                           .1,  .15,    0,    0,    0,    1,  .15,   .1,    0,    0,   .2,  -.3, -.15,  .25,
                         -.25,    0,  -.3,    0,    0,  .15,    1,    0, -.05,  .15,   .2,    0,  -.1,   .3,
                          -.1,   .5,    0,    0,   .4,   .1,    0,    1,    0,    0,    0,    0,    0,    0,
                            0,   .2,  .15,    0,    0,    0, -.05,    0,    1,    0,    0,    0,  .15,    0,
                         -.45, -.08,    0,    0,    0,    0,  .15,    0,    0,    1,    0,    0,    0,    0,
                         -.15,    0,    0,    0,  -.1,   .2,   .2,    0,    0,    0,    1,    0,    0,   .1,
                         -.23,    0,    0,    0,    0,  -.3,    0,    0,    0,    0,    0,    1,    0,    0,
                           .5, -.05,  .15,    0,    0, -.15,  -.1,    0,  .15,    0,    0,    0,    1, -.15,
                            0,    0, -.15,    0,    0,  .25,   .3,    0,    0,    0,   .1,    0, -.15,    1),
                     ncol = 14)
  }

  if (is.null(corMat_m)){
    # correlation matrix for males
    corMat_m <- matrix(c(   1,   .3,  -.1, -.15,   .2,   .1, -.25,  -.1,    0, -.25, -.15, -.23,   .5,    0,
                           .3,    1,    0,    0,  -.1,  .15,    0,   .5,   .2, -.08,    0,    0, -.05,    0,
                          -.1,    0,    1,   .6,    0,    0,  -.3,    0,  .15,    0,    0,    0,  .15, -.15,
                         -.15,    0,   .6,    1,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
                           .2,  -.1,    0,    0,    1,    0,    0,   .4,    0,    0,  -.1,    0,    0,    0,
                           .1,  .15,    0,    0,    0,    1,  .15,   .1,    0,    0,   .2,  -.3, -.15,  .25,
                         -.25,    0,  -.3,    0,    0,  .15,    1,    0, -.05,  .15,   .2,    0,  -.1,   .3,
                          -.1,   .5,    0,    0,   .4,   .1,    0,    1,    0,    0,    0,    0,    0,    0,
                            0,   .2,  .15,    0,    0,    0, -.05,    0,    1,    0,    0,    0,  .15,    0,
                         -.25, -.08,    0,    0,    0,    0,  .15,    0,    0,    1,    0,    0,    0,    0,
                         -.15,    0,    0,    0,  -.1,   .2,   .2,    0,    0,    0,    1,    0,    0,   .1,
                         -.23,    0,    0,    0,    0,  -.3,    0,    0,    0,    0,    0,    1,    0,    0,
                           .5, -.05,  .15,    0,    0, -.15,  -.1,    0,  .15,    0,    0,    0,    1, -.15,
                            0,    0, -.15,    0,    0,  .25,   .3,    0,    0,    0,   .1,    0, -.15,    1),
                       ncol = 14)
  }


  # if matrices are not positive definite, force them to be
  if (!corpcor::is.positive.definite(corMat_f)){
    corMat_f <- corpcor::make.positive.definite(corMat_f, tol=1e-3)
  }

  if (!corpcor::is.positive.definite(corMat_m)){
    corMat_m <- corpcor::make.positive.definite(corMat_m, tol=1e-3)
  }

  # draw samples from multinormal distribution
  mat_f <- MASS::mvrnorm(n = round(nsim / 2), mu = mu_f, Sigma = corMat_f,
                         empirical = TRUE)
  mat_m <- MASS::mvrnorm(n = round(nsim / 2), mu = mu_m, Sigma = corMat_m,
                         empirical = TRUE)


  tib_f <- tibble::as_tibble(mat_f)
  names(tib_f) <- c("age", "household_income", "weight", "height", "no_children",
                    "happiness", "fitness", "food_expenses", "alcohol_expenses",
                    "tattoos", "rhine_swimming", "data_use", "no_consultations",
                    "anual_hiking_hours")

  tib_m <- tibble::as_tibble(mat_m)
  names(tib_m) <- c("age", "household_income", "weight", "height", "no_children",
                    "happiness", "fitness", "food_expenses", "alcohol_expenses",
                    "tattoos", "rhine_swimming", "data_use", "no_consultations",
                    "anual_hiking_hours")

  tib_f$sex <- "female"
  tib_m$sex <- "male"

  tib_f$education <- sample(c("obligatory_school", "apprenticeship", "SEK_II",
                            "SEK_III"), size = nrow(tib_f), replace = TRUE,
                            prob = c(.146, .447, .093, .314))
  tib_m$education <- sample(c("obligatory_school", "apprenticeship", "SEK_II",
                                   "SEK_III"), size = nrow(tib_m), replace = TRUE,
                                 prob = c(.101, .38, .05, .469))

  tib_f$confession <- ifelse(tib_f$education ==  "SEK_III",
                             sample(c("confessionless", "muslim", "other", "catholic",
                               "evangelical-reformed"), size = nrow(tib_f),
                             replace = TRUE, prob = c(.353, .026, .049, .347, .225)),
                             sample(c("confessionless", "muslim", "other", "catholic",
                                      "evangelical-reformed"), size = nrow(tib_f),
                                    replace = TRUE, prob = c(.253, .051, .074, .372, .25)))

  tib_m$confession <- ifelse(tib_m$education ==  "SEK_III",
                             sample(c("confessionless", "muslim", "other", "catholic",
                                      "evangelical-reformed"), size = nrow(tib_m),
                                    replace = TRUE, prob = c(.353, .026, .049, .347, .225)),
                             sample(c("confessionless", "muslim", "other", "catholic",
                                      "evangelical-reformed"), size = nrow(tib_m),
                                    replace = TRUE, prob = c(.253, .051, .074, .372, .25)))

  tib_f$fasnacht_active <- sample(c("yes", "no"), size = nrow(tib_f), replace = TRUE,
                                  prob = c(.02, .98))
  tib_m$fasnacht_active <- sample(c("yes", "no"), size = nrow(tib_m), replace = TRUE,
                                  prob = c(.035, .965))

  tib <- rbind(tib_f, tib_m)

  tib$alcohol_expenses <- tib$alcohol_expenses + ifelse(tib$fasnacht_active == "yes",
                                                        runif(1, 0:40), 0)

  tib$eye_correction <- sample(c("yes", "no"), size = nrow(tib), replace = TRUE,
                               prob = c(.66, .37))

  tib <- tib[sample(1:nrow(tib)),]

  tib <- tib %>%
    mutate(id = 1:nrow(tib),
           age = case_when(age < 18 | age > 105 ~ runif(1, 18, 85),
                           TRUE ~ age),
           age = round(age, 2),
           no_children = case_when(no_children < 0 ~ runif(1, 0, 3),
                                   TRUE ~ no_children),
           no_children = round(no_children),
           happiness = case_when(happiness > 10 ~ runif(1, 6, 9),
                                 happiness < 5 & runif(1, 0, 1) < .35 ~ runif(1, 6, 9),
                                 TRUE ~ happiness),
           happiness = round(happiness),
           fitness = case_when(fitness < 0 | fitness > 10 ~ runif(1, 0, 10),
                               TRUE ~ fitness),
           fitness = round(fitness),
           food_expenses = round(food_expenses),
           alcohol_expenses = case_when(alcohol_expenses < 0 ~ runif(1, 5, 50),
                                        TRUE ~ alcohol_expenses),
           alcohol_expenses = round(alcohol_expenses),
           tattoos = case_when(tattoos < 0 ~ 0,
                               TRUE ~ tattoos),
           tattoos = round(tattoos),
           rhine_swimming = case_when(rhine_swimming < 0 ~ 0,
                                      TRUE ~ rhine_swimming),
           rhine_swimming = round(rhine_swimming),
           data_use = round(data_use),
           no_consultations = case_when(no_consultations < 0 ~ runif(1, 0, 10),
                                        TRUE ~ no_consultations),
           no_consultations = round(no_consultations),
           anual_hiking_hours = case_when(anual_hiking_hours < 0 ~ 0,
                                          TRUE ~ anual_hiking_hours),
           anual_hiking_hours = round(anual_hiking_hours)
           )

  tib <- tib[, c("id", names(tib)[-ncol(tib)])]

  tib
}

