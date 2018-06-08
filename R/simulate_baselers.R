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
simulate_baselers <- function(nsim = 1000,
                              corMat_f = NULL,
                              corMat_m = NULL,
                              seed = NULL){

  # some sources:
  # age: https://www.bfs.admin.ch/bfs/de/home/statistiken/kataloge-datenbanken/tabellen.assetdetail.3202980.html
  # weight, height: https://www.laenderdaten.info/durchschnittliche-koerpergroessen.php
  # tattoo: https://www.migrosmagazin.ch/tattoos-ohne-grenzen
    # income: https://www.srf.ch/news/schweiz/mit-7100-franken-pro-monat-ueberleben-oder-etwa-doch-weniger
  # consultations: https://www.krankenkasse-vergleich.ch/news/das-schweizer-gesundheitssystem-in-der-oecd-statistik, https://www.bfs.admin.ch/bfs/de/home/statistiken/kataloge-datenbanken/tabellen.assetdetail.250255.html

  if (!is.null(seed) && is.numeric(seed)){
    set.seed(seed = seed)
  }

  # continuous variables:
  # age, income (per month), weight (kg), height (cm), children,
  # happiness (0 to 10), fitness (0 to 10), food, alcohol, tattoos,
  # rhine (no per month), datause (no of times phone is checked per day),
  # consultations, hiking

  # means and sds for females and males
  mu_f <- c(43.14, 7112, 64, 164, 1.54, 6, 5, 438, 25, 3, 4, 88, 4.5, 60)
  stddev_f <- c(20, 1200, 8, 10, 1.25, 2, 2, 80, 10, 2.8, 2.5, 20, 2, 20)
  var_f <- stddev_f ^ 2

  mu_m <- c(41.01, 7112, 84.9, 178, 1.54, 6, 5, 438, 40, 3.8, 4, 88, 3, 60)
  stddev_m <- c(20, 1200, 12, 11, 1.25, 2, 2, 80, 15, 2.8, 2.5, 20, 1.5, 25)
  var_m <- stddev_m ^ 2

  if (is.null(corMat_f)){
    # correlation matrix females
    corMat_f <- matrix(c(   var_f[1],   .3,  -.1, -.15,   .2,   .1, -.25,  -.1,    0, -.45, -.15, -.23,   .5,    0,
                           .3,    var_f[2],    0,    0,  -.1,  .15,    0,   .5,   .2, -.08,    0,    0, -.05,    0,
                          -.1,    0,    var_f[3],   .6,    0,    0,  -.3,    0,  .15,    0,    0,    0,  .15, -.15,
                         -.15,    0,   .6,    var_f[4],    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
                           .2,  -.1,    0,    0,    var_f[5],    0,    0,   .4,    0,    0,  -.1,    0,    0,    0,
                           .1,  .15,    0,    0,    0,    var_f[6],  .15,   .1,    0,    0,   .2,  -.3, -.15,  .25,
                         -.25,    0,  -.3,    0,    0,  .15,    var_f[7],    0, -.05,  .15,   .2,    0,  -.1,   .3,
                          -.1,   .5,    0,    0,   .4,   .1,    0,    var_f[8],    0,    0,    0,    0,    0,    0,
                            0,   .2,  .15,    0,    0,    0, -.05,    0,    var_f[9],    0,    0,    0,  .15,    0,
                         -.45, -.08,    0,    0,    0,    0,  .15,    0,    0,    var_f[10],    0,    0,    0,    0,
                         -.15,    0,    0,    0,  -.1,   .2,   .2,    0,    0,    0,    var_f[11],    0,    0,   .1,
                         -.23,    0,    0,    0,    0,  -.3,    0,    0,    0,    0,    0,    var_f[12],    0,    0,
                           .5, -.05,  .15,    0,    0, -.15,  -.1,    0,  .15,    0,    0,    0,    var_f[13], -.15,
                            0,    0, -.15,    0,    0,  .25,   .3,    0,    0,    0,   .1,    0, -.15,    var_f[14]),
                     ncol = 14)
  }

  if (is.null(corMat_m)){
    # correlation matrix for males
    corMat_m <- matrix(c(   var_m[1],   .3,  -.1, -.15,   .2,   .1, -.25,  -.1,    0, -.25, -.15, -.23,   .5,    0,
                           .3,    var_m[2],    0,    0,  -.1,  .15,    0,   .5,   .2, -.08,    0,    0, -.05,    0,
                          -.1,    0,    var_m[3],   .6,    0,    0,  -.3,    0,  .15,    0,    0,    0,  .15, -.15,
                         -.15,    0,   .6,    var_m[4],    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
                           .2,  -.1,    0,    0,    var_m[5],    0,    0,   .4,    0,    0,  -.1,    0,    0,    0,
                           .1,  .15,    0,    0,    0,    var_m[6],  .15,   .1,    0,    0,   .2,  -.3, -.15,  .25,
                         -.25,    0,  -.3,    0,    0,  .15,    var_m[7],    0, -.05,  .15,   .2,    0,  -.1,   .3,
                          -.1,   .5,    0,    0,   .4,   .1,    0,    var_m[8],    0,    0,    0,    0,    0,    0,
                            0,   .2,  .15,    0,    0,    0, -.05,    0,    var_m[9],    0,    0,    0,  .15,    0,
                         -.25, -.08,    0,    0,    0,    0,  .15,    0,    0,    var_m[10],    0,    0,    0,    0,
                         -.15,    0,    0,    0,  -.1,   .2,   .2,    0,    0,    0,    var_m[11],    0,    0,   .1,
                         -.23,    0,    0,    0,    0,  -.3,    0,    0,    0,    0,    0,    var_m[12],    0,    0,
                           .5, -.05,  .15,    0,    0, -.15,  -.1,    0,  .15,    0,    0,    0,    var_m[13], -.15,
                            0,    0, -.15,    0,    0,  .25,   .3,    0,    0,    0,   .1,    0, -.15,    var_m[14]),
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
  names(tib_f) <- c("age", "income", "weight", "height", "children",
                    "happiness", "fitness", "food", "alcohol",
                    "tattoos", "rhine", "datause", "consultations",
                    "hiking")

  tib_m <- tibble::as_tibble(mat_m)
  names(tib_m) <- c("age", "income", "weight", "height", "children",
                    "happiness", "fitness", "food", "alcohol",
                    "tattoos", "rhine", "datause", "consultations",
                    "hiking")

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

  tib_f$fasnacht <- sample(c("yes", "no"), size = nrow(tib_f), replace = TRUE,
                                  prob = c(.02, .98))
  tib_m$fasnacht <- sample(c("yes", "no"), size = nrow(tib_m), replace = TRUE,
                                  prob = c(.035, .965))

  tib <- rbind(tib_f, tib_m)

  tib$alcohol <- tib$alcohol + ifelse(tib$fasnacht == "yes",
                                                        runif(1, 0:40), 0)

  tib$eyecor <- sample(c("yes", "no"), size = nsim, replace = TRUE,
                               prob = c(.66, .37))

  tib <- tib[sample(1:nsim),]


  id_scramble <- paste0("bas_", sapply(1:nsim, FUN = function(x) {paste(sample(LETTERS, size = 5, replace = TRUE), collapse = "")}))

  tib <- tib %>%
    mutate(id = id_scramble,
           age = case_when(age < 18 | age > 105 ~ runif(1, 18, 85),
                           TRUE ~ age),
           age = round(age, 0),

           height = round(height, 1),

           # weight
           weight = round(weight, 1),

           ## make 10% of cases NA

           weight = case_when(runif(nsim) < .15 ~ NA_real_,
                               TRUE ~ weight),

           income = round(income / 100, 0) * 100,

           ## make 15% of cases NA

           income = case_when(runif(nsim) < .15 ~ NA_real_,
                              TRUE ~ income),

           children = case_when(children < 0 ~ runif(1, 0, 3),
                                   TRUE ~ children),
           children = round(children),
           happiness = case_when(happiness > 10 ~ runif(1, 6, 9),
                                 happiness < 5 & runif(1, 0, 1) < .35 ~ runif(1, 6, 9),
                                 TRUE ~ happiness),
           happiness = case_when(happiness < 1 ~ runif(1, 1, 10),
                                TRUE ~ happiness),
           happiness = round(happiness),

           fitness = case_when(fitness < 1 | fitness > 10 ~ runif(1, 1, 10),
                               TRUE ~ fitness),
           fitness = round(fitness),
           food = round(food / 10) * 10,

           ### alcohol

           alcohol = case_when(alcohol < 0 ~ runif(1, 5, 50),
                                        TRUE ~ alcohol),
           alcohol = round(alcohol),

           ## make 15% of cases 0

           alcohol = case_when(runif(nsim) < .15 ~ 0,
                               TRUE ~ alcohol),

           ## Tattoos

           tattoos = case_when(tattoos < 0 ~ 0,
                               TRUE ~ tattoos),
           tattoos = round(tattoos),

           ## make 50% of cases 0

           tattoos = case_when(runif(nsim) < .5 ~ 0,
                               TRUE ~ tattoos),


           rhine = case_when(rhine < 0 ~ 0,
                                      TRUE ~ rhine),
           rhine = round(rhine),
           datause = round(datause),
           consultations = case_when(consultations < 0 ~ runif(1, 0, 10),
                                        TRUE ~ consultations),
           consultations = round(consultations),
           hiking = case_when(hiking < 0 ~ 0,
                                          TRUE ~ hiking),
           hiking = round(hiking),

           # confession
           ## make 5% of cases NA

           confession = case_when(runif(nsim) < .15 ~ NA_character_,
                              TRUE ~ confession)


           )


  # Change order a bit

  tib <- tib %>% select(id, sex, age, height, weight, income, education, confession, everything())


  tib
}

