#' baselers
#'
#' A tibble containing simulated data of basler inhabitants. Most mean values are based on actual populationstatistics, however correlations between variables aren't.
#'
#'
#' @format A tibble containing 150000 rows and 20 columns
#' \describe{
#'   \item{id}{(numeric) - A unique identifier (1 through 150000).}
#'   \item{age}{(numeric) - A peson's age in years.}
#'   \item{household_income}{(numeric) - Income of the household a person lives in.}
#'   \item{weight}{(numeric) - A person's weight in kg.}
#'   \item{height}{(numeric) - A person's height in cm.}
#'   \item{no_children}{(numeric) - The number of children a person has.}
#'   \item{happiness}{(numeric) - How happy a person is on a scale from 0 to 10.}
#'   \item{fitness}{(numeric) - A person's fitness level rated from 0 to 10.}
#'   \item{food_expenses}{(numeric) - How much (CHF) a person spends on food per month.}
#'   \item{alcohol_expenses}{(numeric) - How much (CHF) a person spends on alcohol beverages per month.}
#'   \item{tattoos}{(numeric) - The number of tattoos a person has.}
#'   \item{rhine_swimming}{(numeric) - How often a person goes swimming in the rhine per month.}
#'   \item{data_use}{(numeric) - How many times a day a person checks an app on the mobile phone.}
#'   \item{no_consultations}{(numeric) - The number of times a person's sees a doctor per year.}
#'   \item{anual_hiking_hours}{(numeric) - How many hours a person spends hiking per year.}
#'   \item{sex}{(character) - A person's sex. Either "male" or "female".}
#'   \item{education}{(character) - The highest completed degree/ education a person has. Levels are "obligatory_school", "apprenticeship", "SEK_II", "SEK_III".}
#'   \item{confession}{(character) - A person's confession. Levels are "catholic", "confessionless", "evangelical-reformed", "muslim", "other".}
#'   \item{fasnacht_active}{(character) - Whether a person actively (i.e. in a Gugge, Clique or similar) participates at fasnacht. Levels are "yes" and "no".}
#'   \item{eye_correction}{(character) - Whether a person needs eye correction. Levels are "yes" and "no".}
#'  }
#' @source Simulated with the simulate_baselers function with nsim = 150000, and seed = 1
"baselers"