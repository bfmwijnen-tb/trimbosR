
#'  FUNCTIE: calc_prevalentie_gewogen()
#'
#'  Doel: Gewogen aantallen en percentages berekenen
#'  voor een categorische variabele binnen een survey-design.
#'
#' @param var naam van de variabele (string).
#' @param dsgn een svydesign object (met gewichten).
#'
#' @return weigthed precentages
#' @importFrom labelled to_factor
#' @importFrom survey svymean
#' @importFrom survey svytotal
#' @importFrom survey svydesign
#' @importFrom tibble tibble
#' @export
#' @examples
#' data("df_example")
#' ## Gebruik je survey-design zoals het is
#' design <- survey::svydesign(
#'  id      = ~RESPNR,          # of ID
#'  weights = ~WT1,             # jouw gewichtvariabele
#'  data    = df_example        # jouw dataset
#')
#'## Gebruik voor één variabele:
#' calc_prevalentie_gewogen("yypandi_Ax1", design)
#'
calc_percentage_gewogen <- function(var, dsgn) {

  # Zet variabele altijd netjes om naar factor (labels indien aanwezig)
  dsgn$variables[[var]] <- labelled::to_factor(dsgn$variables[[var]])

  # Formule
  f <- as.formula(paste0("~", var))

  # Gewogen proporties
  pct <- survey::svymean(f, dsgn, na.rm = TRUE)

  # Gewogen aantallen
  n <- survey::svytotal(f, dsgn, na.rm = TRUE)

  tibble::tibble(
    Variabele  = var,
    Categorie  = levels(dsgn$variables[[var]]),
    n          = round(as.numeric(n), 0),
    Percentage = round(as.numeric(pct) * 100, 2)
  )
}
