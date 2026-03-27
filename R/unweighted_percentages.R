
#' FUNCTIE: calc_percentage_ongewogen()
#'
#' Doel:Ongewogen aantallen en percentages berekenen voor één of meerdere categorische variabelen.
#'
#' @param vars variabelenaam of vector met variabelen
#' @param data is dataset
#' @param include_na is TRUE/FALSE (missings meenemen als categorie)
#'
#' @return unweigthed precentages
#' @importFrom labelled to_factor
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @export
#' @examples
#' Hier moet nog een voorbeeld komen
#'
calc_percentage_ongewogen <- function(vars, data, include_na = FALSE) {

  # Check input
  if (!all(vars %in% names(data))) {
    missend <- vars[!vars %in% names(data)]
    stop("Variabelen niet gevonden in data: ", paste(missend, collapse = ", "))
  }

  # Bepaal hoe NA behandeld wordt
  na_optie <- if (include_na) "ifany" else "no"

  resultaten <- lapply(vars, function(var) {

    # Factor maken (met labels)
    x <- labelled::to_factor(data[[var]])

    # Frequenties
    n <- table(x, useNA = na_optie)
    pct <- prop.table(n)

    tibble::tibble(
      Variabele = var,
      Categorie = names(n),
      n = as.numeric(n),
      Percentage = round(as.numeric(pct) * 100, 2)
    )
  })

  dplyr::bind_rows(resultaten)
}
