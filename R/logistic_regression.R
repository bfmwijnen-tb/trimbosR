#' FUNCTIE: log_regressie_gewogen()
#'
#' De functie is bedoeld om gewogen logistische regressies uit te voeren voor één of meerdere binaire uitkomstmaten, met een categorische voorspeller met twee of meer niveaus. Het uitvoeren van gewogen logistische regressies zonder interacties voor meerdere binaire uitkomstmaten. Voor elke uitkomstmaat worden alle unieke paarsgewijze vergelijkingen tussen de niveaus van een categorische onafhankelijke variabele uitgevoerd (bijv. bij 3 niveaus: 3 vergelijkingen).
#' @param dep vector met afhankelijke variabelen (binaire factoren, 0/1)
#' @param ind categorische voorspeller (factor met ≥ 2 niveaus)
#' @param correct optionele vector met controlevariabelen (bv. leeftijd, opleiding)
#' @param design svydesign-object (survey design met gewichten)
#'
#' @return Tabel met per uitkomstmaat en per vergelijking
#' @importFrom survey svyglm
#' @importFrom survey svytotal
#' @importFrom survey svydesign
#' @importFrom dplyr case_when
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @export log_regressie_gewogen
#' @examples
#' library(trimbosR)
#' data("df_example")
#' library(survey)
#' df_example$Education[df_example$Education == "NA" | is.na(df_example$Education)] <- "unknown" ## Missende waarde opvullen
#' df_example$Marijuana <- ifelse(df_example$Marijuana == "YES", 1,
#' ifelse(df_example$Marijuana == "NO", 0, NA))  ## Change YES/NO to 1 - 0 (still includes NAs)
#' df_example$HardDrugs <- ifelse(df_example$HardDrugs == "YES", 1,
#' ifelse(df_example$HardDrugs == "NO", 0, NA))  ## Change YES/NO to 1 - 0 (still includes NAs)
#' ## Survey-design instellen
#' design <- survey::svydesign(
#' ids = ~ID,
#' strata = ~Education,
#' weights = ~WT1,
#' data = df_example,
#' nest = TRUE
#' )
#' ###  Gebruik voor één of meerdere uitkomstmaten:
#' log_regressie_gewogen(
#' dep = c("HardDrugs", "Marijuana"),  # afhankelijke vars
#' ind = "Age",                        # onafhankelijke var
#' correct = "Gender",                 # covariaten
#' design = design                     # survey-design
#' )
#'
log_regressie_gewogen <- function(dep, ind, correct = NULL, design) {

  stopifnot(length(ind) == 1)

  resultaten_all <- list()

  # Zorg dat predictor een factor is
  design$variables[[ind]] <- as.factor(design$variables[[ind]])
  levels_ind <- levels(design$variables[[ind]])

  # Loop over uitkomsten -----------------------------------
  for (uitkomst in dep) {

    resultaten <- list()

    voorspellers <- c(ind, correct)
    form <- as.formula(
      paste(uitkomst, "~", paste(voorspellers, collapse = " + "))
    )

    # Hulpfunctie -------------------------------------------
    bereken_vergelijking <- function(design_obj, ref, comp) {

      fit <- survey::svyglm(form, design = design_obj, family = quasibinomial())
      sm  <- summary(fit)$coefficients

      coef_name <- paste0(ind, comp)
      row <- which(rownames(sm) == coef_name)
      if (length(row) != 1) return(NULL)

      est <- sm[row, 1]
      se  <- sm[row, 2]
      p   <- sm[row, 4]

      OR  <- exp(est)
      CI_low  <- exp(est - 1.96 * se)
      CI_high <- exp(est + 1.96 * se)

      d <- round(log(OR) / (sqrt(3) / pi), 2)

      effect <- dplyr::case_when(
        p < 0.05 & d >= 0.8  ~ "+++",   # let op: pas hier p-level aan als het nodig is
        p < 0.05 & d >= 0.5  ~ "++",
        p < 0.05 & d >= 0.2  ~ "+",
        p < 0.05 & d <= -0.8 ~ "---",
        p < 0.05 & d <= -0.5 ~ "--",
        p < 0.05 & d <= -0.2 ~ "-",
        TRUE ~ "n.s."
      )

      tibble::tibble(
        Referentie    = ref,
        Categorie     = comp,
        Afhankelijk   = uitkomst,
        Onafhankelijk = ind,
        p_waarde      = p,
        OR_95_CI      = sprintf("%.2f (%.2f - %.2f)", OR, CI_low, CI_high),
        Cohen_d       = d,
        Effectgrootte = effect
      )
    }

    # Unieke paarsgewijze vergelijkingen --------------------
    for (i in seq_len(length(levels_ind) - 1)) {

      ref <- levels_ind[i]

      design2 <- design
      design2$variables[[ind]] <- relevel(
        design2$variables[[ind]],
        ref = ref
      )

      for (comp in levels_ind[(i + 1):length(levels_ind)]) {
        resultaten[[length(resultaten) + 1]] <-
          bereken_vergelijking(design2, ref, comp)
      }
    }

    resultaten_all[[length(resultaten_all) + 1]] <-
      dplyr::bind_rows(resultaten)
  }

  dplyr::bind_rows(resultaten_all)
}


