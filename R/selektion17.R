#' Erzeugen der Variable Selektion_17
#'
#' @param data der Ausgangsdatensatz
#' @param a erste Variable
#' @return Erzeugen der Variable Selektion_17 aus \code{a}
#' @import dplyr
#' @export
selektion17 <- function(data, a) {
  rename(data, Sel_Selektion17 = all_of(a)) %>%
    mutate(Sel_Selektion17 = replace(Sel_Selektion17, Sel_Selektion17 == "keins", "")) %>%
    mutate(Sel_Selektion17 = replace(Sel_Selektion17, Sel_Selektion17 == "Newsletter-Abo", "begrüßen"))
}
