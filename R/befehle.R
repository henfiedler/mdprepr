#' Befehle für FileMaker-kompatible Aufbereitung
#'
#' @param data ein Ausgangsdatensatz
#' @param a erste Variable
#' @param b zweite Variable
#' @return Erzeugen der Variable Sel_Bemerkung aus \code{a}, \code{b}.
#' @import dplyr
#' @export
bemerkung <- function(data, a, b) {
  mutate(data, Sel_Bemerkung = paste0("Unterlagen: ",
                                      ifelse(grepl("Mail", a), "Mail", "Post"),
                                      ifelse(b != "",
                                             paste0(", Bem: ", b), "")))
}

#' @param data ein Ausgangsdatensatz.
#' @param a erste Variable
#' @return Erzeugen der Variable Selektion_17 aus \code{a}.
#' @import dplyr
#' @export
selektion17 <- function(data, a) {
  rename(data, Sel_Selektion17 = all_of(a)) %>%
    mutate(Sel_Selektion17 = replace(Sel_Selektion17, Sel_Selektion17 == "keins", "")) %>%
    mutate(Sel_Selektion17 = replace(Sel_Selektion17, Sel_Selektion17 == "Newsletter-Abo", "begrüßen"))
}

#' @param data ein Ausgangsdatensatz.
#' @param a erste Variable
#' @return Erzeugen der Variable Mitgl_digitale_Zeotschrift aus \code{a}.
#' @import dplyr
#' @export
zeitschrift <- function(data, a) {
  mutate(data, Mitgl_digitale_Zeitschrift = ifelse(grepl("Mail", a), 1, 0))
}
