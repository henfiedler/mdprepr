#' Befehle für FileMaker-kompatible Aufbereitung
#'
#' @param data ein Ausgangsdatensatz.
#' @param a eine Variable
#' @param b eine Variable
#' @return Erzeugen neuer Variablen in \code{data} aus \code{a}, \code{b}, etc.
bemerkung <- function(data, a, b) {
  mutate(data, Sel_Bemerkung = paste0("Unterlagen: ",
                                      ifelse(grepl("Mail", a), "Mail", "Post"),
                                      ifelse(b != "",
                                             paste0(", Bem: ", b), "")))
}

selektion17 <- function(data, a) {
  rename(data, Sel_Selektion17 = all_of(a)) %>% # in den anderen Datensätzen "Weitere.Informationen"
    mutate(Sel_Selektion17 = replace(Sel_Selektion17, Sel_Selektion17 == "keins", "")) %>%
    mutate(Sel_Selektion17 = replace(Sel_Selektion17, Sel_Selektion17 == "Newsletter-Abo", "begrüßen"))
}

zeitschrift <- function(data, a) {
  mutate(data, Mitgl_digitale_Zeitschrift = ifelse(grepl("Mail", a), 1, 0))
}
