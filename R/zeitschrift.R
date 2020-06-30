#' Erzeugen der Variable Mitgl_digitale_Zeitschrift
#'
#' @param data der Ausgangsdatensatz
#' @param a erste Variable
#' @return Erzeugen der Variable Mitgl_digitale_Zeitschrift aus \code{a}.
#' @import dplyr
#' @export
zeitschrift <- function(data, a) {
  mutate(data, Mitgl_digitale_Zeitschrift = ifelse(grepl("Mail", a), 1, 0))
}
