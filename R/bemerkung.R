#' Erzeugen der Variable Sel_Bemerkung
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
