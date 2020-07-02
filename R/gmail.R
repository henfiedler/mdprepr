#' Erzeugen der Variable gmail
#'
#' @param data der Ausgangsdatensatz
#' @param a erste Variable
#' @return Alle Google-Mail Adressen in der Variable \code{a} werden zu gmail.com standardisiert
#' @import dplyr
#' @export
gmail <- function(data, a) {
  mutate(data, Adr_e.mail = str_replace_all(a, "\\s", "")) %>%
    mutate(a = str_to_lower(a)) %>%
    mutate(a = str_replace_all(a, "@googlemail.com", "@gmail.com"))
}
