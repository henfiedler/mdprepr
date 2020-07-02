#' Erzeugen der Variable Adr_e.mail
#'
#' @param data der Ausgangsdatensatz
#' @param a erste Variable
#' @return Die Variable \code{a} wird zu Adr_e.Mail umbenannt und alle Google-Mail-Adressen werden zu gmail.com standardisiert
#' @import dplyr
#' @export
gmail <- function(data, a) {
  rename(data, Adr_e.mail = all_of(a)) %>%
  mutate(Adr_e.mail = str_replace_all(Adr_e.mail, "\\s", "")) %>%
    mutate(Adr_e.mail = str_to_lower(Adr_e.mail)) %>%
    mutate(Adr_e.mail = str_replace_all(Adr_e.mail, "@googlemail.com", "@gmail.com"))
}
