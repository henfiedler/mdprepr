bemerkung <- function(data, a, b)
{ # erzeuge eine neue Variable "Sel_Bemerkung"
  mutate(data, Sel_Bemerkung = paste0("Unterlagen: ",
                                      ifelse(grepl("Mail", a), "Mail", "Post"),
                                      ifelse(b != "",
                                             paste0(", Bem: ", b), "")))
}

selektion17 <- function(data, a)
{rename(data, Sel_Selektion17 = all_of(a)) %>% # in den anderen Datensätzen "Weitere.Informationen"
    mutate(Sel_Selektion17 = replace(Sel_Selektion17, Sel_Selektion17 == "keins", "")) %>%
    mutate(Sel_Selektion17 = replace(Sel_Selektion17, Sel_Selektion17 == "Newsletter-Abo", "begrüßen"))
}

zeitschrift <- function(data, a)
{mutate(data, Mitgl_digitale_Zeitschrift = ifelse(grepl("Mail", a), 1, 0))
}
