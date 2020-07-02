# Setup zum Schreiben von Paketen
install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
library(devtools)
has_devel()

# Paket lokal laden
devtools::load_all()

# roxygen Kommentare konvertieren
devtools::document()
