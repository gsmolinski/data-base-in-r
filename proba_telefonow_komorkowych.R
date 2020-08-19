proba_telefonow_komorkowych <- function(wielkosc_proby = 20000) {
  message("Wybierz plik z prefixami: .txt, .csv (bez nagłówka!).")
  prefixy <- read.table(file = file.choose(), header = FALSE, sep = ";")
  prefixy <- prefixy$V1
  if (!is.numeric(prefixy)) {
    stop("W wybranym pliku z prefixami mogą być tylko liczby!")
  }
  options(scipen = 999)
  max_numer_telefonu = 999999999
  dlugosc_numeru_telefonu = max_numer_telefonu + 1
  wszystkie_numery <- unlist(sapply(prefixy, function(prefixy) seq(from = prefixy * (as.numeric(substr(dlugosc_numeru_telefonu, 1, nchar(dlugosc_numeru_telefonu) - nchar(prefixy)))), to = prefixy * (as.numeric(substr(dlugosc_numeru_telefonu, 1, nchar(dlugosc_numeru_telefonu) - nchar(prefixy)))) + (as.numeric((substr(dlugosc_numeru_telefonu, 1, nchar(dlugosc_numeru_telefonu) - nchar(prefixy)))) - 1), by = 1)))
  proba <- sample(wszystkie_numery, wielkosc_proby)
  message("Wybierz plik, do którego mają zostać zapisane wylosowane numery telefonów: .txt, .csv.")
  write.table(x = proba, file = file.choose(), row.names = FALSE, col.names = FALSE)
  message("Gotowe.")
}


#Poniżej ustal wielkość próby
proba_telefonow_komorkowych(wielkosc_proby = 1000)