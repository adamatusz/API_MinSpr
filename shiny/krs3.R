suppressPackageStartupMessages({
  library(rvest)
  library(xml2)
  library(readxl)
  library(httr)
  library(jsonlite)
  library(tidyverse)
})

setwd("~/Adam/Marek/shiny/")
# Zakres numerów KRS do sprawdzenia
# krs_start <- 1115426
# krs_end <- 1115430

lista_krs <- read_xlsx("../krs.xlsx") %>% select("nr KRS") %>% rename(nr_KRS = "nr KRS")
lista_krs2 <- read_xlsx("../shiny/Zeszyt11.xlsx") %>% select("KRS") %>% rename(nr_KRS = "KRS")

# Pusta lista na wyniki
wyniki <- list()

# Pętla po numerach KRS
for (krs_num in lista_krs$nr_KRS) {
  
  # Formatowanie numeru do 10 cyfr (z przodu zera)
  # krs_str <- sprintf("%010d", krs_num)
  
  # Budowa URL
  url <- paste0("https://api-krs.ms.gov.pl/api/krs/OdpisAktualny/", 
                krs_num, "?rejestr=S&format=json")
  
  # Wysłanie zapytania
  res <- GET(url)
  
  # Obsługa odpowiedzi – tylko jeśli status 200 (czyli OK)
  if (status_code(res) == 200) {
    data_txt <- content(res, as = "text", encoding = "UTF-8")
    data_json <- fromJSON(data_txt, flatten = TRUE)
    
    # Dodaj do listy wyników
    wyniki[[as.character(krs_num)]] <- data_json
    krs_json_output <- toJSON(wyniki, pretty = TRUE, auto_unbox = TRUE)
    cat(krs_num,"\n")
  } else {
    cat("Brak danych dla KRS:", krs_num, "\n")
  }
}

# finalna_tabela_krs <- wyniki[!sapply(wyniki, is_null)] %>% bind_rows()
# 
# # Przykład wypisania nazw podmiotów
# my_table <- sapply(wyniki, function(x) x$odpis$dzial1$danePodmiotu$nazwa)


str(krs_json_output)
str(data_json)

# data_json$odpis$naglowekA$numerKRS

krs_from_JSON <- jsonlite::fromJSON(krs_json_output)

View(krs_from_JSON)


krs_from_JSON$`0001115260`$odpis$dane$dzial1$danePodmiotu$nazwa
krs_from_JSON$`0001115260`$odpis$dane$dzial1$siedzibaIAdres$adres$ulica
krs_from_JSON$`0001115260`$odpis$dane$dzial1$siedzibaIAdres$adres$nrDomu
krs_from_JSON$`0001115260`$odpis$dane$dzial1$siedzibaIAdres$adres$nrLokalu
krs_from_JSON$`0001115260`$odpis$dane$dzial1$siedzibaIAdres$adres$kodPocztowy
krs_from_JSON$`0001115260`$odpis$dane$dzial1$siedzibaIAdres$adres$miejscowosc
krs_from_JSON$`0001115260`$odpis$dane$dzial1$danePodmiotu$czyPosiadaStatusOPP
krs_from_JSON$`0001115260`$odpis$dane$dzial2$reprezentacja$sklad[1, 2]


# Lista, która będzie przechowywać ramki danych dla każdego KRS-u
# (pojedyncze wiersze naszej finalnej tabeli).
lista_wynikow <- list()

# 3. Pętla przez każdy numer KRS
for (krs_numer in lista_krs$nr_KRS) {
  
  # Pobiera pełną strukturę danych dla bieżącego numeru KRS
  dane_dla_tego_krs <- krs_from_JSON[[krs_numer]]
 
  
  # Inicjalizacja zmiennych na wypadek braku danych w ścieżce
  nazwa <- NA
  ulica <- NA
  nr_domu <- NA
  nr_lokalu <- NA
  kod_pocztowy <- NA
  miejscowosc <- NA
  status_opp <- NA
  
  # Bezpieczny dostęp do danych za pomocą kluczy
  # Sprawdzamy istnienie każdego poziomu przed próbą dostępu, aby uniknąć błędów
  if (!is.null(dane_dla_tego_krs$odpis) &&
      !is.null(dane_dla_tego_krs$odpis$dane) &&
      !is.null(dane_dla_tego_krs$odpis$dane$dzial1)) {
    
    dzial1_dane <- dane_dla_tego_krs$odpis$dane$dzial1
    
    # Dane podmiotu
    if (!is.null(dzial1_dane$danePodmiotu)) {
      if (!is.null(dzial1_dane$danePodmiotu$nazwa)) {
        nazwa <- dzial1_dane$danePodmiotu$nazwa
      }
      if (!is.null(dzial1_dane$danePodmiotu$czyPosiadaStatusOPP)) {
        status_opp <- dzial1_dane$danePodmiotu$czyPosiadaStatusOPP
      }
    }
    
    # Dane adresowe
    if (!is.null(dzial1_dane$siedzibaIAdres) && !is.null(dzial1_dane$siedzibaIAdres$adres)) {
      adres_dane <- dzial1_dane$siedzibaIAdres$adres
      if (!is.null(adres_dane$ulica)) {
        ulica <- adres_dane$ulica
      }
      if (!is.null(adres_dane$nrDomu)) {
        nr_domu <- adres_dane$nrDomu
      }
      if (!is.null(adres_dane$nrLokalu)) {
        nr_lokalu <- adres_dane$nrLokalu
      }
      if (!is.null(adres_dane$kodPocztowy)) {
        kod_pocztowy <- adres_dane$kodPocztowy
      }
      if (!is.null(adres_dane$miejscowosc)) {
        miejscowosc <- adres_dane$miejscowosc
      }
    }
  }
  
  # Wiersz danych dla bieżącego KRS-u jako małą ramkę danych
  wiersz_aktualny_krs <- data.frame(
    KRS = krs_numer,
    NazwaPodmiotu = nazwa,
    Ulica = ulica,
    NumerDomu = nr_domu,
    NumerLokalu = nr_lokalu,
    KodPocztowy = kod_pocztowy,
    Miejscowosc = miejscowosc,
    StatusOPP = status_opp,
    stringsAsFactors = FALSE 
  )
  
  # Dodaje wiersz do listy wyników
  lista_wynikow[[krs_numer]] <- wiersz_aktualny_krs
}

# Wiersze z listy w jedną ramkę danych
finalna_tabela_krs <- do.call(rbind, lista_wynikow)

# Usuwa wersze, aby tabela była "czysta"
rownames(finalna_tabela_krs) <- NULL

# Wyświetla wynikową tabelę
print(finalna_tabela_krs)

writexl::write_xlsx(finalna_tabela_krs, path = "finalna_tabela_krs.xlsx")

sumaWszystkichStrachow <- c(
  "TRUE"  = sum(finalna_tabela_krs$StatusOPP == TRUE, na.rm = TRUE),
  "FALSE" = sum(finalna_tabela_krs$StatusOPP == FALSE, na.rm = TRUE),
  "NA"    = sum(is.na(finalna_tabela_krs$StatusOPP))
)
