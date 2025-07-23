# app.R (zmodyfikowany dla zakładek)

suppressPackageStartupMessages({
  library(shiny)
  library(rvest)
  library(xml2)
  library(readxl)
  library(httr)
  library(jsonlite)
  library(tidyverse)
  library(DT) # Do interaktywnych tabel
  library(writexl) # Możliwość zapisu
})

# --- PRZETWARZANIE DANYCH (kod krs3.R i poprzednich poprawek) ---
# Ten blok kodu zostanie wykonany tylko raz przy starcie aplikacji
# i wczytaniu danych do środowiska Shiny.

# Ustawienie katalogu roboczego - ważne dla wczytywania krs.xlsx
# lista_krs <- read_xlsx("krs.xlsx") %>% select("nr KRS") %>% rename(nr_KRS = "nr KRS")
lista_krs <- read_xlsx("Zeszyt11.xlsx") %>% select("KRS") %>% rename(nr_KRS = "KRS")


# Pusta lista na wyniki
wyniki <- list()

# Wektor do przechowywania numerów KRS, dla których nie pobrano danych
brakujace_krs_numery <- c()

# Pętla po numerach KRS
for (krs_num in lista_krs$nr_KRS) {
  
  url <- paste0("https://api-krs.ms.gov.pl/api/krs/OdpisAktualny/",
                krs_num, "?rejestr=S&format=json")
  
  res <- GET(url)
  
  if (status_code(res) == 200) {
    data_txt <- content(res, as = "text", encoding = "UTF-8")
    data_json <- fromJSON(data_txt, flatten = TRUE)
    wyniki[[as.character(krs_num)]] <- data_json
  } else {
    # Logowanie błędów tutaj, np. message("Brak danych dla KRS:", krs_num)
    brakujace_krs_numery <- c(brakujace_krs_numery, krs_num)
  }
}

krs_json_output <- toJSON(wyniki, pretty = TRUE, auto_unbox = TRUE)
krs_from_JSON <- jsonlite::fromJSON(krs_json_output)

# Lista, która będzie przechowywać ramki danych dla każdego KRS-u
lista_wynikow <- list()

for (krs_numer in lista_krs$nr_KRS) {
  
  dane_dla_tego_krs <- krs_from_JSON[[as.character(krs_numer)]]
  
  nazwa <- NA
  ulica <- NA
  nr_domu <- NA
  nr_lokalu <- NA
  kod_pocztowy <- NA
  miejscowosc <- NA
  status_opp <- NA
  reprezentacja <- NA
  
  if (!is.null(dane_dla_tego_krs$odpis) &&
      !is.null(dane_dla_tego_krs$odpis$dane)) {
    
    # Dział 1 - Dane Podmiotu i Adres
    if (!is.null(dane_dla_tego_krs$odpis$dane$dzial1)) {
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
    
    # Dział 2 - Reprezentacja
    if (!is.null(dane_dla_tego_krs$odpis$dane$dzial2) &&
        !is.null(dane_dla_tego_krs$odpis$dane$dzial2$reprezentacja) &&
        !is.null(dane_dla_tego_krs$odpis$dane$dzial2$reprezentacja$sklad)) {
      
      if (nrow(dane_dla_tego_krs$odpis$dane$dzial2$reprezentacja$sklad) >= 1 &&
          ncol(dane_dla_tego_krs$odpis$dane$dzial2$reprezentacja$sklad) >= 2) {
        reprezentacja <- dane_dla_tego_krs$odpis$dane$dzial2$reprezentacja$sklad[1, 2]
      } else {
        reprezentacja <- NA
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
    Reprezentacja = reprezentacja,
    stringsAsFactors = FALSE
  )
  
  # Dodaje wiersz do listy wyników
  lista_wynikow[[as.character(krs_numer)]] <- wiersz_aktualny_krs
}

finalna_tabela_krs <- do.call(rbind, lista_wynikow)
rownames(finalna_tabela_krs) <- NULL

sumaWszystkichStrachow <- c(
  "TRUE"  = sum(finalna_tabela_krs$StatusOPP == TRUE, na.rm = TRUE),
  "FALSE" = sum(finalna_tabela_krs$StatusOPP == FALSE, na.rm = TRUE),
  "NA"    = sum(is.na(finalna_tabela_krs$StatusOPP))
)


# --- INTERFEJS UŻYTKOWNIKA (UI) ---
ui <- fluidPage(
  titlePanel("Dane KRS z API Ministerstwa Sprawiedliwości"),
  
  # SidebarLayout na tabsetPanel
  tabsetPanel(
    tabPanel("Tabela Danych KRS", # Pierwsza zakładka
             fluidRow(
               column(12,
                      h3("Tabela Danych KRS"),
                      DTOutput("krsTable") # Tabela danych
               )
             )
    ),
    tabPanel("Podsumowanie Statusu OPP", # Druga zakładka
             fluidRow(
               column(12,
                      h3("Podsumowanie Statusu Organizacji Pożytku Publicznego"),
                      verbatimTextOutput("oppSummary"), # Sumy statusu OPP
                      hr(),
                      helpText("Dane pobrane z API KRS Ministerstwa Sprawiedliwości.")
               )
             )
    ),
    tabPanel("Brakujące KRS", # Nowa zakładka
             fluidRow(
               column(12,
                      h3("Numery KRS, dla których nie pobrano danych"),
                      verbatimTextOutput("missingKrs"), # Wyjście dla brakujących numerów KRS
                      hr(),
                      helpText("Lista numerów KRS, dla których API nie zwróciło danych lub wystąpił błąd.")
               )
             )
    )
  )
)

# --- LOGIKA SERWERA (SERVER) ---
server <- function(input, output, session) {
  
  # Renderowanie tabeli
  output$krsTable <- renderDT({
    datatable(finalna_tabela_krs, options = list(pageLength = 10))
  })
  
  # Renderowanie podsumowania statusu OPP
  output$oppSummary <- renderPrint({
    print(sumaWszystkichStrachow)
  })
  # Renderowanie listy brakujących numerów KRS
  output$missingKrs <- renderPrint({
    if (length(brakujace_krs_numery) > 0) {
      cat("Nie udało się pobrać danych dla następujących numerów KRS:\n")
      print(brakujace_krs_numery)
    } else {
      cat("Brak brakujących danych KRS. Wszystkie dane zostały pobrane poprawnie.\n")
    }
  })
  }

# Uruchomienie aplikacji Shiny
shinyApp(ui = ui, server = server)