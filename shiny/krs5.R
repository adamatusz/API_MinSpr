# app.R (z dodaną walidacją nazwy kolumny)

suppressPackageStartupMessages({
  library(shiny)
  library(rvest)
  library(xml2)
  library(readxl)
  library(httr)
  library(jsonlite)
  library(tidyverse)
  library(DT)
  library(writexl)
})

# --- INTERFEJS UŻYTKOWNIKA (UI) ---
ui <- fluidPage(
  tags$head(
    includeCSS("www/custom.css")
  ),
  titlePanel("Dane KRS z API Ministerstwa Sprawiedliwości"),
  
  tabsetPanel(
    tabPanel("Tabela Danych KRS",
             fluidRow(
               column(12,
                      h3("Tabela Danych KRS"),
                      fileInput("uploadKrs", "Wgraj listę KRS (.xlsx)",
                                accept = c(".xlsx")),
                      actionButton("processKrs", "Przetwórz wgrane KRS"), # Przycisk do uruchomienia przetwarzania
                      hr(),
                      downloadButton("downloadData", "Pobierz Tabelę Danych KRS (.xlsx)"), # Przycisk pobierania
                      hr(),
                      DTOutput("krsTable")
               )
             )
    ),
    tabPanel("Podsumowanie Statusu OPP",
             fluidRow(
               column(12,
                      h3("Podsumowanie Statusu Organizacji Pożytku Publicznego"),
                      verbatimTextOutput("oppSummary"),
                      hr(),
                      helpText("Dane pobrane z API KRS Ministerstwa Sprawiedliwości.")
               )
             )
    ),
    tabPanel("Brakujące KRS",
             fluidRow(
               column(12,
                      h3("Numery KRS, dla których nie pobrano danych"),
                      verbatimTextOutput("missingKrs"),
                      hr(),
                      helpText("Lista numerów KRS, dla których API nie zwróciło danych lub wystąpił błąd.")
               )
             )
    )
  )
)

# --- LOGIKA SERWERA (SERVER) ---
server <- function(input, output, session) {
  
  processed_krs_data <- reactiveVal(data.frame(
    KRS = character(0), NazwaPodmiotu = character(0), Ulica = character(0),
    NumerDomu = character(0), NumerLokalu = character(0), KodPocztowy = character(0),
    Miejscowosc = character(0), StatusOPP = logical(0), Reprezentacja = character(0),
    stringsAsFactors = FALSE
  ))
  
  missing_krs_numbers <- reactiveVal(character(0))
  validation_message <- reactiveVal("") # Nowa zmienna reaktywna na komunikat walidacyjny
  
  observeEvent(input$processKrs, {
    # 1. Sprawdź, czy plik został wgrany
    req(input$uploadKrs)
    
    # Resetuj komunikaty i dane przy nowym przetwarzaniu
    validation_message("")
    processed_krs_data(data.frame(
      KRS = character(0), NazwaPodmiotu = character(0), Ulica = character(0),
      NumerDomu = character(0), NumerLokalu = character(0), KodPocztowy = character(0),
      Miejscowosc = character(0), StatusOPP = logical(0), Reprezentacja = character(0),
      stringsAsFactors = FALSE
    ))
    missing_krs_numbers(character(0))
    
    # Wczytaj plik Excela bez selekcji kolumny na tym etapie
    tryCatch({
      uploaded_data <- read_excel(input$uploadKrs$datapath)
      
      # 2. Walidacja: Sprawdź, czy kolumna "nr KRS" istnieje
      if (!"nr KRS" %in% names(uploaded_data)) {
        validation_message("Błąd: Zmień nazwę kolumny w pliku Excel na \"nr KRS\".")
        return(NULL) # Zatrzymaj dalsze wykonywanie funkcji
      }
      
      # Jeśli kolumna istnieje, możesz kontynuować selekcję
      lista_krs <- uploaded_data %>%
        select("nr KRS") %>%
        rename(nr_KRS = "nr KRS")
      
      # Pusta lista na wyniki
      wyniki_api <- list()
      current_missing_krs <- c()
      
      # Pętla po numerach KRS
      withProgress(message = 'Pobieranie danych z API KRS', value = 0, {
        for (i in seq_along(lista_krs$nr_KRS)) {
          krs_num <- lista_krs$nr_KRS[i]
          
          incProgress(1/length(lista_krs$nr_KRS), detail = paste("Pobieram KRS:", krs_num))
          
          url <- paste0("https://api-krs.ms.gov.pl/api/krs/OdpisAktualny/",
                        krs_num, "?rejestr=S&format=json")
          
          res <- GET(url)
          
          # DODANA ZMIANA: Dodatkowe sprawdzenie treści odpowiedzi
          data_txt <- content(res, as = "text", encoding = "UTF-8")
          
          if (status_code(res) == 200 && !is.null(data_txt) && nchar(data_txt) > 2) { # Sprawdź, czy txt nie jest NULL i ma min. 3 znaki (np. "{}")
            # Spróbuj przetworzyć JSON w bloku tryCatch
            tryCatch({
              data_json <- fromJSON(data_txt, flatten = TRUE)
              # Dodatkowe sprawdzenie, czy pobrany JSON zawiera oczekiwaną strukturę
              if (!is.null(data_json$odpis)) {
                wyniki_api[[as.character(krs_num)]] <- data_json
              } else {
                current_missing_krs <- c(current_missing_krs, krs_num)
              }
            }, error = function(e) {
              # Jeśli fromJSON zwróci błąd, dodaj KRS do brakujących
              current_missing_krs <- c(current_missing_krs, krs_num)
              message("Błąd parsowania JSON dla KRS ", krs_num, ": ", e$message) # Do debugowania
            })
          } else {
            current_missing_krs <- c(current_missing_krs, krs_num)
            message("Brak lub nieprawidłowa odpowiedź dla KRS ", krs_num, ". Status: ", status_code(res), " Treść: ", data_txt) # Do debugowania
          }
        }
      })
      
      missing_krs_numbers(current_missing_krs)
      
      # Przetwarzanie danych JSON do finalnej tabeli
      if (length(wyniki_api) > 0) {
        krs_json_output <- toJSON(wyniki_api, pretty = TRUE, auto_unbox = TRUE)
        krs_from_JSON <- fromJSON(krs_json_output)
        
        lista_wynikow <- list()
        
        for (krs_numer_str in names(krs_from_JSON)) {
          dane_dla_tego_krs <- krs_from_JSON[[krs_numer_str]]
          
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
            
            if (!is.null(dane_dla_tego_krs$odpis$dane$dzial1)) {
              dzial1_dane <- dane_dla_tego_krs$odpis$dane$dzial1
              
              if (!is.null(dzial1_dane$danePodmiotu)) {
                if (!is.null(dzial1_dane$danePodmiotu$nazwa)) {
                  nazwa <- dzial1_dane$danePodmiotu$nazwa
                }
                if (!is.null(dzial1_dane$danePodmiotu$czyPosiadaStatusOPP)) {
                  status_opp <- dzial1_dane$danePodmiotu$czyPosiadaStatusOPP
                }
              }
              
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
          
          wiersz_aktualny_krs <- data.frame(
            KRS = krs_numer_str,
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
          lista_wynikow[[krs_numer_str]] <- wiersz_aktualny_krs
        }
        
        final_data <- do.call(rbind, lista_wynikow)
        rownames(final_data) <- NULL
        processed_krs_data(final_data)
        
      } else {
        processed_krs_data(data.frame(
          KRS = character(0), NazwaPodmiotu = character(0), Ulica = character(0),
          NumerDomu = character(0), NumerLokalu = character(0), KodPocztowy = character(0),
          Miejscowosc = character(0), StatusOPP = logical(0), Reprezentacja = character(0),
          stringsAsFactors = FALSE
        ))
      }
    }, error = function(e) {
      # Obsłuż inne możliwe błędy podczas wczytywania pliku (np. uszkodzony Excel)
      validation_message(paste("Wystąpił błąd podczas wczytywania pliku:", e$message))
      return(NULL)
    })
  })
  
  # Renderowanie tabeli
  output$krsTable <- renderDT({
    # Wyświetl komunikat walidacyjny, jeśli istnieje, zamiast tabeli
    validate(
      need(validation_message() == "", {
        # Jeśli jest komunikat walidacyjny, wyświetl go
        div(style = "color: red; font-weight: bold; font-size: 1.2em;", validation_message())
      })
    )
    # Jeśli komunikat pusty, wyświetl tabelę
    datatable(processed_krs_data(), options = list(pageLength = 10))
  })
  
  
  # Renderowanie podsumowania statusu OPP (dodałem walidację)
  output$oppSummary <- renderPrint({
    validate(
      need(validation_message() == "", {
        div(style = "color: red; font-weight: bold; font-size: 1.2em;", validation_message())
      })
    )
    data <- processed_krs_data()
    if (nrow(data) > 0) {
      sumaWszystkichStrachow <- c(
        "TRUE"  = sum(data$StatusOPP == TRUE, na.rm = TRUE),
        "FALSE" = sum(data$StatusOPP == FALSE, na.rm = TRUE),
        "NA"    = sum(is.na(data$StatusOPP))
      )
      print(sumaWszystkichStrachow)
    } else {
      cat("Brak danych do podsumowania. Wgraj listę KRS i przetwórz dane.\n")
    }
  })
  
  # Renderowanie listy brakujących numerów KRS (dodałem walidację)
  output$missingKrs <- renderPrint({
    validate(
      need(validation_message() == "", {
        div(style = "color: red; font-weight: bold; font-size: 1.2em;", validation_message())
      })
    )
    missing_numbers <- missing_krs_numbers()
    if (length(missing_numbers) > 0) {
      cat("Nie udało się pobrać danych dla następujących numerów KRS:\n")
      print(missing_numbers)
    } else {
      cat("Brak brakujących danych KRS. Wszystkie dane zostały pobrane poprawnie.\n")
    }
  })
  
  # Obsługa przycisku pobierania
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("dane_krs-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(processed_krs_data(), path = file)
    }
  )
  
}

# Uruchomienie aplikacji Shiny
shinyApp(ui = ui, server = server)