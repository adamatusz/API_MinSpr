# Projekt to aplikacja Shiny służąca do pobierania i przetwarzania danych z Krajowego Rejestru Sądowego (KRS) z wykorzystaniem oficjalnego API Ministerstwa Sprawiedliwości. Aplikacja umożliwia użytkownikowi wgranie listy numerów KRS w pliku Excel, pobranie aktualnych odpisów w formacie JSON, przetworzenie ich do tabeli oraz zapis wyników do pliku **`.xlsx`**.​

Aplikacja działa jako aplikacja webowa na platformie shinyapps.io: [**`https://adamatusz.shinyapps.io/MS_KRS/`**](https://adamatusz.shinyapps.io/MS_KRS/) oraz ma kod źródłowy udostępniony w repozytorium GitHub: [**`https://github.com/adamatusz/API_MinSpr`**](https://github.com/adamatusz/API_MinSpr).​

## Funkcjonalności

-   Wgrywanie listy numerów KRS z pliku **`.xlsx`** poprzez komponent **`fileInput`** w interfejsie Shiny.​

-   Automatyczne pobieranie danych z API [**`https://api-krs.ms.gov.pl/api/krs/OdpisAktualny/<KRS`**](https://api-krs.ms.gov.pl/api/krs/OdpisAktualny/%3CKRS)**`?rejestr=...&format=json`** (skrótowo) dla każdego numeru KRS z listy, z obsługą błędów HTTP.​

-   Przetwarzanie zagnieżdżonego JSON-a do płaskiej ramki danych z najważniejszymi polami: numer KRS, nazwa podmiotu, adres (ulica, numer domu, lokalu, kod pocztowy, miejscowość), informacja o statusie OPP oraz skrócony opis reprezentacji.​

-   Wyświetlanie wynikowej tabeli w interfejsie jako tabeli interaktywnej (**`DT::datatable`**) z możliwością sortowania i filtrowania.​

-   Generowanie podsumowania liczby podmiotów ze statusem OPP (organizacja pożytku publicznego) na podstawie przetworzonych danych.​

-   Wyświetlanie listy numerów KRS, dla których nie udało się pobrać danych z API (np. błąd, brak wpisu).​

-   Eksport przetworzonych danych do pliku **`.xlsx`** przy pomocy **`writexl::write_xlsx`** oraz przycisku **`downloadButton`** w interfejsie.​

## Wykorzystane technologie i pakiety

Projekt jest zbudowany w języku **R** z użyciem frameworka Shiny i kilku dodatkowych pakietów pomocniczych.​

-   **`shiny`** – tworzenie interfejsu użytkownika (UI) oraz logiki serwera (server) dla aplikacji webowej.​

-   **`httr`** – wysyłanie zapytań HTTP **`GET`** do API KRS oraz obsługa kodów statusu odpowiedzi.​

-   **`jsonlite`** – parsowanie odpowiedzi w formacie JSON do struktur R (**`fromJSON`**, **`toJSON`**).​

-   **`readxl`** – wczytywanie pliku Excel z listą numerów KRS przekazanego przez użytkownika.​

-   **`writexl`** – zapis wynikowej ramki danych do pliku **`.xlsx`** udostępnianego do pobrania.​

-   **`tidyverse`** – operacje na ramkach danych oraz wygodniejsze przetwarzanie danych (dplyr/tidyr itp.).​

-   **`DT`** – prezentacja danych tabelarycznych w formie interaktywnej tabeli w przeglądarce.​

Dodatkowo wczytywany jest plik CSS (**`www/custom.css`**) w sekcji **`tags$head(includeCSS("www/custom.css"))`**, co pozwala na dopasowanie wyglądu interfejsu do własnych potrzeb.​

## Struktura aplikacji

Aplikacja jest jednoplikowym projektem Shiny, który składa się z definicji UI i serwera oraz wywołania **`shinyApp(ui, server)`** na końcu pliku.​

## Interfejs użytkownika (UI)

Interfejs jest zbudowany z wykorzystaniem **`fluidPage()`** oraz **`tabsetPanel()`** z trzema zakładkami.​

-   Zakładka „Tabela Danych KRS”:

    -   **`fileInput("uploadKrs", "Wgraj listę KRS (.xlsx)")`** – pole do wgrywania pliku Excel.​

    -   **`actionButton("processKrs", "Przetwórz wgrane KRS")`** – przycisk uruchamiający pobieranie danych z API i ich przetwarzanie.​

    -   **`downloadButton("downloadData", "Pobierz Tabelę Danych KRS (.xlsx)")`** – przycisk do pobrania przetworzonej tabeli.​

    -   **`DTOutput("krsTable")`** – miejsce na interaktywną tabelę wynikową.​

-   Zakładka „Podsumowanie Statusu OPP”:

    -   **`verbatimTextOutput("oppSummary")`** – tekstowe podsumowanie liczby podmiotów z i bez statusu OPP.​

-   Zakładka „Brakujące KRS”:

    -   **`verbatimTextOutput("missingKrs")`** – lista numerów KRS, dla których nie uzyskano danych z API.​

W stopce zakładek umieszczony jest **`helpText("Dane pobrane z API KRS Ministerstwa Sprawiedliwości.")`**, co podkreśla źródło danych.​

## Logika serwera

Logika serwera jest zdefiniowana jako funkcja **`server(input, output, session)`** i opiera się na obiektach reaktywnych.​

-   **`processedKrsData <- reactiveVal(data.frame(...))`** – reaktywna ramka danych, w której przechowywane są przetworzone rekordy KRS.​

-   **`missingKrsNumbers <- reactiveVal(character(0))`** – reaktywna lista numerów KRS, dla których nie udało się pobrać danych.​

Kluczowy fragment stanowi **`observeEvent(input$processKrs, { ... })`**, który:

-   Odczytuje wgrany plik Excel za pomocą **`readxl::read_excel`** i pobiera kolumnę z numerami KRS.​

-   Iteruje po numerach KRS w pętli **`for`**, wywołując **`httr::GET`** na API, a następnie w razie powodzenia przetwarzając JSON do postaci płaskiej ramki danych.​

-   Wyciąga zagnieżdżone elementy JSON, takie jak **`dane.odpis.dane.dzial1.danePodmiotu`** czy **`dane.odpis.dane.dzial1.siedzibaIAdres.adres`**, a także fragment reprezentacji z **`dane.odpis.dane.dzial2.reprezentacja.sklad`** (jeśli dostępny).​

-   Każdy rekord dla pojedynczego numeru KRS jest zamieniany na wiersz ramki danych i dodawany do listy, która na koniec jest łączona funkcją **`do.call(rbind, ...)`**.​

-   Aktualizuje **`processedKrsData(finalData)`** oraz listę brakujących numerów **`missingKrsNumbers(currentMissingKrs)`**.​

Wynik jest następnie wykorzystywany w:

-   **`output$krsTable <- renderDT(...)`** – renderowanie tabeli z danymi.​

-   **`output$oppSummary <- renderPrint(...)`** – obliczanie sumy rekordów ze statusem OPP TRUE/FALSE/NA i wyświetlanie podsumowania.​

-   **`output$missingKrs <- renderPrint(...)`** – wyświetlanie komunikatu o tym, dla których numerów KRS nie pobrano danych.​

-   **`output$downloadData <- downloadHandler(...)`** – przygotowanie pliku **`.xlsx`** do pobrania, z wykorzystaniem bieżącej zawartości **`processedKrsData()`**.​

## Sposób uruchomienia i wymagania

Do uruchomienia aplikacji lokalnie potrzebne są:

-   Zainstalowany R (zalecana wersja nowsza niż 4.x).

-   Środowisko RStudio (opcjonalnie, ale wygodniejsze dla pracy z Shiny).

-   Zainstalowane pakiety: **`shiny`**, **`httr`**, **`jsonlite`**, **`readxl`**, **`writexl`**, **`tidyverse`**, **`DT`** oraz ewentualne pakiety pomocnicze wskazane na początku pliku (np. **`rvest`**, **`xml2`**).​

Instrukcja uruchomienia:

1.  Sklonować repozytorium **`API_MinSpr`** z GitHuba lub pobrać plik **`krs4.R`** do lokalnego projektu w RStudio.​​

2.  Upewnić się, że w katalogu projektu znajduje się folder **`www/`** z plikiem **`custom.css`**, jeśli chcemy zachować zdefiniowaną stylizację.​

3.  W RStudio otworzyć plik z aplikacją i uruchomić go za pomocą przycisku „Run App” lub komendy **`shiny::runApp()`** w konsoli.​

4.  W interfejsie przeglądarki wgrać plik **`.xlsx`** z kolumną zawierającą numery KRS, kliknąć „Przetwórz wgrane KRS”, poczekać na zakończenie pobierania i przetwarzania danych, a następnie przejrzeć wyniki, podsumowanie OPP i listę brakujących numerów oraz ewentualnie pobrać dane jako plik Excel.​
