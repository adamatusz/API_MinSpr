suppressPackageStartupMessages({
  library(xml2)
  library(readxl)
  library(httr)
  library(jsonlite)
  library(tidyverse)
})

dane <- read_json("c:/Users/Adam MAtuszczyk/Downloads/2025-05-05.json")



df <- as.data.frame(t(as.data.frame(dane)))
rownames(df) <- NULL

df %>%
  summarise(n())


dane2 <- spec_csv("C:/Users/Adam MAtuszczyk/Documents/Adam/Marek/testKRS.txt")


typy <- map_chr(dane2$cols, ~ class(.x)[1])
table(typy)
