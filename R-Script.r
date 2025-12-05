# Bibliotheken-Sammlung
library(tidyverse)
library(readxl)
library(dplyr)
# 1. Kombinieren Sie die Tabellen der beiden Jahre, so dass die Jahreszahl für jeden Datensatz ebenfalls ersichtlich ist.


file <- "su-d-01.02.03.06.xlsx"

# Sheet-Namen
sheets <- excel_sheets(file)

# Erste Tabelle
tabelle1 <- read_excel(file, sheet = sheets[1]) %>%
  mutate(Jahr = sheets[1])

# Zweite Tabelle
tabelle2 <- read_excel(file, sheet = sheets[2]) %>%
  mutate(Jahr = sheets[2])

tabellen_beide <- bind_rows(tabelle1, tabelle2)

View(tabellen_beide)


# 2. Erzeugen Sie je einen Sekundärindex für die Kantone und einen für die Bezirke, so dass für jeden Gemeindendatensatz auch der Kantons- und Bezirksname ersichtlich ist.

# 3. Organisieren Sie die Wohnbevölkerung nach den folgenden Altersgruppen: 
# Kinder (Bis 12 Jahre)
# Minderjährige (unter 18 Jährige)
# Erwachsene, jünger als 65 Jahre
# Erwachsene, ab 65 Jahren

# 4. Bestimmen Sie die statistischen Kennwerte für die drei Altersgruppen über alle Datensätze der Jahre 2010 und 2022 der Originalquelle und speichern Sie Ihre Ergebnisse als Tabelle.

# 5. Ermitteln Sie den Unterschied des Durchschnittsalters dieser Gruppen in den beiden Referenzjahren für alle Bezirke im Kanton Zürich und speichern Sie diese Unterschiede als separate Tabelle.

# 6. Erstellen Sie einen Boxplot, aus dem die Verteilungen des Durchschnittsalters der Minderjährigen in den Gemeinden der 11 Bezirke des Kanton Zürichs ohne die Stadt Zürich in den beiden Jahren hervorgehen.
