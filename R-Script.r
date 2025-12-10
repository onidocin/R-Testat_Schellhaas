# Bibliotheken-Sammlung
library("tidyverse")
library("readxl")
library("dplyr")
# 1. Kombinieren Sie die Tabellen der beiden Jahre, so dass die Jahreszahl für jeden Datensatz ebenfalls ersichtlich ist.


file <- "su-d-01.02.03.06.xlsx"

# Sheet-Namen
sheets <- excel_sheets(file)

# Erste Tabelle
tabelle1 <- read_excel(file, sheet = "2022", skip = 1) %>%
  mutate(Jahr = "2022")

# Zweite Tabelle
tabelle2 <- read_excel(file, sheet = "2010") %>%
  mutate(Jahr = "2010")


tabellen_beide <- bind_rows(tabelle1, tabelle2)

View(tabellen_beide)


# 2. Erzeugen Sie je einen Sekundärindex für die Kantone und einen für die Bezirke, so dass für jeden Gemeindendatensatz auch der Kantons- und Bezirksname ersichtlich ist.
tibble(
  Jahr = c(
    "2022",
    "2010"
  )
) |>
  group_by(Jahr) |>
  mutate(
    daten = read_excel("su-d-01.02.03.06.xlsx", skip = 1, sheet = Jahr) |> list()
  ) |>
  unnest(daten) |>
  filter(Region != "Schweiz") |>
  rename(`100` = "100 und mehr") 


# 3. Organisieren Sie die Wohnbevölkerung nach den folgenden Altersgruppen: 
# Kinder (Bis 12 Jahre)
# Minderjährige (unter 18 Jährige)
# Erwachsene, jünger als 65 Jahre
# Erwachsene, ab 65 Jahren
read_excel("su-d-01.02.03.06.xlsx") |>
  mutate(
    Kinder = column_to_rownames()
  ) |> View("su-d-01.02.03.06.xlsx")
# 4. Bestimmen Sie die statistischen Kennwerte für die drei Altersgruppen über alle Datensätze der Jahre 2010 und 2022 der Originalquelle und speichern Sie Ihre Ergebnisse als Tabelle.
#Achtung: 

# 5. Ermitteln Sie den Unterschied des Durchschnittsalters dieser Gruppen in den beiden Referenzjahren für alle Bezirke im Kanton Zürich und speichern Sie diese Unterschiede als separate Tabelle.
write.csv2()
# 6. Erstellen Sie einen Boxplot, aus dem die Verteilungen des Durchschnittsalters der Minderjährigen in den Gemeinden der 11 Bezirke des Kanton Zürichs ohne die Stadt Zürich in den beiden Jahren hervorgehen.
# Daten Visualisieren