# Benötigte Pakete
install.packages("janitor")
library(tidyverse)
library(readxl)
library(janitor)
library(dplyr)
library(stringr)

# ---- Benutzeranpassung: Dateipfade und sheetnamen ----
file <- "su-d-01.02.03.06.xlsx"
sheet_2010 <- "2022"
sheet_2022 <- "2010"

# ---- Hilfsdaten: Liste deutscher Kantonsnamen (für Erkennung) ----
kantone_de <- c("Zürich","Bern","Luzern","Uri","Schwyz","Obwalden","Nidwalden",
                "Glarus","Zug","Fribourg","Solothurn","Basel-Stadt","Basel-Landschaft",
                "Schaffhausen","Appenzell Ausserrhoden","Appenzell Innerrhoden",
                "St. Gallen","Graubünden","Aargau","Thurgau","Ticino","Vaud",
                "Valais","Neuchâtel","Genève","Jura")
# Some names might use different spellings (e.g., "Wallis" instead of "Valais") - adapt if needed.

# ---- Funktion zum Einlesen + Umwandeln eines Jahresfiles ----
read_population_year <- function(file, sheet, year) {
  raw <- read_excel(file, sheet = sheet)
  
  # Region/Gemeinde
  region <- raw[[1]][-1]  # erste Zeile entfernen (Überschrift)
  
  # Alters-Spalten (alles außer Spalte 1 und 2)
  age_data <- raw[-1, 3:ncol(raw)]
  
  # Spaltennamen für pivot_longer (alles in character)
  names(age_data) <- paste0("age_", seq_len(ncol(age_data)))
  age_data <- age_data %>% mutate(across(everything(), as.character))
  
  # Pivot
  pop_long <- age_data %>%
    mutate(region = region) %>%
    pivot_longer(
      cols = -region,
      names_to = "temp",
      values_to = "population"
    ) %>%
    mutate(
      age = parse_number(temp),               # extrahiert Zahl aus "age_0", "age_100+"
      age = if_else(str_detect(temp, "\\+"), 100, age),
      population = as.numeric(population),
      year = year
    ) %>%
    select(region, year, age, population)
  
  return(pop_long)
}



# ---- Einlesen beider Jahre und Kombinieren ----
pop2010_long <- read_population_year(file, sheet_2010, 2010)
pop2022_long <- read_population_year(file, sheet_2022, 2022)

head(pop2010_long)
head(pop2022_long)

pop_all <- bind_rows(pop2010_long, pop2022_long)

# ---- Jetzt: Kantons- und Bezirkszuweisung für jede Gemeinde ----
# Idee: wir identifizieren Zeilen, die ganz sicher Kantone sind (nach kantone_de), füllen diese nach unten.
# Für Bezirke verwenden wir die leading_spaces- bzw. level_guess Heuristik:
pop_all <- pop_all %>%
  mutate(region_trim = str_trim(region))

# mark canton rows explicitly where name matches canton list
pop_all <- pop_all %>%
  mutate(kanton_row = if_else(region_trim %in% kantone_de, region_trim, NA_character_))

pop_all <- pop_all %>%
  mutate(
    kanton_row = if_else(str_detect(region_trim, paste(kantone_de, collapse="|")), region_trim, NA_character_)
  )

pop_all <- pop_all %>%
  tidyr::fill(kanton_row, .direction = "down") %>%
  rename(kanton = kanton_row)


# For districts: detect rows where level_guess == "bezirk" OR where the row name contains keywords "Bezirk" etc.


pop_all <- pop_all %>%
  mutate(
    district_row = if_else(
      str_detect(region_trim, regex("Bezirk|Kreis|District", ignore_case = TRUE)),
      region_trim,
      NA_character_
    )
  ) %>%
  group_by(year, kanton) %>%
  tidyr::fill(district_row, .direction = "down") %>%
  ungroup() %>%
  rename(bezirk = district_row)


# Fallback: Wenn bezirk NA, setze bezirk = kanton (z.B. wenn keine Bezirke vorhanden)
pop_all <- pop_all %>% mutate(bezirk = if_else(is.na(bezirk), kanton, bezirk))

# Nun haben wir für jede Zeile: gebiet (roh), gebiet_trim, kanton, bezirk, alter_num, einwohner, jahr

# ---- Altersgruppen definieren (nach Aufgabenstellung) ----
# Gefordert waren:
# - Kinder (bis 12 Jahre)   : <= 12
# - Minderjährige (unter 18): < 18
# - Erwachsene < 65         : 18-64 (oder >=18 & <65)
# - Erwachsene >= 65        : >= 65
#
# Hinweis: Gruppen überlappen (Kinder ⊂ Minderjährige). Später verlangt Aufgabe 5 "drei Altersgruppen" -> ich nehme an,
# es sind die drei disjunkten Gruppen: Kinder (0-12), Erwachsene 18-64, Senioren 65+. "Minderjährige" wird zusätzlich ausgewiesen.
# Ich berechne sowohl die disjunkten Summen als auch die Minderjährigen als eigene (überlappende) Kategorie.

pop_all <- pop_all %>%
  mutate(
    gruppe_disj = case_when(
      age <= 12 ~ "Kinder_0_12",
      age >= 65 ~ "Erwachsene_65_plus",
      age >= 18 & age < 65 ~ "Erwachsene_18_64",
      TRUE ~ "Sonstige"  # z.B. 13-17
    ),
    minderjaehrig = if_else(age < 18, "Minderjährige", NA_character_)
  )


# ---- Aggregation: Einwohner pro Gemeinde x Jahr x Altersgruppe (sowohl disjunkte Gruppen als auch Minderjährige) ----
# Wir wollen pro Gemeindename (letztlich die tatsächliche Gemeindezeile) die Summen bilden.
# Erkennen einer Gemeindezeile: level_guess == "gemeinde" oder fallback: gebiet ist nicht kanton und nicht bezirk
# Für Robustheit: wir aggregieren nach kombinierter (kanton, bezirk, gebiet_trim) — das gibt Gemeinde-Ebene.

age_groups <- pop_all %>%
  group_by(year, kanton, bezirk, gemeinde = region_trim, age) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
  # jetzt gruppensummen
  mutate(
    gruppe_disj = case_when(
      age <= 12 ~ "Kinder_0_12",
      age >= 65 ~ "Erwachsene_65_plus",
      age >= 18 & age < 65 ~ "Erwachsene_18_64",
      TRUE ~ paste0("Alter_", age)
    ),
    minderjaehrig_flag = if_else(age < 18, 1L, 0L)
  ) %>%
  group_by(year, kanton, bezirk, gemeinde, gruppe_disj) %>%
  summarise(einwohner_gruppe = sum(population, na.rm = TRUE), .groups = "drop")

# Zusätzlich: Minderjährige (alle unter 18) pro Gemeinde
minderjaehrige_sum <- pop_all %>%
  filter(age < 18) %>%
  group_by(year, kanton, bezirk, gemeinde = region_trim) %>%
  summarise(minderjaehrige = sum(population, na.rm = TRUE), .groups = "drop")

# Merge in eine breite Form (Breitformat mit Gruppen als Spalten) für weitere Berechnungen
wide_groups <- age_groups %>%
  pivot_wider(names_from = gruppe_disj, values_from = einwohner_gruppe, values_fill = 0) %>%
  left_join(minderjaehrige_sum, by = c("year","kanton","bezirk","gemeinde"))

# ---- 5) Statistische Kennwerte für die drei Altersgruppen über alle Datensätze 2010 & 2022 ----
# Hier nehme ich (wie oben erklärt) die drei disjunkten Gruppen:
# Kinder_0_12, Erwachsene_18_64, Erwachsene_65_plus
stat_groups <- wide_groups %>%
  select(year, kanton, bezirk, gemeinde, Kinder_0_12, Erwachsene_18_64, Erwachsene_65_plus) %>%
  pivot_longer(cols = c(Kinder_0_12, Erwachsene_18_64, Erwachsene_65_plus),
               names_to = "altersgruppe", values_to = "anzahl") %>%
  group_by(altersgruppe) %>%
  summarise(
    n = sum(!is.na(anzahl)),
    mean = mean(anzahl, na.rm = TRUE),
    sd = sd(anzahl, na.rm = TRUE),
    median = median(anzahl, na.rm = TRUE),
    min = min(anzahl, na.rm = TRUE),
    max = max(anzahl, na.rm = TRUE),
    .groups = "drop"
  )

# Speichere Ergebnis als CSV
write_csv(stat_groups, "statistische_kennwerte_altersgruppen_2010_2022.csv")

# ---- 6) Unterschied des Durchschnittsalters dieser Gruppen in den beiden Referenzjahren für alle Bezirke im Kanton Zürich ----
# Wir müssen "Durchschnittsalter dieser Gruppen" definieren: ich interpretiere das als 
# "für jede Gruppe (z.B. Kinder_0_12) berechne das durchschnittliche Alter innerhalb der Gruppe in einem Bezirk" 
# (also gewichteter Mittelwert der Altersjahre, gewichtet mit Einwohnerzahl).
#
# Vorgehen:
# - Für jede Bezirk x Jahr x Altersgruppe: compute weighted mean age = sum(age * persons_in_age)/sum(persons_in_age)
# - Dann Unterschied 2022 - 2010

# Bezirke und Kantone definieren
pop_all <- pop_all %>%
  mutate(
    kanton_row = if_else(str_starts(region_trim, "- "),
                         str_remove(region_trim, "^-\\s*"),
                         NA_character_)
  ) %>%
  fill(kanton_row, .direction = "down") %>%
  rename(kanton = kanton_row)
unique(pop_all$kanton)


# Erst: berechne Einwohner pro Alter (alter_num) aggregiert auf Bezirk-Ebene (kanton == "Zürich")
zuerich_age_dist <- pop_all %>%
  filter(kanton == "Zürich") %>%
  group_by(year, kanton, bezirk, age) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop")

# sichere weighted mean Funktion
weighted_mean_age <- function(age, population) {
  if(length(age) == 0 || sum(population, na.rm = TRUE) == 0) return(NA_real_)
  sum(age * population, na.rm = TRUE) / sum(population, na.rm = TRUE)
}

# Summarise für Bezirke
bezirk_age_means <- zuerich_age_dist %>%
  group_by(year, bezirk) %>%
  summarise(
    mean_kinder_0_12 = weighted_mean_age(age[age >= 0 & age <= 12], population[age >= 0 & age <= 12]),
    mean_erw_18_64  = weighted_mean_age(age[age >= 18 & age < 65], population[age >= 18 & age < 65]),
    mean_erw_65plus = weighted_mean_age(age[age >= 65], population[age >= 65]),
    .groups = "drop"
  )

# Problem Zürich lösen

pop_clean <- pop_all %>%
  mutate(
    # Hierarchie-Ebene erkennen
    level = case_when(
      region_trim == "Schweiz" ~ "CH",
      str_detect(region_trim, "^\\- ") ~ "Kanton",
      str_detect(region_trim, "^>> ") ~ "Bezirk",
      str_detect(region_trim, "^\\.\\.\\.\\.") ~ "Gemeinde",
      TRUE ~ "Sonst"
    ),
    
    # Text bereinigen (Prefix entfernen)
    region_clean = region_trim %>%
      str_remove("^\\- ") %>%
      str_remove("^>> ") %>%
      str_remove("^\\.\\.\\.\\.") %>%
      str_trim()
  ) %>%
  
  # Hierarchie aufbauen
  mutate(
    kanton = if_else(level == "Kanton", region_clean, NA_character_),
    bezirk = if_else(level == "Bezirk", region_clean, NA_character_),
    gemeinde = if_else(level == "Gemeinde", region_clean, NA_character_)
  ) %>%
  
  # Werte nach unten „durchreichen“
  fill(kanton, .direction = "down") %>%
  fill(bezirk, .direction = "down") %>%
  fill(gemeinde, .direction = "down") %>%
  
  # Schweiz-Level behalten wir separat
  mutate(kanton = if_else(level == "CH", NA_character_, kanton))




# Pivot so wir Differenz 2022 - 2010 berechnen können
bezirk_age_wide <- bezirk_age_means %>%
  pivot_longer(
    cols = starts_with("mean"),
    names_to = "gruppe",
    values_to = "mean_age"
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = mean_age,
    names_prefix = "jahr_"
  ) %>%
  mutate(differenz_2022_minus_2010 = jahr_2022 - jahr_2010)

test_wide <- bezirk_age_means %>%
  pivot_longer(
    cols = starts_with("mean"),
    names_to = "gruppe",
    values_to = "mean_age"
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = mean_age,
    names_prefix = "jahr_"
  )

unique(bezirk_age_means$year)
names(test_wide)
head(bezirk_age_means)

# Speichere als CSV
write_csv(bezirk_age_wide, "differenz_durchschnittsalter_bezirke_zuerich_2022_minus_2010.csv")

# ---- 7) Boxplot: Verteilungen des Durchschnittsalters der Minderjährigen in den Gemeinden der 11 Bezirke des Kanton Zürichs ohne die Stadt Zürich in beiden Jahren ----
# Wir berechnen pro Gemeinde das Durchschnittsalter der Minderjährigen (alter < 18), dann filtern die Bezirke (11 Bezirke ohne 'Stadt Zürich') und plotten Boxplots je Jahr.
# Falls der Bezirk der Stadt Zürich "Zürich" oder "Stadt Zürich" heißt, filtern wir ihn weg.

# Zuerst: Durchschnittsalter Minderjährige pro Gemeinde und Jahr (kanton Zürich)
minder_mean_gemeinde <- pop_all %>%
  filter(kanton == "Zürich", alter_num < 18) %>%
  group_by(jahr, bezirk, gemeinde = gebiet_trim, alter_num) %>%
  summarise(einwohner = sum(einwohner, na.rm = TRUE), .groups = "drop") %>%
  group_by(jahr, bezirk, gemeinde) %>%
  summarise(
    mean_age_minder = if_else(sum(einwohner, na.rm = TRUE) == 0, NA_real_,
                              sum(alter_num * einwohner, na.rm = TRUE) / sum(einwohner, na.rm = TRUE)),
    .groups = "drop"
  )

# Exkludiere Stadt Zürich: wir versuchen beides zu erwischen ("Zürich", "Stadt Zürich", "Zuerich")
exclude_names <- c("Stadt Zürich", "Zürich", "Zuerich")
minder_mean_gemeinde_plot <- minder_mean_gemeinde %>%
  filter(!bezirk %in% exclude_names)

# Plot
library(ggplot2)
p <- ggplot(minder_mean_gemeinde_plot, aes(x = factor(jahr), y = mean_age_minder)) +
  geom_boxplot() +
  facet_wrap(~ bezirk, scales = "free_y") +
  labs(
    title = "Verteilung des Durchschnittsalters der Minderjährigen (Gemeinden) in den Bezirken des Kantons Zürich",
    subtitle = "Ohne die Stadt Zürich — Vergleich 2010 vs 2022",
    x = "Jahr",
    y = "Durchschnittsalter Minderjährige (Jahre)"
  ) +
  theme_minimal()

# Speichern des Plots
ggsave("boxplot_minderjaehrige_zuerich_bezirke_ohne_stadtzuerich.png", plot = p, width = 14, height = 10, dpi = 300)

# ---- Zusätzliche Speicherung: Nützliche Tabellen für Weiterverwendung ----
write_csv(wide_groups, "gemeinden_altersgruppen_breit_2010_2022.csv")
write_csv(minder_mean_gemeinde, "mean_age_minder_gemeinde_zuerich_2010_2022.csv")

