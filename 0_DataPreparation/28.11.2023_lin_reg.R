library(tibble)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)


# Säubern der Umgebung
remove(list = ls())

# Einladen der Daten
kiwo <- read.csv("C:/Users/anna-/Documents/GitHub/Gruppe1/0_LiteratureReview/kiwo.csv", header = TRUE, sep = ",", dec = ".")
wetter <- read.csv("C:/Users/anna-/Documents/GitHub/Gruppe1/0_LiteratureReview/wetter.csv", header = TRUE, sep = ",", dec = ".")
umsatz <- read.csv("C:/Users/anna-/Documents/GitHub/Gruppe1/0_LiteratureReview/umsatzdaten_gekuerzt.csv", header = TRUE, sep = ",", dec = ".")

# Konvertierung von "Datum" zu Date
kiwo$Datum <- as.Date(kiwo$Datum, format = "%Y-%m-%d")
wetter$Datum <- as.Date(wetter$Datum, format = "%Y-%m-%d")
umsatz$Datum <- as.Date(umsatz$Datum, format = "%Y-%m-%d")

# Erweiterung des Umsatzes durch die fehlenden Werte
alle_warengruppen <- unique(umsatz$Warengruppe)
platzhalter <- expand.grid(Datum = seq(min(umsatz$Datum), max(umsatz$Datum), by = "1 day"), Warengruppe = alle_warengruppen)
umsatz_voll <- platzhalter %>%
  left_join(umsatz, by = c("Datum", "Warengruppe"))  %>%
  mutate(Umsatz = coalesce(Umsatz, 0))

# Zusammenführen der Daten
daten_tibble <- wetter %>%
  left_join(umsatz_voll, by = "Datum") %>%
  left_join(kiwo, by = "Datum")

# NA zu 0
daten_tibble <- daten_tibble %>%
  mutate(KielerWoche = ifelse(is.na(KielerWoche), 0, KielerWoche))

#Entfernen der Daten vor Umsatzinformationen
daten_tibble <- daten_tibble %>%
  filter(as.Date(Datum) >= as.Date("2013-07-01"))

# Sortieren des Datensatzes nach Datum und Warengruppe
daten_tibble <- daten_tibble %>%
  arrange(Datum, Warengruppe)

#Aufteilung des Datums in Jahr, Woche,Monat und Wochentag
daten_tibble <- daten_tibble %>%
  mutate(Woche = week(Datum), Jahr = year(Datum), Monat = month(Datum))
# neue Spalte Wochentag
daten_tibble <- daten_tibble %>%
  mutate(Wochentag = weekdays(Datum))

# Gruppieren nach Datum und Warengruppe
daten_tibble <- daten_tibble %>%
  group_by(Warengruppe) %>%
  mutate(vt_umsatz = lag(Umsatz, default = NA))

# Aufhen der Gruppierung
daten_tibble <- daten_tibble %>%
  ungroup()

# Gruppieren nach Woche, und aufsummieren des Umsatzes pro Woche
wochenumsatz <- daten_tibble %>%
  group_by(Woche, Jahr) %>%
  summarize(Wochenumsatz = sum(Umsatz, na.rm = FALSE)) %>%
  mutate(Woche = Woche + 1)
daten_tibble <-  daten_tibble %>%
  left_join(wochenumsatz, by = c("Woche", "Jahr"))

# Gruppieren nach Woche und Warengruppe, und aufsummieren des Umsatzes pro Woche
wochenumsatz_wg <- daten_tibble %>%
  group_by(Warengruppe, Woche, Jahr) %>%
  summarize(Wochenumsatz_wg = sum(Umsatz, na.rm = FALSE)) %>%
  mutate(Woche = Woche + 1)
daten_tibble <-  daten_tibble %>%
  left_join(wochenumsatz_wg, by = c("Warengruppe", "Woche", "Jahr"))
  
# Schulferienzeiträume SH
ferien <- list(
  c(as.Date("2013-07-01"), as.Date("2013-08-03")),
  c(as.Date("2013-10-04"), as.Date("2013-10-18")),
  c(as.Date("2013-12-12"), as.Date("2014-01-06")),
  c(as.Date("2014-04-16"), as.Date("2014-05-02")),
  c(as.Date("2014-07-14"), as.Date("2014-08-23")),
  c(as.Date("2014-10-13"), as.Date("2014-10-25")),
  c(as.Date("2014-12-22"), as.Date("2015-01-15")),
  c(as.Date("2015-04-01"), as.Date("2015-04-15")),
  c(as.Date("2015-07-20"), as.Date("2015-08-15")),
  c(as.Date("2015-10-19"), as.Date("2015-10-31")),
  c(as.Date("2015-12-21"), as.Date("2016-01-16")),
  c(as.Date("2016-03-24"), as.Date("2016-04-16")),
  c(as.Date("2016-07-25"), as.Date("2016-09-03")),
  c(as.Date("2016-10-17"), as.Date("2016-10-29")),
  c(as.Date("2016-12-23"), as.Date("2017-01-06")),
  c(as.Date("2017-04-07"), as.Date("2017-04-21")),
  c(as.Date("2017-07-24"), as.Date("2017-09-02")),
  c(as.Date("2017-10-16"), as.Date("2017-10-27")),
  c(as.Date("2017-12-21"), as.Date("2018-01-06")),
  c(as.Date("2018-03-29"), as.Date("2018-04-18")),
  c(as.Date("2018-07-09"), as.Date("2018-08-18")),
  c(as.Date("2018-10-01"), as.Date("2018-10-18")),
  c(as.Date("2018-12-21"), as.Date("2019-01-04")),
  c(as.Date("2019-04-04"), as.Date("2019-04-18")),
  c(as.Date("2019-07-01"), as.Date("2019-08-10")),
  c(as.Date("2019-10-04"), as.Date("2019-10-18")),
  c(as.Date("2019-12-23"), as.Date("2019-12-31"))
)

langer_zeitraum <- seq(as.Date("2013-07-01"), as.Date("2019-12-31"), by = "days")

# Daten erstellen
ferien_tibble <- tibble(
  Datum = langer_zeitraum,
  Ferientage = 0  # Standardmäßig NA setzen
)

# markieren der Ferien mit 1
for (zeitraum in ferien) {
  ferien_tibble$Ferientage[ferien_tibble$Datum %in% seq(zeitraum[1], zeitraum[2], by = "days")] <- 1
}

daten_tibble <- left_join(daten_tibble, ferien_tibble, by ="Datum")

# lese weitere csv-Datein zur erwiterung des Datensatzes
feiertage <- read.csv("C:/Users/anna-/Documents/GitHub/Gruppe1/0_LiteratureReview/feiertage.csv", header = TRUE, sep = ",", dec = ".")
wahltage <-  read.csv("C:/Users/anna-/Documents/GitHub/Gruppe1/0_LiteratureReview/wahltage.csv", header = TRUE, sep = ",", dec = ".")
wetter_plus <-  read.csv("C:/Users/anna-/Documents/GitHub/Gruppe1/0_LiteratureReview/wetter_plus.csv", header = TRUE, sep = ";", dec = ".")
index <-  read.csv("C:/Users/anna-/Documents/GitHub/Gruppe1/0_LiteratureReview/index_backwaren_mit_mehrwertst.csv", header = TRUE, sep = ";", dec = ".")

# Datum umformatieren
feiertage$Datum <- as.Date(feiertage$Datum, format = "%d.%m.%Y")
wahltage$Datum <- as.Date(wahltage$Datum, format = "%m/%d/%Y")
wetter_plus$Datum <- as.Date(wetter_plus$Datum, format = "%d.%m.%Y")

# zusammenführen der Datensätze
daten_tibble <- left_join(daten_tibble, feiertage, by ="Datum")
daten_tibble <- left_join(daten_tibble, wahltage, by ="Datum")
daten_tibble <- left_join(daten_tibble, wetter_plus, by ="Datum")
daten_tibble <-  daten_tibble %>%
  left_join(index, by = c("Monat", "Jahr"))

# NA in 0 
daten_tibble <- daten_tibble %>%
  mutate(Feiertag = ifelse(is.na(Feiertag), 0, Feiertag),
         Wahl = ifelse(is.na(Wahl), 0, Wahl))
#
# Erstellung des linearen Models
# linearesmodell <- lm(Umsatz ~ as.factor(Warengruppe) + Wettercode, data = daten_tibble)
# summary(linearesmodell)


# Speichern des Tibble als CSV-Datei
write.csv(daten_tibble, file = "C:/Users/anna-/Documents/GitHub/Gruppe1/0_LiteratureReview/meteolytix.csv", row.names = FALSE)

# Durchschnittlichen Umsatz pro Monat 
durchschnitt_umsatz_monat <- daten_tibble %>%
  group_by(Monat) %>%
  summarise(Durchschnitt_Umsatz_Monat = mean(Umsatz, na.rm = TRUE),
            sd_Durchschnitt_Umsatz_Monat = sd(Umsatz, na.rm = TRUE))

# Balkendiagramm mit Fehlerbalken für den durchschnittlichen Umsatz pro Monat
ggplot(durchschnitt_umsatz_monat, aes(x = factor(Monat), y = Durchschnitt_Umsatz_Monat)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = Durchschnitt_Umsatz_Monat - sd_Durchschnitt_Umsatz_Monat, 
                    ymax = Durchschnitt_Umsatz_Monat + sd_Durchschnitt_Umsatz_Monat),
                width = 0.25) +
  labs(title = "Durchschnittlicher Umsatz pro Monat",
       x = "Monat",
       y = "Durchschnittlicher Monatsumsatz") +
  theme_minimal()


# Umsatz pro Jahr 
Umsatz_Jahr <- daten_tibble %>%
  group_by(Jahr) %>%
  summarise(Umsatz_Jahr = sum(Umsatz, na.rm = TRUE),
            sd_Umsatz_Jahr = sd(Umsatz, na.rm = TRUE))

# Balkendiagramm mit Fehlerbalken für den durchschnittlichen Umsatz pro Monat erstellen
ggplot(Umsatz_Jahr, aes(x = factor(Jahr), y = Umsatz_Jahr)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = Umsatz_Jahr - sd_Umsatz_Jahr, 
                    ymax = Umsatz_Jahr + sd_Umsatz_Jahr),
                width = 0.25) +
  labs(title = "Umsatz pro Jahr",
       x = "Jahr",
       y = "Jahresumsatz") +
  theme_minimal()

# Umsatz pro Feiertage
Umsatz_feiertage <- daten_tibble %>%
  group_by(Feiertag) %>%
  summarise(Umsatz_feier = mean(Umsatz, na.rm = TRUE),
            sd_Umsatz_feier = sd(Umsatz, na.rm = TRUE))

ggplot(Umsatz_feiertage, aes(x = factor(Feiertag), y = Umsatz_feier)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = Umsatz_feier - sd_Umsatz_feier, 
                    ymax = Umsatz_feier + sd_Umsatz_feier),
                width = 0.25) +
  labs(title = "Umsatz pro Feiertag",
       x = "Feiertag",
       y = "Umsatz") +
  theme_minimal()
