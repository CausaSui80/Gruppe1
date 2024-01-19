library(tibble)
library(dplyr)
library(tidyr)

# Säubern der Umgebung
remove(list = ls())

# Einladen der Daten
kiwo <- read.csv("https://raw.githubusercontent.com/opencampus-sh/einfuehrung-in-data-science-und-ml/main/kiwo.csv", header = TRUE, sep = ",", dec = ".")
wetter <- read.csv("https://raw.githubusercontent.com/opencampus-sh/einfuehrung-in-data-science-und-ml/main/wetter.csv", header = TRUE, sep = ",", dec = ".")
umsatz <- read.csv("https://raw.githubusercontent.com/opencampus-sh/einfuehrung-in-data-science-und-ml/main/umsatzdaten_gekuerzt.csv", header = TRUE, sep = ",", dec = ".")

# Konvertierung von "Datum" zu Date
kiwo$Datum <- as.Date(kiwo$Datum, format = "%Y-%m-%d")
wetter$Datum <- as.Date(wetter$Datum, format = "%Y-%m-%d")
umsatz$Datum <- as.Date(umsatz$Datum, format = "%Y-%m-%d")

# Erweiterung des Umsatzes durch die fehlenden Werte
alle_warengruppen <- unique(umsatz$Warengruppe)
platzhalter <- expand.grid(Datum = seq(min(umsatz$Datum), max(umsatz$Datum), by = "1 day"), Warengruppe = alle_warengruppen)
umsatz_voll <- platzhalter %>%
  left_join(umsatz, by = c("Datum", "Warengruppe"))

# Zusammenführen der Daten
daten_tibble <- wetter %>%
  left_join(umsatz_voll, by = "Datum") %>%
  left_join(kiwo, by = "Datum")

daten_tibble <- daten_tibble %>%
  mutate(KielerWoche = ifelse(is.na(KielerWoche), 0, KielerWoche))

#Entfernen der Daten vor Umsatzinformationen
daten_tibble <- daten_tibble %>%
  filter(as.Date(Datum) >= as.Date("2013-07-01"))

# Sortieren des Datensatzes nach Datum und Warengruppe
daten_tibble <- daten_tibble %>%
  arrange(Datum, Warengruppe)

# Gruppieren nach Datum und Warengruppe
daten_tibble <- daten_tibble %>%
  group_by(Warengruppe) %>%
  mutate(vt_umsatz = lag(Umsatz, default = NA))

# Aufhen der Gruppierung
daten_tibble <- daten_tibble %>%
  ungroup()



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
  Ferientage = NA  # Standardmäßig NA setzen
)

# markieren der Ferien mit 1
for (zeitraum in ferien) {
  ferien_tibble$Ferientage[ferien_tibble$Datum %in% seq(zeitraum[1], zeitraum[2], by = "days")] <- 1
}

daten_tibble <- left_join(daten_tibble, ferien_tibble, by ="Datum")

# neue Spalte Wochentag
daten_tibble <- daten_tibble %>%
  mutate(Wochentag = weekdays(Datum))

# lese weitere csv-Datein zur erwiterung des Datensatzes
feiertage <- read.csv("C:/Users/anna-/Desktop/KI/Jams/0_DataPreparation/feiertage.csv", header = TRUE, sep = ",", dec = ".")
wahltage <-  read.csv("C:/Users/anna-/Desktop/KI/Jams/0_DataPreparation/wahltage.csv", header = TRUE, sep = ",", dec = ".")
wetter_plus <-  read.csv("C:/Users/anna-/Desktop/KI/Jams/0_DataPreparation/wetter_plus.csv", header = TRUE, sep = ";", dec = ".")

# Datum umformatieren
feiertage$Datum <- as.Date(feiertage$Datum, format = "%d.%m.%Y")
wahltage$Datum <- as.Date(wahltage$Datum, format = "%m/%d/%Y")
wetter_plus$Datum <- as.Date(wetter_plus$Datum, format = "%d.%m.%Y")

# zusammenführen der Datensätze
daten_tibble <- left_join(daten_tibble, feiertage, by ="Datum")
daten_tibble <- left_join(daten_tibble, wahltage, by ="Datum")
daten_tibble <- left_join(daten_tibble, wetter_plus, by ="Datum")



#
# Erstellung des linearen Models
# linearesmodell <- lm(Umsatz ~ as.factor(Warengruppe) + Wettercode, data = daten_tibble)
# summary(linearesmodell)
# Speichern des Tibble als CSV-Datei
write.csv(daten_tibble$data, file = "C:/Users/anna-/Desktop/KI/Jams/0_DataPreparation/meteolytix.csv", row.names = FALSE)
