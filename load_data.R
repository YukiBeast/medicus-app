library(readxl)
library(data.table)
library(labelled)

load_and_clean_data <- function(file_path) {
  
  # 1. Datei laden
  raw <- read_excel(file_path)
  raw <- as.data.table(raw)
  
  # 2. Dynamischer Spaltenfilter
  valid_cols <- grep("^(CASE|MISSING|[A-Z]{2}[0-9]{2})", names(raw), value = TRUE)
  med <- raw[, ..valid_cols] 
  
  # 3. Platzhalter für die kategorisierte Alters-Variable erstellen
  med[, BB01_02 := c("Alter (Kat.)", rep("1", nrow(med) - 1))]
  
  # 4. Labels (erste Zeile) extrahieren und aus dem Datensatz entfernen
  labels <- as.list(med[1, ])
  med <- med[-1, ]
  
  # 5. Datentypen korrigieren
  med <- type.convert(med, as.is = TRUE) 
  
  # 6. Alter kategorisieren (Beobachtungen ab 60 Jahren)
  med[, BB01_02 := cut(BB01_01,
                       breaks = c(59, 69, 74, 79, Inf),
                       labels = c("60-69 Jahre", "70-74 Jahre", "75-79 Jahre", "80+ Jahre"),
                       right = TRUE)]
  
  # 7. Labels wieder anheften
  var_label(med) <- labels
  
  # 8. Neue Altersvariable in BB01 umbenennen
  setnames(med, "BB01_02", "BB01")
  
  # 9. Abbrecher und Personen unter 60 herausfiltern
  med <- med[MISSING != 100 & BB01_01 >= 60 & BB02 != "keine Angabe"]
  
  med[, AA01 := NULL]
  
  # --- SPALTEN NEU ORDNEN ---
  # Finde die genaue Position der alten Variable BB01_01
  pos_old <- which(names(med) == "BB01_01")
  
  # Generiere die neue Spaltenreihenfolge: BB01 direkt vor BB01_01 schieben
  new_order <- append(setdiff(names(med), "BB01"), "BB01", after = pos_old - 1)
  
  # Wende die neue Reihenfolge an
  setcolorder(med, new_order)
  
  # 10. Alte, unkategorisierte Altersvariable löschen
  med[, BB01_01 := NULL]
  
  # 11. Strings bereinigen
  med[II06 == "städtisch, stark bebaut, eher viel Grün",
      II06 := "städtisch, stark bebaut,\n eher viel Grün"]
  
  med[II04 == "Fachhochschulreife oder Hochschulreife ((Fach-)Abitur)",
      II04 := "(Fach-)Abitur"]
  
  med[II04 == "Hauptschulabschluss / Volksschulabschluss",
      II04 := "Hauptschulabschluss /\n Volksschulabschluss"]
  
  return(med)
}