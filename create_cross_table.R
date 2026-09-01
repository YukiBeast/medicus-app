create_cross_table <- function(item1, item2 = NULL, data, share = TRUE) {
  dt1 <- extract(item1, data)
  
  # Das saubere Label der ersten Frage für die Spaltenbeschriftung
  label1 <- sub("[0-9]+\\.[0-9]+\\s?", "", item1$label)
  
  # =======================================
  # 1. UNIVARIATER FALL
  # =======================================
  if (is.null(item2) || (is.character(item2) && item2 == "none")) {
    tbl <- tbl <- as.data.frame(table(dt1$answer, useNA = "ifany"))
    
    if (share) {
      # Berechne Prozente und formatiere sie mit "%" Zeichen
      tbl$Freq <- (tbl$Freq / sum(tbl$Freq)) * 100
      tbl$Freq <- sprintf("%.1f %%", tbl$Freq)
      col2_name <- "Anteil"
    } else {
      # Als Ganzzahl erzwingen (verhindert Kommastellen bei Anzahlen)
      tbl$Freq <- as.integer(tbl$Freq)
      col2_name <- "Anzahl"
    }
    
    # Spalten schön benennen!
    colnames(tbl) <- c(label1, col2_name)
    return(tbl)
  }
  
  # =======================================
  # 2. BIVARIATER FALL
  # =======================================
  dt2 <- extract(item2, data)
  forplot <- dt1[dt2, on = "id"]
  
  # Basis-Kreuztabelle
  tbl <- table(forplot$answer, forplot$i.answer, useNA = "ifany")
  
  if (share) {
    # Zeilenprozente berechnen (margin = 1) - passend zu deinem Plot!
    tbl <- prop.table(tbl, margin = 1) * 100
    tbl_df <- as.data.frame.matrix(tbl)
    
    # Alle Spalten mit "%" formatieren
    tbl_df[] <- lapply(tbl_df, function(x) sprintf("%.1f %%", x))
  } else {
    tbl_df <- as.data.frame.matrix(tbl)
    # Alle Spalten als Ganzzahlen formatieren
    tbl_df[] <- lapply(tbl_df, as.integer)
  }
  
  return(tbl_df)
}