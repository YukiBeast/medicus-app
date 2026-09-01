extract <- function(item, data) {
  UseMethod("extract")
}

# Methode für Single Choice
extract.single_choice <- function(item, data) {
  
  # Holt die ID und die spezifische Spalte
  res <- data[, .(id = CASE, answer = get(item$name))]
  
  return(res)
}

# Methode für Multiple Choice
extract.multiple_choice <- function(item, data) {
  
  # Wandelt MC-Spalten ins Long-Format um
  res <- melt(data, id.vars = "CASE", measure.vars = patterns(item$prefix),
              variable.name = "answer", variable.factor = FALSE)
  
  # Filtert nur die echten Auswahlen und behält ID + Name der Option
  res <- res[value == "ausgewählt", .(id = CASE, answer = assign_labels(answer, data = data))]
  
  return(res)
}

extract.ordinal <- function(item, data) {
  # 1. Fetch raw data
  temp_data <- data[, .(id = CASE, raw_wert = get(item$name))]
  
  # 2. Convert to ordered factor
  temp_data[, answer := factor(raw_wert, 
                               levels = item$levels, 
                               ordered = TRUE)]
  
  # 3. Return standardized format
  return(temp_data[, .(id, answer)])
}

extract.numeric <- function(item, data) {
  # 1. Prendi i dati grezzi numerici
  temp_data <- data[, .(id = CASE, raw_wert = get(item$name))]
  
  # 2. Taglia la variabile continua in categorie usando i breaks della carta
  temp_data[, answer := cut(raw_wert, 
                            breaks = item$breaks, 
                            right = TRUE)]
  
  # 3. Restituisci il formato standard per plot e tabelle
  return(temp_data[, .(id, answer)])
}
# 
# extract(initial_deck$BB01, med)  # numeric (Alter)
# extract(initial_deck$BB02, med)
# extract(initial_deck$DD02, med)  # multiple choice

