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
  res <- melt(med, id.vars = "CASE", measure.vars = patterns(item$prefix),
              variable.name = "answer", variable.factor = FALSE)
  
  # Filtert nur die echten Auswahlen und behält ID + Name der Option
  res <- res[value == "ausgewählt", .(id = CASE, answer = assign_label(answer))]
  
  return(res)
}

# extract(my_deck$BB02, med)  # single choice
# extract(my_deck$DD02, med)  # multiple choice
