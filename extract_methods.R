extract <- function(item, data) {
  UseMethod("extract")
}

# Methode für Single Choice
extract.single_choice <- function(item, data) {
  
  all_answers <- unique(data[[item$name]])
  # Holt die ID und die spezifische Spalte
  res <- data[, .(id = CASE, answer = factor(get(item$name),
                                             levels = all_answers,
                                             ordered = FALSE))]
  
  return(res)
}

# Methode für Multiple Choice
extract.multiple_choice <- function(item, data) {
  
  # Wandelt MC-Spalten ins Long-Format um
  res <- melt(data, id.vars = "CASE", measure.vars = patterns(item$prefix),
              variable.name = "answer", variable.factor = FALSE)
  all_answers <- unique(assign_labels(res$answer, data = data))
  # Filtert nur die echten Auswahlen und behält ID + Name der Option
  res <- res[value == "ausgewählt", .(id = CASE, answer = factor(assign_labels(answer, data = data),
                                                                 levels = all_answers))]
  
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
  # 1. Get the raw numeric data
  raw_values <- data[[item$name]]
  
  # 2. Get the breaks from the Codebook configuration
  my_breaks <- item$breaks 
  
  # 3. Generate the pretty labels
  pretty_labels <- generate_nice_labels(my_breaks)
  
  # 4. Cut the data using the pretty labels
  output <- data[, .(id = CASE, answer = cut(
    raw_values, 
    breaks = my_breaks, 
    labels = pretty_labels, 
    right = TRUE
  ))]
  
  # Return as a formatted data.frame for the UI
  return(output)
}

generate_nice_labels <- function(breaks) {
  labels <- character(length(breaks) - 1)
  
  for (i in 1:(length(breaks) - 1)) {
    lower <- breaks[i] + 1  # Add 1 because (59, 69] starts at 60
    upper <- breaks[i + 1]
    
    if (is.infinite(upper)) {
      labels[i] <- paste0(lower, "+")
    } else {
      labels[i] <- paste0(lower, "-", upper)
    }
  }
  
  return(labels)
}

# 
# extract(initial_deck$BB01, med)  # numeric (Alter)
# extract(initial_deck$BB02, med)
# extract(initial_deck$DD02, med)  # multiple choice

