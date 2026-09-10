create_deck <- function(data, codebook, scales) {
  card_list <- list()
  
  for (i in seq_len(nrow(codebook))) {
    row <- codebook[i, ]
    item_code <- as.character(row$item)
    
    # Skip if the variable does not exist in the loaded data
    if (!any(grepl(item_code, names(data)))) next 
    
    card <- list(name = item_code, label = row$label)
    
    # For MULTIPLE CHOICE items
    if (row$class == "multiple") {
      card$prefix <- paste0("^", item_code, "_")
      class(card) <- c("multiple_choice", "categorical")
      
      # For SINGLE CHOICE 
    } else if (row$class == "single") {
      
      if (!is.na(row$scale) && row$scale %in% names(scales)) {
        # CASE 1: Ordinal Scale (e.g., freq, intens)
        card$levels <- scales[[row$scale]]
        class(card) <- c("ordinal", "single_choice")
        
      } else if (!is.na(row$scale) && row$scale == "categorical") {
        # CASE 2: Categorical (e.g., Geschlecht / Gender)
        class(card) <- c("single_choice", "categorical")
        
      } else if (!is.na(row$scale) && (row$scale == "numerical" || row$scale == "numeric")) {
        # CASE 3: Numeric (e.g., Alter / Age) - Now INSIDE the 'single' block!
        
        # 1. Remove any blank spaces from the string (e.g., "59, 69" -> "59,69")
        breaks_clean <- gsub(" ", "", as.character(row$breaks))
        
        # 2. Split the string and convert it into a numeric vector
        card$breaks <- as.numeric(strsplit(breaks_clean, ",")[[1]])
        
        # 3. Assign the specific class
        class(card) <- c("numeric", "single_choice")
      }
    }
    
    # Save the card to the deck (Outside of all if/else blocks!)
    card_list[[item_code]] <- card
  }
  
  return(card_list)
}

scales <- list(
  freq = c("(fast) nie",
           "selten",
           "manchmal",
           "oft",
           "(fast) immer"),
  
  intens = c("sehr leicht",
             "leicht",
             "neutral",
             "schwierig"
  ),
  
  howmuch = c("Ich kann es nicht beurteilen",
              "wenig",
              "mittelmäßig",
              "ziemlich",
              "sehr"),
  helpful = c("überhaupt nicht hilfreich",
              "wenig hilfreich",
              "neutral",
              "hilfreich",
              "sehr hilfreich"),
  
  yesno = c("nein",
            "eher nein",
            "unsicher",
            "eher ja",
            "ja, auf jeden Fall"),
  
  education = c("Andere / keine Angabe",
                "Hauptschulabschluss /\n Volksschulabschluss",
                "Mittlere Reife (Realschule)",
                "(Fach-)Abitur")
  
)

# initial_deck <- create_deck(med, medicus_codebook, scales)
