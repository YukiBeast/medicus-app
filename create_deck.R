create_deck <- function(data) {
  card_list <- list()
  col_names <- names(data)
  labels_list <- var_label(data)
  
  # 1. Find all potential prefixes
  base_names <- unique(gsub("_.*$", "", col_names))
  
  # 2. FILTER: Keep only base names that contain at least one number (0-9)
  base_names <- grep("[0-9]", base_names, value = TRUE)
  
  # 3. Loop through each valid base name
  for (base in base_names) {
    
    # Extract the raw label from your list (fallback to base name if missing)
    raw_label <- if (!is.null(labels_list[[base]])) labels_list[[base]] else base
    
    # Clean the label: remove ":" and everything after, then trim whitespace
    clean_label <- trimws(sub(":.*$", "", raw_label))
    
    # Find all columns that start with this base name + an underscore
    mc_columns <- grep(paste0("^", base, "_"), col_names, value = TRUE)
    
    if (length(mc_columns) > 1) {
      card_list[[base]] <- structure(
        list(
          name = base,
          prefix = paste0("^", base, "_"),
          label = clean_label 
        ),
        class = c("multiple_choice", "categorical")
      )
      
    } else if (base %in% col_names) {
      card_list[[base]] <- structure(
        list(
          name = base,
          label = clean_label 
        ),
        class = c("single_choice", "categorical")
      )
    }
  }
  
  return(card_list)
}

# --- Usage ---
# Pass both your data and your labels object into the function
# my_deck <- create_deck(med)
