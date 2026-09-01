library(data.table)
library(labelled)

# 1. Esegui questo script solo una volta nel tuo ambiente locale
generate_codebook <- function(data) {
  
  ordinal_scales <- list(
    "CC02" = "howmuch",
    "DD03" = "intens",
    "EE01" = "freq",
    "DD01" = "freq",
    "EE02" = "freq",
    "EE04" = "intens",
    "FF01" = "freq",
    "FF03" = "intens",
    "FF05" = "freq",
    "GG01" = "freq",
    "HH01" = "helpful",
    "HH03" = "yesno",
    "HH05" = "yesno",
    "II04" = "education"
  )
  
  col_names <- names(data)
  labels_list <- var_label(data)
  
  base_names <- unique(gsub("_.*$", "", col_names))
  base_names <- grep("[0-9]", base_names, value = TRUE)
  
  codebook_rows <- list()
  
  for (base in base_names) {
    raw_label <- if (!is.null(labels_list[[base]])) labels_list[[base]] else base
    clean_label <- trimws(sub(":.*$", "", raw_label))
    mc_columns <- grep(paste0("^", base, "_"), col_names, value = TRUE)
    
    # Determina classe e scala
    if (length(mc_columns) > 1) {
      class_val <- "multiple"
      scale_val <- NA
      breaks_val <- NA
      
    } else if (base %in% names(ordinal_scales)) {
      class_val <- "single"
      scale_val <- ordinal_scales[[base]]
      breaks_val <- NA
      
    } else if (base == "BB01") {
      clean_label = "Alter"
      class_val <- "single"
      scale_val <- "numeric"
      breaks_val <- "59, 69, 74, 79, Inf"
    }
    else {
      class_val <- "single"
      scale_val <- "categorical"
      breaks_val <- NA
    }
    
    if (base == "II03") {
      clean_label <- "Was macht die App angenehm"
    }
    
    codebook_rows[[base]] <- data.table(
      item = base,
      label = clean_label,
      class = class_val,
      scale = scale_val,
      breaks = breaks_val # Colonna pronta per i tagli numerici (es. 59,69,74,79)
    )
    

  }
  

  
  cb <- rbindlist(codebook_rows)
  return(cb)
}

# generate_codebook(med) |> View()
# ordinal_scales[[1]]
# Genera e salva!
# cb <- generate_codebook(med)
# write.csv2(cb, "data/medicus_codebook.csv", row.names = FALSE)
