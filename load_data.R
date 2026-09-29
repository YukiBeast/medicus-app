library(readxl)
library(data.table)
library(labelled)

load_and_clean_data <- function(file_path, codebook) {
  
  # 1. Load the file
  raw <- read_excel(file_path)
  raw <- as.data.table(raw)
  codebook <- as.data.table(codebook)
  
  # 2. Dynamic column filter 
  valid_cols <- grep("^(CASE|MISSING|[A-Z]{2}[0-9]{2})", names(raw), value = TRUE)
  
  # FIX 1: Use 'with = FALSE', which is much more stable in Shinylive than '..valid_cols'
  med <- raw[, valid_cols, with = FALSE] 
  
  # ==========================================
  # 3. DYNAMIC RENAMING (e.g., BB01_01 -> BB01)
  # ==========================================
  # FIX 2: Check if columns exist and use base-R subsetting to avoid WASM typo-crashes
  if ("class" %in% names(codebook) && "item" %in% names(codebook)) {
    single_items <- codebook$item[codebook$class %in% c("single", "numeric")]
  } else {
    single_items <- character(0)
  }
  
  for (itm in single_items) {
    if (!(itm %in% names(med))) {
      matching_col <- grep(paste0("^", itm, "_[0-9]+$"), names(med), value = TRUE)
      if (length(matching_col) == 1) {
        setnames(med, matching_col, itm)
      }
    }
  }
  
  # 4. Extract labels (from the first row) and remove them from the dataset
  labels <- as.list(med[1, ])
  med <- med[-1, ]
  
  # 5. Convert data types
  med <- type.convert(med, as.is = TRUE) 
  
  # ==========================================
  # 6. DYNAMIC FILTERING (Age and dropouts)
  # ==========================================
  if ("label" %in% names(codebook) && "item" %in% names(codebook)) {
    alter_item <- codebook$item[grepl("Alter", codebook$label, ignore.case = TRUE)]
    gesch_item <- codebook$item[grepl("Geschlecht", codebook$label, ignore.case = TRUE)]
  } else {
    alter_item <- character(0)
    gesch_item <- character(0)
  }
  
  # FIX 3: Ensure MISSING actually exists before filtering
  if ("MISSING" %in% names(med)) {
    med <- med[MISSING != 100]
  }
  
  # FIX 4: Replace %notin% with standard base R `!(... %in% ...)`
  if (length(alter_item) == 1 && length(gesch_item) == 1 && 
      alter_item %in% names(med) && gesch_item %in% names(med)) {
    med <- med[get(alter_item) >= 60 & !(get(gesch_item) %in% c("keine Angabe", "prefer not to say"))]
  }
  
  # ==========================================
  # 6.1 DYNAMIC COLUMN REMOVAL (Data Protection)
  # ==========================================
  ds_index <- grep("Datenschutzeinwilligung", labels, ignore.case = TRUE)
  
  if (length(ds_index) > 0) {
    ds_cols <- names(labels)[ds_index]
    med[, (ds_cols) := NULL]
    labels[ds_cols] <- NULL
  }
  
  # 7. Reattach the labels as variable attributes
  var_label(med) <- labels
  
  # ==========================================
  # 8. DYNAMIC STRING CLEANING
  # ==========================================
  if ("label" %in% names(codebook) && "item" %in% names(codebook)) {
    wohn_item <- codebook$item[grepl("Wohnumfeld", codebook$label, ignore.case = TRUE)]
    if (length(wohn_item) == 1 && wohn_item %in% names(med)) {
      med[get(wohn_item) == "städtisch, stark bebaut, eher viel Grün", 
          (wohn_item) := "städtisch, stark bebaut,\n eher viel Grün"]
    }
    
    schul_item <- codebook$item[grepl("Schulabschluss", codebook$label, ignore.case = TRUE)]
    if (length(schul_item) == 1 && schul_item %in% names(med)) {
      med[get(schul_item) == "Fachhochschulreife oder Hochschulreife ((Fach-)Abitur)", 
          (schul_item) := "(Fach-)Abitur"]
      
      med[get(schul_item) == "Hauptschulabschluss / Volksschulabschluss", 
          (schul_item) := "Hauptschulabschluss /\n Volksschulabschluss"]
    }
  }
  
  return(med)
}