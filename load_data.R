library(readxl)
library(data.table)
library(labelled)

load_and_clean_data <- function(file_path, codebook) {
  
  # 1. Load the file
  raw <- read_excel(file_path)
  raw <- as.data.table(raw)
  codebook <- as.data.table(codebook)
  
  # 2. Dynamic column filter (Keep only relevant metadata and survey items)
  valid_cols <- grep("^(CASE|MISSING|[A-Z]{2}[0-9]{2})", names(raw), value = TRUE)
  med <- raw[, ..valid_cols] 
  
  # ==========================================
  # 3. DYNAMIC RENAMING (e.g., BB01_01 -> BB01)
  # ==========================================
  # Find all items that are NOT multiple choice according to the codebook
  single_items <- codebook[class %in% c("single", "numeric"), item]
  
  for (itm in single_items) {
    if (!(itm %in% names(med))) {
      # Search for a column that starts with the item and has a numeric suffix
      matching_col <- grep(paste0("^", itm, "_[0-9]+$"), names(med), value = TRUE)
      
      # If exactly one column is found, rename it to the clean item code
      if (length(matching_col) == 1) {
        setnames(med, matching_col, itm)
      }
    }
  }
  
  # 4. Extract labels (from the first row) and remove them from the dataset
  labels <- as.list(med[1, ])
  med <- med[-1, ]
  
  # 5. Convert data types (automatically converts numbers to numeric)
  med <- type.convert(med, as.is = TRUE) 
  
  # ==========================================
  # 6. DYNAMIC FILTERING (Age and dropouts)
  # ==========================================
  alter_item <- codebook[grepl("Alter", label, ignore.case = TRUE), item]
  gesch_item <- codebook[grepl("Geschlecht", label, ignore.case = TRUE), item]
  
  if (length(alter_item) == 1 && length(gesch_item) == 1) {
    # get() tells data.table to evaluate the dynamic string as a column name
    med <- med[MISSING != 100 & get(alter_item) >= 60 & get(gesch_item) != "keine Angabe"]
  }
  
  # ==========================================
  # 6.1 DYNAMIC COLUMN REMOVAL (Data Protection)
  # ==========================================
  # Search for the term in the extracted labels list (returns the index)
  ds_index <- grep("Datenschutzeinwilligung", labels, ignore.case = TRUE)
  
  if (length(ds_index) > 0) {
    # Get the actual column name(s) (e.g., "AA02")
    ds_cols <- names(labels)[ds_index]
    
    # 1. Remove the column from the data.table
    med[, (ds_cols) := NULL]
    
    # 2. Remove the item from the labels list so it stays in sync
    labels[ds_cols] <- NULL
  }
  
  # 7. Reattach the labels as variable attributes
  var_label(med) <- labels
  
  # ==========================================
  # 8. DYNAMIC STRING CLEANING
  # ==========================================
  
  # Fix living environment (Wohnumfeld) strings
  wohn_item <- codebook[grepl("Wohnumfeld", label, ignore.case = TRUE), item]
  if (length(wohn_item) == 1 && wohn_item %in% names(med)) {
    # Syntax: (wohn_item) := overwrites the dynamically found column
    med[get(wohn_item) == "städtisch, stark bebaut, eher viel Grün", 
        (wohn_item) := "städtisch, stark bebaut,\n eher viel Grün"]
  }
  
  # Fix education (Schulabschluss) strings
  schul_item <- codebook[grepl("Schulabschluss", label, ignore.case = TRUE), item]
  if (length(schul_item) == 1 && schul_item %in% names(med)) {
    med[get(schul_item) == "Fachhochschulreife oder Hochschulreife ((Fach-)Abitur)", 
        (schul_item) := "(Fach-)Abitur"]
    
    med[get(schul_item) == "Hauptschulabschluss / Volksschulabschluss", 
        (schul_item) := "Hauptschulabschluss /\n Volksschulabschluss"]
  }
  med[, MISSING := NULL]
  return(med)
}

# library(readxl)
# medicus <- read_excel("data/raw/data_medicus-app_2026-08-11.xlsx")
# medicus_codebook <- read_excel("data/medicus_codebook.xlsx")
# med <- load_and_clean_data("data/raw/data_medicus-app_2026-08-11.xlsx", medicus_codebook)
