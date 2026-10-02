library(readxl)
library(data.table)
library(labelled)
library(tools)

# Notice we now accept the original file names as arguments
load_and_clean_data <- function(data_path, data_name, codebook_path, codebook_name) {
  
  read_extension <- function(path, name) {
    # Extract extension from the original name, not the temp path
    data_extension <- tolower(file_ext(name))
    
    if (data_extension %in% c("xls", "xlsx")) {
      raw <- read_excel(path, na = c("", "NA"))
      return(as.data.table(raw))
      
    } else if (data_extension == "csv") {
      # fread auto-detects commas vs semicolons perfectly
      return(fread(path, na.strings = c("", "NA")))
      
    } else {
      stop("Unsupported file format. Please upload an Excel or CSV file.")
    }
  }
  
  # 1. Load the files
  med <- read_extension(data_path, data_name)
  codebook <- read_extension(codebook_path, codebook_name)
  
  # 2. Dynamic column filter 
  valid_cols <- grep("^(CASE|MISSING|[A-Z]{2}[0-9]{2})", names(med), value = TRUE)
  med <- med[, valid_cols, with = FALSE] 
  
  # ==========================================
  # 3. DYNAMIC RENAMING (e.g., BB01_01 -> BB01)
  # ==========================================
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
  
  if ("MISSING" %in% names(med)) {
    med <- med[MISSING != 100]
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
  
  return(list("data" = med, "cb" = codebook))
}