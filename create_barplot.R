create_barplot <- function(item1, item2, data, share, language = "EN",
                           bar_pos = "dodge",
                           fix_y = FALSE) {
  library(dplyr)
  
  question_label <- ifelse(language == "EN", "Question", "Frage")
  share_label <- ifelse(language == "EN", "Share", "Anteil")
  count_label <- ifelse(language == "EN", "Count", "Anzahl")
  
  # Determine position adjustment
  col_position <- if (bar_pos == "stack") {
    position_stack(reverse = TRUE) # Keeps ordinal levels in intuitive top-to-bottom order
  } else {
    position_dodge(preserve = "single")
  }
  
  # Helper function to force integer breaks on the Y-axis
  int_breaks <- function(x) {
    b <- pretty(x)
    b[b %% 1 == 0]
  }
  
  # Dynamic Y-axis scale block
  y_scale <- if (!share) {
    scale_y_continuous(breaks = int_breaks)
  } else if (share && fix_y) {
    scale_y_continuous(limits = c(0, 1))
  } else {
    NULL
  }
  

  
  dt1 <- extract(item1, data)
  
  # If-Case: only display 1 variable.
  if (is.null(item2) || is.character(item2) && item2 == "none") {
    summary_data <- dt1 %>% count(answer)
    
    if (share) {
      summary_data <- summary_data %>% mutate(plot_val = n / sum(n))
    } else {
      summary_data <- summary_data %>% mutate(plot_val = n)
    }
    
    return(
      summary_data %>%
        filter(!is.na(answer)) %>%
        ggplot(aes(x = answer, y = plot_val)) +
        geom_col(fill = "#66CC99") + # Feste Farbe passend zum EarthKingdom-Theme
        labs(x = paste0(question_label, " 1: ", sub("[0-9].[0-9]", "", item1$label)),
             y = ifelse(share, share_label, count_label)) +
        scale_fill_discrete(labels = function(x) stringr::str_wrap(x, width = 40)) +
        theme_minimal(base_size = 18) +
        theme(axis.title.y = element_text(margin = margin(r = 15)), # Adds a 15px gap to the right of the title
              panel.grid.major.x = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        y_scale
      )
  }
  
  dt2 <- extract(item2, data)
  
  forplot <- dt1[dt2, on = "id"]
  
  library(dplyr)
  library(paletteer)
  
  summary_data <- forplot %>%
    count(answer, i.answer, .drop = FALSE)
  
  if (share) {
    # FIX: Use a standard if/else instead of ifelse() to prevent vector truncation
    summary_data <- summary_data %>% 
      mutate(plot_val = if (sum(n) == 0) 0 else n / sum(n), .by = answer)
  } else {
    summary_data <- summary_data %>% 
      mutate(plot_val = n)
  }
  
  # We use the length of factors levels to set the required number of colors
  num_levels <- length(levels(summary_data$i.answer))
  
  # Check if the item's class represents ordered/numeric data
  if (any(class(item2) %in% c("ordinal", "numeric"))) {
    
    # Sequential palette for scales with natural order
    colors <- colorRampPalette(paletteer::paletteer_d("MoMAColors::Ernst"))(num_levels)
    
    # Check if the item's class represents unordered/categorical data
  } else if (any(class(item2) %in% c("single_choice", "categorical", "nominal"))) {
    
    # Distinct, qualitative palette for categories without inherent order
    colors <- colorRampPalette(paletteer::paletteer_d("palettetown::vibrava"))(num_levels)
    
  } else {
    
    # Fallback default if class isn't defined or recognized
    colors <- colorRampPalette(paletteer::paletteer_d("palettetown::deoxys"))(num_levels)
  }
  
  summary_data %>%
    filter(!is.na(answer) & !is.na(i.answer)) %>%
    ggplot(aes(x = answer, y = plot_val, fill = i.answer)) +
    geom_col(position = col_position) +
    scale_x_discrete(drop = FALSE) +
    labs(x = paste0(question_label, " 1: ", sub("[0-9].[0-9]", "", item1$label)),
         y = ifelse(share, share_label, count_label),
         fill = paste0(question_label, " 2: ", sub("[0-9].[0-9]", "", item2$label))) +
    theme_minimal(base_size = 18) +
    scale_fill_manual(
      values = colors, 
      drop = FALSE, 
      labels = function(x) stringr::str_wrap(x, width = 45)
    ) +
    theme(
      axis.title.y = element_text(margin = margin(r = 15)), # Adds a 15px gap to the right of the title
      panel.grid.major.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom") +
    guides(fill = guide_legend(ncol = 1, title.position = "left")) +
    y_scale
}

# create_barplot(item1, item2, med)

