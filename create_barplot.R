create_barplot <- function(item1, item2, data, share) {
  library(dplyr)
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
        ggplot(aes(x = answer, y = plot_val)) +
        geom_col(fill = "#6D8D5A") + # Feste Farbe passend zum EarthKingdom-Theme
        labs(x = paste0("Frage: ", sub("[0-9].[0-9]", "", item1$label)),
             y = ifelse(share, "Anteil", "Anzahl")) +
        scale_fill_discrete(labels = function(x) stringr::str_wrap(x, width = 40)) +
        theme_minimal(base_size = 22) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  }
  
  dt2 <- extract(item2, data)
  
  forplot <- dt1[dt2, on = "id"]
  
  library(dplyr)
  library(paletteer)
  
  summary_data <- forplot %>%
    count(answer, i.answer)
  
  # 2. Bedingung: Anteil vs. absolute Anzahl
  if (share) {
    summary_data <- summary_data %>% 
      mutate(plot_val = n / sum(n), .by = answer)
  } else {
    summary_data <- summary_data %>% 
      mutate(plot_val = n)
  }
  
  colors <- colorRampPalette(paletteer::paletteer_d("palettetown::deoxys"))(length(levels(summary_data$i.answer)))
  
  summary_data %>%
    ggplot(aes(x = answer, y = plot_val, fill = i.answer)) +
    geom_col(position = position_dodge(preserve = "single")) +
    labs(x = paste0("Frage 1: ", sub("[0-9].[0-9]", "", item1$label)),
         y = ifelse(share, "Anteil", "Anzahl"),
         fill = paste0("Frage 2: ", sub("[0-9].[0-9]", "", item2$label))) +
    theme_minimal(base_size = 22) +
    scale_fill_manual(
      values = colors, 
      drop = FALSE, 
      labels = function(x) stringr::str_wrap(x, width = 45)
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom") +
    guides(fill = guide_legend(ncol = 2, title.position = "left"))
    
}

# create_barplot(item1, item2, med)

