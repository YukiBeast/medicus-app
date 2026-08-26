create_barplot <- function(item1, item2, data) {
  dt1 <- extract(item1, data)
  dt2 <- extract(item2, data)
  
  forplot <- dt1[dt2, on = "id"]
  
  library(dplyr)
  library(paletteer)
  forplot %>%
    # 1. Count the occurrences of each combination
    count(answer, i.answer) %>%
    # 2. Calculate the proportion relative to the total dataset
    mutate(prop = n / sum(n), .by = answer) %>%
    # 3. Plot using geom_col instead of geom_bar
    ggplot(aes(x = answer, y = prop, fill = i.answer)) +
    geom_col(position = "dodge", col = "black") +
    labs(x = paste0("Frage 1: ", sub("[0-9].[0-9]", "", item1$label)),
         y = "Anteil",
         fill = paste0("Frage 2: ", sub("[0-9].[0-9]", "", item2$label))) +
    theme_minimal(base_size = 22) +
    scale_fill_paletteer_d("tvthemes::EarthKingdom") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
}

# create_barplot(item1, item2, med)

