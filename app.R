library(shiny)
library(data.table)
library(ggplot2)

# ==========================================
# 1. GLOBAL SETUP
# ==========================================
# Load only the FUNCTIONS, do not execute the data yet
source("load_data.R")         # Must contain a function: load_and_clean_data(file_path)
source("assign_labels.R")     # Assuming 'labels_list' is created here
source("create_deck.R")
source("extract_methods.R")
source("create_cross_table.R")
source("create_barplot.R")


# ==========================================
# 2. USER INTERFACE (UI)
# ==========================================
ui <- fluidPage(
  
  titlePanel("MEDICUS: Interaktive Datenexploration"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Daten"),
      # Module to upload the file (e.g., CSV)
      fileInput(inputId = "data_upload", 
                label = "Datendatei hochladen:", 
                accept = c(".csv", ".xlsx", ".rds")),
      
      # OK Button
      actionButton(inputId = "btn_ok", label = "Daten verarbeiten", class = "btn-primary"),
      
      hr(), # Horizontal separator line
      
      h4("2. Variablen"),
      # Empty dropdowns at the start (will be populated after clicking OK)
      selectInput(inputId = "var1", label = "1. Variable (Gruppierung/X-Achse):", choices = NULL),
      selectInput(inputId = "var2", label = "2. Variable (Fokus/Farbe):", choices = NULL)
    ),
    
    mainPanel(
      h3("Visualisierung"),
      plotOutput("plot_output"),
      
      hr(),
      
      h3("Kreuztabelle"),
      tableOutput("table_output")
    )
  )
)


# ==========================================
# 3. SERVER LOGIC
# ==========================================
server <- function(input, output, session) {
  
  # Reactive container to store the data and the deck after processing
  rv <- reactiveValues(
    daten = NULL,
    deck = NULL
  )
  
  # What happens when the user clicks on "Daten verarbeiten" (OK)
  observeEvent(input$btn_ok, {
    
    # Ensure that the user has actually uploaded a file
    req(input$data_upload) 
    
    # 1. Load and clean the data using the path of the newly uploaded file
    # (Make sure that load_data.R contains this function)
    rv$daten <- load_and_clean_data(input$data_upload$datapath)
    
    # 2. Create the card deck
    rv$deck <- create_deck(rv$daten)
    
    # 3. Generate the list for the dropdown menus
    dropdown_choices <- setNames(names(rv$deck), sapply(names(rv$deck), function(code) {
      
      # 1. Kategorie-Zahl aus dem Buchstaben generieren (A=0, B=1, C=2, D=3...)
      # Zieht den 1. Buchstaben (z.B. "C" aus "CC02")
      first_letter <- substring(code, 1, 1) 
      cat_num <- match(first_letter, LETTERS) - 1
      
      # 2. Fragen-Zahl aus dem Code ziehen (z.B. "02" -> 2)
      question_num <- as.numeric(substring(code, 3, 4))
      
      # 3. Alte Zahlen (wie "3.4" oder "2") aus dem ursprünglichen Label entfernen
      old_label <- rv$deck[[code]]$label
      # Regex \b sucht nach eigenständigen Zahlen (mit oder ohne Punkt)
      clean_label <- gsub("\\b[0-9]+(\\.[0-9]+)?\\b", "", old_label)
      
      # 4. Überflüssige Leerzeichen (die durch das Löschen entstehen) entfernen
      clean_label <- trimws(gsub("\\s+", " ", clean_label))
      
      # 5. Neues Label zusammenbauen (z.B. "2.2 Beschwerden")
      return(paste0(cat_num, ".", question_num, " ", clean_label))
    }))
    
    # 4. Update the dropdown menus in the UI with the newly generated names
    updateSelectInput(session, "var1", choices = dropdown_choices)
    
    # Select the second element by default, if it exists
    if (length(dropdown_choices) > 1) {
      updateSelectInput(session, "var2", choices = dropdown_choices, selected = dropdown_choices[2])
    } else {
      updateSelectInput(session, "var2", choices = dropdown_choices)
    }
  })
  
  
  # Output: Plot
  output$plot_output <- renderPlot({
    # req() stops execution if data or variables have not been loaded/selected yet
    req(rv$deck, input$var1, input$var2) 
    
    karte1 <- rv$deck[[input$var1]]
    karte2 <- rv$deck[[input$var2]]
    
    create_barplot(karte1, karte2, rv$daten)
  })
  
  # Output: Table
  output$table_output <- renderTable({
    req(rv$deck, input$var1, input$var2)
    
    karte1 <- rv$deck[[input$var1]]
    karte2 <- rv$deck[[input$var2]]
    
    create_cross_table(karte1, karte2, rv$daten)
  }, rownames = TRUE)
  
}

shinyApp(ui = ui, server = server)