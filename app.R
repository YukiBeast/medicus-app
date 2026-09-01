library(shiny)
library(data.table)
library(ggplot2)

# ==========================================
# 1. GLOBAL SETUP
# ==========================================
source("load_data.R")         
source("assign_labels.R")     
source("create_deck.R")       # Contains 'scales'
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
      h4("1. Daten & Codebook"),
      
      # Upload for Survey Data
      fileInput(inputId = "data_upload", 
                label = "1. Datendatei hochladen (z.B. Excel):", 
                accept = c(".csv", ".xlsx", ".rds")),
      
      # NEW: Upload for Codebook
      fileInput(inputId = "codebook_upload", 
                label = "2. Codebook hochladen (.csv):", 
                accept = c(".csv")),
      
      # OK Button
      actionButton(inputId = "btn_ok", label = "Daten verarbeiten", class = "btn-primary"),
      
      hr(), 
      
      h4("2. Variablen"),
      selectInput(inputId = "var1", label = "1. Variable (Gruppierung/X-Achse):", choices = NULL),
      selectInput(inputId = "var2", label = "2. Variable (Fokus/Farbe):", choices = NULL),
      
      hr(), 
      
      checkboxInput(inputId = "show_share", 
                    label = "Als Anteile (%) anzeigen", 
                    value = TRUE)
    ),
    
    mainPanel(
      h3("Visualisierung"),
      plotOutput("plot_output"),
      
      hr(),
      
      h3("Kreuztabelle"),
      tableOutput("table_output"),
      br(),
      downloadButton(outputId = "download_table", label = "Tabelle als Excel-CSV exportieren")
    )
  )
)


# ==========================================
# 3. SERVER LOGIC
# ==========================================
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    daten = NULL,
    deck = NULL
  )
  
  # Triggered when "Daten verarbeiten" is clicked
  observeEvent(input$btn_ok, {
    
    # REQUIRE BOTH FILES to be uploaded before proceeding
    req(input$data_upload, input$codebook_upload) 
    
    # 1. Load Codebook from the uploaded file
    # (fread automatically handles .csv formats very well)
    cb <- fread(input$codebook_upload$datapath)
    
    # 2. Load and clean the survey data, passing the codebook to it
    rv$daten <- load_and_clean_data(input$data_upload$datapath, cb)
    
    # 3. Create the card deck 
    rv$deck <- create_deck(rv$daten, cb, scales)
    
    # 4. Generate dropdown choices dynamically from the deck
    # This creates a nice format like: "BB01 - Alter" or "CC02 - Beschwerden 2"
    dropdown_choices <- setNames(
      names(rv$deck), 
      sapply(rv$deck, function(card) paste0(card$name, " - ", card$label))
    )
    
    # 5. Update UI Dropdowns
    updateSelectInput(session, "var1", choices = dropdown_choices)
    
    choices_var2 <- c("Keine Auswahl (Univariat)" = "none", dropdown_choices)
    updateSelectInput(session, "var2", choices = choices_var2, selected = "none")
  })
  
  
  # Output: Plot
  output$plot_output <- renderPlot({
    req(rv$deck, input$var1, input$var2) 
    
    karte1 <- rv$deck[[input$var1]]
    karte2 <- if (input$var2 == "none") "none" else rv$deck[[input$var2]]
    
    create_barplot(karte1, karte2, rv$daten, share = input$show_share)
  })
  
  # Reactive Data for Table & Download
  table_data <- reactive({
    req(rv$deck, input$var1, input$var2)
    
    karte1 <- rv$deck[[input$var1]]
    karte2 <- if (input$var2 == "none") "none" else rv$deck[[input$var2]]
    
    create_cross_table(karte1, karte2, rv$daten, share = input$show_share)
  })
  
  # Output: Table Display
  output$table_output <- renderTable({
    table_data()
  }, rownames = TRUE)
  
  # Output: Download Table
  output$download_table <- downloadHandler(
    filename = function() {
      paste0("medicus_auswertung_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv2(table_data(), file, row.names = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)