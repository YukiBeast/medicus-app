library(shiny)
library(data.table)
library(ggplot2)
library(readxl)
library(labelled)
library(dplyr)
library(plotly)

# Workaround for Chromium Issue 468227 in Shinylive
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# ==========================================
# 1. GLOBAL SETUP & TRANSLATION DICTIONARY
# ==========================================
source("load_data.R")         
source("assign_labels.R")     
source("create_deck.R")       # Contains 'scales'
source("extract_methods.R")
source("create_cross_table.R")
source("create_barplot.R")

# Dictionary containing all UI text in German (DE) and English (EN)
dict <- list(
  "title"        = c(DE = "MEDICUS: Interaktive Datenexploration", EN = "MEDICUS: Interactive Data Exploration"),
  "header_1"     = c(DE = "1. Daten & Codebook",                   EN = "1. Data & Codebook"),
  "upload_data"  = c(DE = "1. Datendatei hochladen (z.B. Excel):", EN = "1. Upload data file (e.g., Excel):"),
  "upload_cb"    = c(DE = "2. Codebook hochladen (.csv):",         EN = "2. Upload codebook (.csv):"),
  "btn_ok"       = c(DE = "Daten verarbeiten",                     EN = "Process Data"),
  "header_2"     = c(DE = "2. Variablen",                          EN = "2. Variables"),
  "var1"         = c(DE = "1. Variable (Gruppierung/X-Achse):",    EN = "1. Variable (Grouping/X-Axis):"),
  "var2"         = c(DE = "2. Variable (Fokus/Farbe):",            EN = "2. Variable (Focus/Color):"),
  "show_share"   = c(DE = "Als Anteile (%) anzeigen",              EN = "Show as proportions (%)"),
  "show_legend"  = c(DE = "Legende anzeigen",                      EN = "Show legend"),
  "viz"          = c(DE = "Visualisierung",                        EN = "Visualization"),
  "cross_table"  = c(DE = "Kreuztabelle",                          EN = "Cross Table"),
  "download_tbl" = c(DE = "Tabelle als Excel-CSV exportieren",     EN = "Export table as Excel-CSV"),
  "none_sel"     = c(DE = "Keine Auswahl (Univariat)",             EN = "No Selection (Univariate)")
)

# ==========================================
# 2. USER INTERFACE (UI)
# ==========================================
ui <- fluidPage(
  
  # Language Switcher placed at the very top
  br(),
  fluidRow(
    column(12, align = "right",
           radioButtons("lang", label = NULL, 
                        choices = c("EN", "DE"), 
                        selected = "EN", 
                        inline = TRUE)
    )
  ),
  
  # The rest of the UI will be generated dynamically on the server
  uiOutput("app_body")
)

# ==========================================
# 3. SERVER LOGIC
# ==========================================
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    daten = NULL,
    deck = NULL
  )
  
  # Helper function to easily fetch the correct translation based on current language
  tr <- function(text_id) {
    req(input$lang)
    dict[[text_id]][input$lang]
  }
  
  # ---------------------------------------------------------
  # DYNAMIC UI GENERATION (Rebuilds when language changes)
  # ---------------------------------------------------------
  output$app_body <- renderUI({
    
    # We build the layout inside renderUI so we can inject the translated texts
    fluidPage(
      titlePanel(tr("title")),
      
      sidebarLayout(
        sidebarPanel(
          h4(tr("header_1")),
          
          fileInput(inputId = "data_upload", 
                    label = tr("upload_data"), 
                    accept = c(".csv", ".xlsx", ".rds")),
          
          fileInput(inputId = "codebook_upload", 
                    label = tr("upload_cb"), 
                    accept = c(".csv")),
          
          actionButton(inputId = "btn_ok", label = tr("btn_ok"), class = "btn-primary"),
          
          hr(), 
          
          h4(tr("header_2")),
          selectInput(inputId = "var1", label = tr("var1"), choices = NULL),
          selectInput(inputId = "var2", label = tr("var2"), choices = NULL),
          
          hr(), 
          
          checkboxInput(inputId = "show_share", 
                        label = tr("show_share"), 
                        value = TRUE),
          checkboxInput(inputId = "show_legend", 
                        label = tr("show_legend"), 
                        value = TRUE)
        ),
        
        mainPanel(
          h3(tr("viz")),
          plotlyOutput("plot_output", height = "650px"),
          
          hr(),
          
          h3(tr("cross_table")),
          tableOutput("table_output"),
          br(),
          downloadButton(outputId = "download_table", label = tr("download_tbl"))
        )
      )
    )
  })
  
  # ---------------------------------------------------------
  # DATA PROCESSING
  # ---------------------------------------------------------
  observeEvent(input$btn_ok, {
    
    # Require both files before proceeding
    req(input$data_upload, input$codebook_upload) 
    
    # 1. Load Codebook
    cb <- fread(input$codebook_upload$datapath)
    
    # 2. Load and clean the survey data
    rv$daten <- load_and_clean_data(input$data_upload$datapath, cb)
    
    # 3. Create the card deck 
    rv$deck <- create_deck(rv$daten, cb, scales)
    
    # 4. Generate dropdown choices dynamically
    dropdown_choices <- setNames(
      names(rv$deck), 
      sapply(rv$deck, function(card) paste0(card$name, " - ", card$label))
    )
    
    # 5. Update UI Dropdowns
    updateSelectInput(session, "var1", choices = dropdown_choices)
    
    # Use the translated string for the "none" option!
    none_choice <- setNames("none", tr("none_sel"))
    choices_var2 <- c(none_choice, dropdown_choices)
    
    updateSelectInput(session, "var2", choices = choices_var2, selected = "none")
  })
  
  # ---------------------------------------------------------
  # PLOT LOGIC
  # ---------------------------------------------------------
  current_plot <- reactive({
    req(rv$deck, input$var1, input$var2) 
    
    karte1 <- rv$deck[[input$var1]]
    karte2 <- if (input$var2 == "none") "none" else rv$deck[[input$var2]]
    
    # Base Plot
    p <- create_barplot(karte1, karte2, rv$daten,
                        share = input$show_share,
                        language = input$lang)
    
    # Remove legend if checkbox is unticked
    if (!input$show_legend) {
      p <- p + theme(legend.position = "none",
                     plot.margin = margin(t = 10, r = 150, b = 10, l = 10),
                     legend.text = element_text(size = 8),
                     legend.title = element_text(size = 10),  
                     legend.key.size = unit(0.5, "cm"))
    }
    
    return(p)
  })
  
  output$plot_output <- renderPlotly({
    p <- current_plot() 
    ply <- ggplotly(p)
    
    # Replace \n with <br> for HTML rendering
    for (i in seq_along(ply$x$data)) {
      if (!is.null(ply$x$data[[i]]$name)) {
        ply$x$data[[i]]$name <- gsub("\n", "<br>", ply$x$data[[i]]$name)
      }
    }
    
    ply %>%
      layout(
        # 1. Provide a fixed minimum margin just for the legend
        margin = list(b = 120), 
        
        legend = list(
          orientation = "h",
          yref = "container",         # MAGIC 1: Pin legend to the main window
          y = 0,                      # MAGIC 2: Put it at the absolute bottom (0)
          yanchor = "bottom",         # MAGIC 3: Anchor it from its own bottom edge
          x = 0,                    
          xanchor = "left",          
          font = list(size = 12),
          title = list(
            side = "top",             
            font = list(size = 14)
          )
        ),
        xaxis = list(
          fixedrange = TRUE,
          automargin = TRUE           # MAGIC 4: Auto-shrinks the bars if labels are long!
        ),
        yaxis = list(fixedrange = TRUE)
      ) %>%
      config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "pan2d", "zoom2d", "autoScale2d"),
             displaylogo = FALSE)
  })
  
  # ---------------------------------------------------------
  # TABLE LOGIC
  # ---------------------------------------------------------
  table_data <- reactive({
    req(rv$deck, input$var1, input$var2)
    
    karte1 <- rv$deck[[input$var1]]
    karte2 <- if (input$var2 == "none") "none" else rv$deck[[input$var2]]
    
    create_cross_table(karte1, karte2, rv$daten, share = input$show_share)
  })
  
  output$table_output <- renderTable({
    table_data()
  }, rownames = TRUE)
  
  output$download_table <- downloadHandler(
    filename = function() {
      paste0("medicus_table_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv2(table_data(), file, row.names = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)