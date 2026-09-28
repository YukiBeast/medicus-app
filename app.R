library(shiny)
library(data.table)
library(ggplot2)
library(readxl)
library(labelled)
library(dplyr)
library(plotly)
library(bslib)
library(shinycssloaders)


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
  "none_sel"     = c(DE = "Keine Auswahl (Univariat)",             EN = "No Selection (Univariate)"),
  "bar_type"    = c(DE = "Darstellung:",         EN = "Layout:"),
  "opt_dodge"   = c(DE = "Nebeneinander",        EN = "Side-by-Side"),
  "opt_stack"   = c(DE = "Gestapelt",            EN = "Stacked"),
  "fix_y" = c(DE = "Y-Achse auf 0-1 fixieren", EN = "Fix Y-axis to 0-1"),
  "welcome" = c(DE = "Bitte laden Sie Ihre Daten und das Codebook hoch und klicken Sie auf 'Daten verarbeiten'.", 
                EN = "Please upload your data and codebook, then click 'Process Data' to begin."),
  "kpi_n" = c(DE = "Gültige Teilnehmer gesamt", EN = "Vallid Total Respondents")
  )

# ==========================================
# 2. USER INTERFACE (UI)
# ==========================================
ui <- fluidPage(
  theme = bs_theme(bootswatch = "lux",
                   primary = "#66CC99"), # "flatly" and "minty" are also great, clean options
  
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
    tagList(                   # <--- USE TAGLIST INSTEAD
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
          
          actionButton(inputId = "btn_ok", label = tr("btn_ok"), class = "btn-primary",
                       width = "100%",
                       style = "color: white;"),
          
          hr(), 
          
          h4(tr("header_2")),
          selectInput(inputId = "var1", label = tr("var1"), choices = NULL),
          selectInput(inputId = "var2", label = tr("var2"), choices = NULL),
          
          hr(), 
          
          checkboxInput(inputId = "show_share", 
                        label = tr("show_share"), 
                        value = TRUE),
          
           # Appears only when share is checked
          conditionalPanel(
            condition = "input.show_share == true",
            checkboxInput(inputId = "fix_y", 
                          label = tr("fix_y"), 
                          value = FALSE)
          ),
          
          checkboxInput(inputId = "show_legend", 
                        label = tr("show_legend"), 
                        value = TRUE),
          radioButtons(
            inputId = "bar_pos",
            label = tr("bar_type"),
            choiceNames = list(tr("opt_dodge"), tr("opt_stack")),
            choiceValues = list("dodge", "stack"),
            selected = "dodge",
            inline = TRUE
          )
        ),
        
        mainPanel(
          # Placeholder for the KPI boxes
          uiOutput("kpi_boxes"),
          br(),
          
          # Plot Card
          bslib::card(
            bslib::card_header(class = "bg-primary text-white", h4(tr("viz"), style = "margin: 0;")),
            # UI Side
            bslib::card_body(withSpinner(plotlyOutput("plot_output", height = "650px"), type = 8, color = "#20c997"))
          ),
          
          br(),
          
          # Table Card
          bslib::card(
            bslib::card_header(class = "bg-primary text-white", h4(tr("cross_table"), style = "margin: 0;")),
            bslib::card_body(
              tableOutput("table_output"),
              br(),
              downloadButton(outputId = "download_table", label = tr("download_tbl"), class = "btn-outline-primary")
            )
          )
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
  
  output$kpi_boxes <- renderUI({
    req(rv$daten, input$var1, rv$deck)
    
    # 1. Total Active Respondents
    total_n <- nrow(rv$daten)
    
    # Extract data for Variable 1
    karte1 <- rv$deck[[input$var1]]
    dt1 <- extract(karte1, rv$daten)
    
    # 2. Dynamic Valid Responses Calculation
    if (is.null(input$var2) || input$var2 == "none") {
      # Univariate case: Count UNIQUE IDs that have a valid answer
      valid_n <- length(unique(dt1$id[!is.na(dt1$answer)]))
      box_title <- if (input$lang == "EN") "Valid answers to this question:" else "Gültige Antworten auf diese Frage:"
      
    } else {
      # Bivariate case: Merge data and count UNIQUE IDs that answered BOTH
      karte2 <- rv$deck[[input$var2]]
      dt2 <- extract(karte2, rv$daten)
      forplot <- dt1[dt2, on = "id"]
      
      # Filter for rows where both answers are valid, then count unique IDs
      valid_ids <- forplot$id[!is.na(forplot$answer) & !is.na(forplot$i.answer)]
      valid_n <- length(unique(valid_ids))
      box_title <- if (input$lang == "EN") "Valid answers to both questions:" else "Gültige Antworten auf beide Fragen:"
    }
    
    # Format the text to show "N (X%)"
    valid_pct <- round((valid_n / total_n) * 100, 1)
    valid_text <- paste0(valid_n, " (", valid_pct, "%)")
    
    layout_columns(
      bslib::value_box(
        title = tr("kpi_n"), # Assumes "Total Respondents" is in your dict
        value = format(total_n, big.mark = ","),
        showcase = icon("users"),
        theme = "primary"
      ),
      bslib::value_box(
        title = box_title,
        value = h5(valid_text, style = "margin: 0; font-weight: bold;"),
        showcase = icon("chart-pie"), 
        theme = "secondary"
      )
    )
  })
  
  # ---------------------------------------------------------
  # PLOT LOGIC
  # ---------------------------------------------------------
  current_plot <- reactive({
    validate(
      need(rv$deck, tr("welcome"))
    )
    req(rv$deck, input$var1, input$var2) 
    
    karte1 <- rv$deck[[input$var1]]
    karte2 <- if (input$var2 == "none") "none" else rv$deck[[input$var2]]
    
    # Base Plot
    p <- create_barplot(karte1,
                        karte2,
                        rv$daten,
                        share = input$show_share,
                        language = input$lang,
                        bar_pos = input$bar_pos,
                        fix_y = isTRUE(input$fix_y))
    
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
      config(
        modeBarButtons = list(list("toImage")),
        displaylogo = FALSE
      )
  })
  
  # ---------------------------------------------------------
  # TABLE LOGIC
  # ---------------------------------------------------------
  table_data <- reactive({
    validate(
      need(rv$deck, tr("welcome"))
    )
    req(rv$deck, input$var1, input$var2)
    
    karte1 <- rv$deck[[input$var1]]
    karte2 <- if (input$var2 == "none") "none" else rv$deck[[input$var2]]
    
    df <- create_cross_table(karte1, karte2, rv$daten, share = input$show_share,
                       language = input$lang)
    
    # If bivariate, convert row names to a real column with a translated header
    if (input$var2 != "none") {
      col_label <- if (input$lang == "EN") "Category" else "Kategorie"
      df <- data.frame(Category = rownames(df), df, check.names = FALSE)
      names(df)[1] <- col_label
    }
    
    return(df)
  })
  
  output$table_output <- renderTable({
    table_data()
  }, rownames = FALSE, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "c")
  
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