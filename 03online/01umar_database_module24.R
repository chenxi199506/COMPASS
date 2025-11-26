# External RCT Database Module - Enhanced Version with Dynamic Keyword Search
externalMetaUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Custom CSS Styles
    tags$style(HTML("
      .external-meta-header {
        background: linear-gradient(135deg, #27AE60 0%, #2ECC71 100%);
        color: white;
        padding: 20px;
        border-radius: 15px;
        margin-bottom: 20px;
        box-shadow: 0 4px 15px rgba(39, 174, 96, 0.2);
        position: relative;
        overflow: hidden;
      }
      
      .external-meta-header::before {
        content: '';
        position: absolute;
        top: -50%;
        right: -50%;
        width: 100%;
        height: 100%;
        background: radial-gradient(circle, rgba(255,255,255,0.1) 1px, transparent 1px);
        background-size: 20px 20px;
        transform: rotate(30deg);
      }
      
      .stats-card-external {
        background: white;
        border-radius: 12px;
        padding: 20px;
        margin-bottom: 15px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        border-left: 4px solid #27AE60;
        transition: all 0.3s ease;
        position: relative;
        overflow: hidden;
      }
      
      .stats-card-external::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 3px;
        background: linear-gradient(90deg, #27AE60, #2ECC71);
      }
      
      .stats-card-external:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 25px rgba(0,0,0,0.15);
      }
      
      .stats-card-external:nth-child(2) {
        border-left-color: #3498DB;
      }
      .stats-card-external:nth-child(2)::before {
        background: linear-gradient(90deg, #3498DB, #2980B9);
      }
      
      .stats-card-external:nth-child(3) {
        border-left-color: #E74C3C;
      }
      .stats-card-external:nth-child(3)::before {
        background: linear-gradient(90deg, #E74C3C, #C0392B);
      }
      
      .stats-card-external:nth-child(4) {
        border-left-color: #F39C12;
      }
      .stats-card-external:nth-child(4)::before {
        background: linear-gradient(90deg, #F39C12, #E67E22);
      }
      
      .database-controls {
        background: white;
        border-radius: 15px;
        padding: 25px;
        margin-bottom: 20px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.08);
        border: 1px solid #e8f4f1;
      }
      
      .keyword-search-panel {
        background: linear-gradient(135deg, #2C3E50 0%, #34495E 100%);
        border-radius: 15px;
        padding: 25px;
        margin-bottom: 25px;
        box-shadow: 0 8px 25px rgba(44, 62, 80, 0.4);
        color: white;
        position: relative;
        overflow: hidden;
        border: 1px solid #34495E;
      }
      
      .keyword-search-panel::before {
        content: '';
        position: absolute;
        top: -10px;
        right: -10px;
        width: 100px;
        height: 100px;
        background: rgba(255,255,255,0.05);
        border-radius: 50%;
      }
      
      .keyword-search-panel::after {
        content: '';
        position: absolute;
        bottom: -20px;
        left: -20px;
        width: 80px;
        height: 80px;
        background: rgba(255,255,255,0.03);
        border-radius: 50%;
      }
      
      .keyword-controls {
        background: rgba(0,0,0,0.2);
        border-radius: 10px;
        padding: 20px;
        margin-top: 15px;
        backdrop-filter: blur(10px);
        border: 1px solid rgba(255,255,255,0.1);
      }
      
      .search-controls-panel {
        background: rgba(255,255,255,0.1);
        border-radius: 10px;
        padding: 15px;
        margin-top: 15px;
        backdrop-filter: blur(10px);
      }
      
      .btn-success-custom {
        background: linear-gradient(135deg, #27AE60 0%, #2ECC71 100%);
        border: none;
        border-radius: 12px;
        font-weight: 600;
        padding: 12px 25px;
        transition: all 0.3s ease;
        color: white;
        box-shadow: 0 4px 15px rgba(39, 174, 96, 0.3);
        margin: 5px;
      }
      
      .btn-success-custom:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 25px rgba(39, 174, 96, 0.4);
        color: white;
      }
      
      .btn-primary-custom {
        background: linear-gradient(135deg, #3498DB 0%, #2980B9 100%);
        border: none;
        border-radius: 12px;
        font-weight: 600;
        padding: 12px 25px;
        transition: all 0.3s ease;
        color: white;
        box-shadow: 0 4px 15px rgba(52, 152, 219, 0.3);
        margin: 5px;
      }
      
      .btn-primary-custom:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 25px rgba(52, 152, 219, 0.4);
        color: white;
      }
      
      .btn-warning-custom {
        background: linear-gradient(135deg, #F39C12 0%, #E67E22 100%);
        border: none;
        border-radius: 12px;
        font-weight: 600;
        padding: 12px 25px;
        transition: all 0.3s ease;
        color: white;
        box-shadow: 0 4px 15px rgba(243, 156, 18, 0.3);
        margin: 5px;
      }
      
      .btn-warning-custom:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 25px rgba(243, 156, 18, 0.4);
        color: white;
      }
      
      .btn-default-custom {
        background: linear-gradient(135deg, #95a5a6 0%, #7f8c8d 100%);
        border: none;
        border-radius: 12px;
        font-weight: 600;
        padding: 12px 25px;
        transition: all 0.3s ease;
        color: white;
        box-shadow: 0 4px 15px rgba(149, 165, 166, 0.3);
        margin: 5px;
      }
      
      .btn-default-custom:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 25px rgba(149, 165, 166, 0.4);
        color: white;
      }
      
      .btn-dark-custom {
        background: linear-gradient(135deg, #2C3E50 0%, #34495E 100%);
        border: none;
        border-radius: 12px;
        font-weight: 600;
        padding: 12px 25px;
        transition: all 0.3s ease;
        color: white;
        box-shadow: 0 4px 15px rgba(44, 62, 80, 0.3);
        margin: 5px;
      }
      
      .btn-dark-custom:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 25px rgba(44, 62, 80, 0.4);
        color: white;
        background: linear-gradient(135deg, #34495E 0%, #2C3E50 100%);
      }
      
      .table-container {
        background: white;
        border-radius: 15px;
        padding: 25px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.08);
        margin-bottom: 20px;
        border: 1px solid #f0f0f0;
      }
      
      .database-info {
        background: linear-gradient(135deg, #2C3E50 0%, #3498DB 100%);
        color: white;
        padding: 20px;
        border-radius: 12px;
        margin-bottom: 20px;
        box-shadow: 0 4px 15px rgba(52, 152, 219, 0.2);
      }
      
      .study-details-panel {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 12px;
        padding: 25px;
        margin-top: 20px;
        border-left: 5px solid #3498DB;
        box-shadow: 0 4px 15px rgba(0,0,0,0.05);
      }
      
      .multi-select-container {
        background: white;
        border-radius: 10px;
        padding: 15px;
        margin-bottom: 15px;
        border: 1px solid #e0e0e0;
      }
      
      .download-warning {
        background: linear-gradient(135deg, #FFF3CD 0%, #FFEAA7 100%);
        border: 1px solid #FFEAA7;
        border-radius: 10px;
        padding: 15px;
        margin: 15px 0;
        color: #856404;
        box-shadow: 0 4px 15px rgba(255, 234, 167, 0.3);
      }
      
      .search-tips {
        background: rgba(0,0,0,0.2);
        border-radius: 10px;
        padding: 15px;
        margin-top: 15px;
        font-size: 13px;
        backdrop-filter: blur(10px);
        border: 1px solid rgba(255,255,255,0.1);
      }
      
      .keyword-input-container {
        background: rgba(255,255,255,0.95);
        border-radius: 10px;
        padding: 15px;
        margin: 10px 0;
        border: 2px solid #34495E;
        transition: all 0.3s ease;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      }
      
      .keyword-input-container:hover {
        border-color: #3498DB;
        transform: translateY(-2px);
        box-shadow: 0 4px 15px rgba(52, 152, 219, 0.2);
      }
      
      .filter-section-title {
        color: #2C3E50;
        font-weight: 600;
        margin-bottom: 15px;
        font-size: 16px;
        display: flex;
        align-items: center;
        gap: 8px;
      }
      
      .stats-value {
        font-size: 28px;
        font-weight: 700;
        margin: 10px 0;
        background: linear-gradient(135deg, #2C3E50, #3498DB);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
      }
      
      .selectize-control .selectize-input {
        border-radius: 8px;
        border: 2px solid #e0e0e0;
        padding: 10px;
        transition: all 0.3s ease;
      }
      
      .selectize-control .selectize-input:focus {
        border-color: #3498DB;
        box-shadow: 0 0 0 3px rgba(52, 152, 219, 0.1);
      }
      
      .text-input-custom {
        border-radius: 10px;
        border: 2px solid #bdc3c7;
        padding: 12px 15px;
        font-size: 14px;
        transition: all 0.3s ease;
        width: 100%;
        background: white;
        color: #2C3E50;
      }
      
      .text-input-custom:focus {
        border-color: #3498DB;
        box-shadow: 0 0 0 3px rgba(52, 152, 219, 0.1);
        background: white;
      }
      
      .logic-control {
        background: rgba(0,0,0,0.2);
        border-radius: 8px;
        padding: 15px;
        margin: 10px 0;
        border: 1px solid rgba(255,255,255,0.1);
      }
      
      .keyword-tag {
        display: inline-block;
        background: rgba(255,255,255,0.9);
        padding: 6px 12px;
        margin: 4px;
        border-radius: 20px;
        font-size: 13px;
        font-weight: 500;
        border: 1px solid rgba(255,255,255,0.3);
        color: #2C3E50;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      .dark-section-title {
        color: white;
        font-weight: 600;
        margin-bottom: 15px;
        font-size: 16px;
        display: flex;
        align-items: center;
        gap: 8px;
      }
    ")),
    
    # Add shinyjs support
    shinyjs::useShinyjs(),
    
    fluidPage(
      # Top header and statistics
      div(class = "external-meta-header",
          fluidRow(
            column(8,
                   h2("🌐 Advanced RCT Studies Database", 
                      style = "margin: 0; font-weight: 800; font-size: 2.2rem;"),
                   p("Comprehensive collection of published RCT studies with detailed effect size data", 
                     style = "margin: 10px 0 0 0; opacity: 0.95; font-size: 1.1rem;")
            ),
            column(4, style = "text-align: center;",
                   div(style = "background: rgba(255,255,255,0.15); padding: 20px; border-radius: 12px; backdrop-filter: blur(10px);",
                       uiOutput(ns("database_stats"))
                   )
            )
          )
      ),
      
      # Dynamic Keyword Search Panel with Black Theme
      div(class = "keyword-search-panel",
          fluidRow(
            column(12,
                   h4("🔍 Dynamic Keyword Search", style = "margin-bottom: 15px; color: white; font-weight: 600;"),
                   p("Add multiple keywords and use AND/OR logic for precise search across all fields", 
                     style = "margin: 0; opacity: 0.9; font-size: 14px; color: #ecf0f1;")
            )
          ),
          
          # Keyword input area
          fluidRow(
            column(12,
                   div(class = "keyword-controls",
                       # Dynamic keyword input container
                       div(
                         id = ns("keyword_inputs"),
                         # Initial input boxes will be added dynamically in server
                       ),
                       
                       # Keyword control buttons
                       fluidRow(
                         column(6,
                                actionButton(ns("add_keyword"), 
                                             "➕ Add Keyword", 
                                             class = "btn-dark-custom"),
                                actionButton(ns("remove_keyword"), 
                                             "➖ Remove Keyword", 
                                             class = "btn-warning-custom")
                         ),
                         column(6, style = "text-align: right;",
                                actionButton(ns("apply_keyword_search"), 
                                             "🚀 Apply Search", 
                                             class = "btn-success-custom"),
                                actionButton(ns("clear_keyword_search"), 
                                             "🗑️ Clear Search", 
                                             class = "btn-default-custom")
                         )
                       ),
                       
                       # Search logic selection
                       fluidRow(
                         column(12,
                                div(class = "logic-control",
                                    radioButtons(ns("search_logic"), 
                                                 "🔧 Search Logic:",
                                                 choices = c("AND (All keywords must match)" = "AND", 
                                                             "OR (Any keyword matches)" = "OR"),
                                                 selected = "AND", 
                                                 inline = TRUE)
                                )
                         )
                       )
                   )
            )
          ),
          
          # Search tips
          fluidRow(
            column(12,
                   div(class = "search-tips",
                       fluidRow(
                         column(6,
                                p(strong("💡 Usage Tips:", style = "color: #ecf0f1;"), 
                                  br(),
                                  "• Add multiple keywords for combined search",
                                  br(),
                                  "• AND: All keywords must appear",
                                  br(),
                                  "• OR: Any keyword appears")
                         ),
                         column(6,
                                p(strong("🎯 Search Examples:", style = "color: #ecf0f1;"), 
                                  br(),
                                  "• ", tags$span(class = "keyword-tag", "diabetes"),
                                  tags$span(class = "keyword-tag", "AND"),
                                  tags$span(class = "keyword-tag", "cardiovascular"),
                                  br(),
                                  "• ", tags$span(class = "keyword-tag", "statins"),
                                  tags$span(class = "keyword-tag", "OR"),
                                  tags$span(class = "keyword-tag", "mortality"))
                         )
                       )
                   )
            )
          )
      ),
      
      # Control panel (Advanced Filters)
      div(class = "database-controls",
          fluidRow(
            column(12,
                   h4("🎯 Advanced Filters", style = "margin-bottom: 20px; color: #2C3E50; font-weight: 600;"),
                   p("Use specific filters for precise control over search results", 
                     style = "color: #666; margin-bottom: 25px; font-size: 14px;")
            )
          ),
          fluidRow(
            column(3,
                   div(class = "filter-section-title", "🦠 Disease Filters"),
                   selectizeInput(ns("search_disease"), 
                                  NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(placeholder = 'Select diseases...',
                                                 maxItems = 5))
            ),
            column(3,
                   div(class = "filter-section-title", "🏥 Clinical Department"),
                   selectizeInput(ns("search_department"), 
                                  NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(placeholder = 'Select departments...',
                                                 maxItems = 5))
            ),
            column(3,
                   div(class = "filter-section-title", "📊 Effect Size Type"),
                   selectizeInput(ns("search_effect_type"), 
                                  NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(placeholder = 'Select effect types...',
                                                 maxItems = 5))
            ),
            column(3,
                   div(class = "filter-section-title", "📈 Statistical Significance"),
                   selectizeInput(ns("search_significance"), 
                                  NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(placeholder = 'Select significance...',
                                                 maxItems = 5))
            )
          ),
          fluidRow(
            column(3,
                   div(class = "filter-section-title", "📚 Journal"),
                   selectizeInput(ns("search_journal"), 
                                  NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(placeholder = 'Select journals...',
                                                 maxItems = 5))
            ),
            column(3,
                   div(class = "filter-section-title", "⭐ JIF Quartile"),
                   selectizeInput(ns("search_jif_quartile"), 
                                  NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(placeholder = 'Select JIF quartiles...',
                                                 maxItems = 5))
            ),
            column(3,
                   div(class = "filter-section-title", "📅 Publication Year"),
                   selectizeInput(ns("search_year"), 
                                  NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(placeholder = 'Select years...',
                                                 maxItems = 5))
            ),
            column(3, style = "padding-top: 35px;",
                   actionButton(ns("apply_filters"), 
                                "✅ Apply Filters", 
                                class = "btn-success-custom"),
                   actionButton(ns("reset_filters"), 
                                "🔄 Reset All", 
                                class = "btn-default-custom")
            )
          )
      ),
      
      # Statistics cards row
      fluidRow(
        column(3,
               div(class = "stats-card-external",
                   h5("📚 Selected RCT Studies", style = "color: #2C3E50; margin: 0 0 10px 0; font-weight: 600;"),
                   div(class = "stats-value", textOutput(ns("total_studies"))),
                   p("RCT study records", style = "color: #7f8c8d; margin: 0; font-size: 12px;")
               )
        ),
        column(3,
               div(class = "stats-card-external",
                   h5("🏥 Clinical Departments", style = "color: #2C3E50; margin: 0 0 10px 0; font-weight: 600;"),
                   div(class = "stats-value", textOutput(ns("total_departments"))),
                   p("Unique specialties", style = "color: #7f8c8d; margin: 0; font-size: 12px;")
               )
        ),
        column(3,
               div(class = "stats-card-external",
                   h5("🦠 Disease Areas", style = "color: #2C3E50; margin: 0 0 10px 0; font-weight: 600;"),
                   div(class = "stats-value", textOutput(ns("total_diseases"))),
                   p("Medical conditions", style = "color: #7f8c8d; margin: 0; font-size: 12px;")
               )
        ),
        column(3,
               div(class = "stats-card-external",
                   h5("📊 Significant Results", style = "color: #2C3E50; margin: 0 0 10px 0; font-weight: 600;"),
                   div(class = "stats-value", textOutput(ns("significant_studies"))),
                   p("P < 0.05", style = "color: #7f8c8d; margin: 0; font-size: 12px;")
               )
        )
      ),
      
      # Download warning
      uiOutput(ns("download_warning")),
      
      # Data table
      div(class = "table-container",
          fluidRow(
            column(12,
                   div(class = "database-info",
                       fluidRow(
                         column(6,
                                h4("📋 RCT Studies Overview", style = "margin: 0; font-weight: 600;"),
                                p("Click on any row to view detailed information", 
                                  style = "margin: 5px 0 0 0; opacity: 0.9;")
                         ),
                         column(6, style = "text-align: right;",
                                downloadButton(ns("download_data"), 
                                               "📥 Download Filtered Data",
                                               class = "btn-success-custom",
                                               style = "border-radius: 12px; padding: 10px 25px;")
                         )
                       )
                   ),
                   
                   br(),
                   
                   DTOutput(ns("external_meta_table")),
                   
                   # Selected row details
                   uiOutput(ns("selected_study_info"))
            )
          )
      )
    )
  )
}

# Server部分 - 修复了formatRound列名问题
externalMetaServer <- function(id, meta_db) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Set maximum download rows
    MAX_DOWNLOAD_ROWS <- 10000
    
    # Keyword counter
    keyword_count <- reactiveVal(1)
    
    # Store dynamically created input IDs
    keyword_ids <- reactiveVal(character(0))
    
    # Track search status
    keyword_search_applied <- reactiveVal(FALSE)
    
    # Read and filter data, keep only needed columns
    external_meta_db <- reactive({
      req(meta_db)
      
      # Define columns to keep (根据您的新数据结构)
      required_cols <- c(
        "Literature_Abbreviation", "Disease", "Clinical_Department", 
        "Effect_Size", "Effect_Size_Type", "CI_Lower", "CI_Upper", 
        "P_Value", "Num_Studies", "Total_Sample_Size", "year", "journal",
        "JIF_Quartile", "is_significant", "PMID", "doi", "citation_count",
        "Effect_Direction", "statistical_power", "country_standardized"
      )
      
      # Ensure all required columns exist
      available_cols <- required_cols[required_cols %in% colnames(meta_db)]
      meta_db[, available_cols, drop = FALSE]
    })
    
    # Update selectize input options based on new data structure
    observe({
      req(external_meta_db())
      data <- external_meta_db()
      
      # Update various selectize input options
      updateSelectizeInput(session, "search_disease", 
                           choices = sort(unique(data$Disease)),
                           server = TRUE)
      
      updateSelectizeInput(session, "search_department", 
                           choices = sort(unique(data$Clinical_Department)),
                           server = TRUE)
      
      updateSelectizeInput(session, "search_effect_type", 
                           choices = sort(unique(data$Effect_Size_Type)),
                           server = TRUE)
      
      updateSelectizeInput(session, "search_significance", 
                           choices = sort(unique(data$is_significant)),
                           server = TRUE)
      
      updateSelectizeInput(session, "search_journal", 
                           choices = sort(unique(data$journal)),
                           server = TRUE)
      
      updateSelectizeInput(session, "search_jif_quartile", 
                           choices = sort(unique(data$JIF_Quartile)),
                           server = TRUE)
      
      updateSelectizeInput(session, "search_year", 
                           choices = sort(unique(data$year), decreasing = TRUE),
                           server = TRUE)
    })
    
    # Dynamic keyword input functions
    observeEvent(input$add_keyword, {
      count <- keyword_count()
      if (count < 10) {
        count <- count + 1
        keyword_count(count)
        
        # Generate unique ID
        new_id <- paste0("keyword", count)
        
        # Update ID list
        current_ids <- keyword_ids()
        keyword_ids(c(current_ids, new_id))
        
        # Add new keyword input box
        insertUI(
          selector = paste0("#", ns("keyword_inputs")),
          where = "beforeEnd",
          ui = div(
            id = ns(new_id),
            class = "keyword-input-container",
            textInput(
              ns(paste0("input_", new_id)),
              paste("🔤 Keyword", count),
              value = "",
              placeholder = "Enter search keyword..."
            ) %>% tagAppendAttributes(class = "text-input-custom")
          )
        )
      }
    })
    
    observeEvent(input$remove_keyword, {
      count <- keyword_count()
      if (count > 1) {
        # Get ID to remove
        current_ids <- keyword_ids()
        id_to_remove <- current_ids[length(current_ids)]
        
        # Remove from list
        updated_ids <- current_ids[-length(current_ids)]
        keyword_ids(updated_ids)
        keyword_count(count - 1)
        
        # Remove UI element
        removeUI(
          selector = paste0("#", ns(id_to_remove))
        )
      }
    })
    
    # Clear keyword search
    observeEvent(input$clear_keyword_search, {
      # Reset all keyword input boxes
      current_ids <- keyword_ids()
      for(id in current_ids) {
        input_id <- paste0("input_", id)
        updateTextInput(session, input_id, value = "")
      }
      
      # Reset search status
      keyword_search_applied(FALSE)
    })
    
    # Reset filters
    observeEvent(input$reset_filters, {
      updateSelectizeInput(session, "search_disease", selected = "")
      updateSelectizeInput(session, "search_department", selected = "")
      updateSelectizeInput(session, "search_effect_type", selected = "")
      updateSelectizeInput(session, "search_significance", selected = "")
      updateSelectizeInput(session, "search_journal", selected = "")
      updateSelectizeInput(session, "search_jif_quartile", selected = "")
      updateSelectizeInput(session, "search_year", selected = "")
    })
    
    # Get all keywords
    get_keywords <- reactive({
      current_ids <- keyword_ids()
      keywords <- character(0)
      
      for(id in current_ids) {
        input_id <- paste0("input_", id)
        keyword <- input[[input_id]]
        if (!is.null(keyword) && keyword != "") {
          keywords <- c(keywords, trimws(keyword))
        }
      }
      keywords
    })
    
    # Initialize first keyword input box
    observe({
      # Initialize first input box ID
      initial_id <- "keyword1"
      keyword_ids(initial_id)
      
      # Insert first input box after UI container is ready
      insertUI(
        selector = paste0("#", ns("keyword_inputs")),
        where = "beforeEnd",
        ui = div(
          id = ns(initial_id),
          class = "keyword-input-container",
          textInput(
            ns(paste0("input_", initial_id)),
            "🔤 Keyword 1",
            value = "",
            placeholder = "Enter search keyword..."
          ) %>% tagAppendAttributes(class = "text-input-custom")
        )
      )
    })
    
    # Apply keyword search
    observeEvent(input$apply_keyword_search, {
      keyword_search_applied(TRUE)
    })
    
    # Reactive data filtering - combine keyword search and advanced filters
    filtered_data <- reactive({
      req(external_meta_db())
      
      data <- external_meta_db()
      keywords <- get_keywords()
      
      # First apply keyword search
      if (keyword_search_applied() && length(keywords) > 0) {
        # Search across all columns
        all_columns <- colnames(data)
        
        if (length(keywords) == 1) {
          # Single keyword: search across all columns, return row if any column matches
          matches <- rep(FALSE, nrow(data))
          for (col in all_columns) {
            column_data <- as.character(data[[col]])
            col_matches <- grepl(keywords[1], column_data, ignore.case = TRUE)
            matches <- matches | col_matches
          }
          data <- data[matches, ]
        } else if (input$search_logic == "AND") {
          # AND logic: all keywords must appear (can be in different columns)
          combined_matches <- rep(TRUE, nrow(data))
          for (keyword in keywords) {
            keyword_matches <- rep(FALSE, nrow(data))
            for (col in all_columns) {
              column_data <- as.character(data[[col]])
              col_matches <- grepl(keyword, column_data, ignore.case = TRUE)
              keyword_matches <- keyword_matches | col_matches
            }
            combined_matches <- combined_matches & keyword_matches
          }
          data <- data[combined_matches, ]
        } else {
          # OR logic: return row if any keyword appears in any column
          combined_matches <- rep(FALSE, nrow(data))
          for (keyword in keywords) {
            for (col in all_columns) {
              column_data <- as.character(data[[col]])
              col_matches <- grepl(keyword, column_data, ignore.case = TRUE)
              combined_matches <- combined_matches | col_matches
            }
          }
          data <- data[combined_matches, ]
        }
      }
      
      # Then apply advanced filters
      if (length(input$search_disease) > 0) {
        data <- data %>% filter(Disease %in% input$search_disease)
      }
      
      if (length(input$search_department) > 0) {
        data <- data %>% filter(Clinical_Department %in% input$search_department)
      }
      
      if (length(input$search_effect_type) > 0) {
        data <- data %>% filter(Effect_Size_Type %in% input$search_effect_type)
      }
      
      if (length(input$search_significance) > 0) {
        data <- data %>% filter(is_significant %in% input$search_significance)
      }
      
      if (length(input$search_journal) > 0) {
        data <- data %>% filter(journal %in% input$search_journal)
      }
      
      if (length(input$search_jif_quartile) > 0) {
        data <- data %>% filter(JIF_Quartile %in% input$search_jif_quartile)
      }
      
      if (length(input$search_year) > 0) {
        data <- data %>% filter(year %in% input$search_year)
      }
      
      data
    }) %>% bindEvent(input$apply_keyword_search, input$apply_filters)
    
    # Database statistics - 更新总研究数量为 490,430
    output$database_stats <- renderUI({
      req(external_meta_db())
      data <- external_meta_db()
      
      tagList(
        h4("Database Info", style = "margin: 0 0 5px 0;"),
        p(paste("Last updated:", format(Sys.Date(), "%B %d, %Y")), 
          style = "margin: 0; font-size: 12px;"),
        p(paste("490,430", "RCT studies"), 
          style = "margin: 0; font-size: 12px;")
      )
    })
    
    # Download warning
    output$download_warning <- renderUI({
      req(filtered_data())
      
      row_count <- nrow(filtered_data())
      
      if (row_count > MAX_DOWNLOAD_ROWS) {
        div(
          class = "download-warning",
          icon("exclamation-triangle"),
          strong("Download Limit Exceeded"),
          p(paste("The current filter results in", format(row_count, big.mark = ","), 
                  "records, which exceeds the maximum download limit of", 
                  format(MAX_DOWNLOAD_ROWS, big.mark = ","), "records.")),
          p("Please refine your search criteria to reduce the number of records before downloading.")
        )
      }
    })
    
    # Statistics cards data - 更新总研究数量显示
    output$total_studies <- renderText({
      req(filtered_data())
      if (nrow(filtered_data()) > 0) {
        format(nrow(filtered_data()), big.mark = ",")
      } else {
        "490,430"  # 显示总数据库大小
      }
    })
    
    output$total_departments <- renderText({
      req(filtered_data())
      length(unique(filtered_data()$Clinical_Department))
    })
    
    output$total_diseases <- renderText({
      req(filtered_data())
      length(unique(filtered_data()$Disease))
    })
    
    output$significant_studies <- renderText({
      req(filtered_data())
      if ("is_significant" %in% colnames(filtered_data())) {
        sig_count <- sum(filtered_data()$is_significant == TRUE, na.rm = TRUE)
        format(sig_count, big.mark = ",")
      } else {
        "N/A"
      }
    })
    
    # Data table with new columns - 修复了formatRound问题
    output$external_meta_table <- renderDT({
      req(filtered_data())
      
      # Select columns to display based on new data structure
      display_data <- filtered_data() %>%
        select(
          Literature_Abbreviation, Disease, Clinical_Department, 
          Effect_Size, Effect_Size_Type, CI_Lower, CI_Upper, P_Value,
          Num_Studies, Total_Sample_Size, year, journal, JIF_Quartile,
          is_significant, PMID, doi, citation_count
        )
      
      # 创建数据表格
      dt <- datatable(
        display_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel'),
          autoWidth = TRUE,
          columnDefs = list(
            list(targets = 0, width = '120px'),  # Literature_Abbreviation
            list(targets = 1, width = '150px'),  # Disease
            list(targets = 2, width = '120px'),  # Clinical_Department
            list(targets = 3, width = '100px'),  # Effect_Size
            list(targets = 4, width = '100px'),  # Effect_Size_Type
            list(targets = 5:6, width = '80px'), # CI_Lower, CI_Upper
            list(targets = 7, width = '80px'),   # P_Value
            list(targets = 8:9, width = '80px'), # Num_Studies, Total_Sample_Size
            list(targets = 10, width = '70px'),  # year
            list(targets = 11, width = '150px'), # journal
            list(targets = 12, width = '80px'),  # JIF_Quartile
            list(targets = 13, width = '80px'),  # is_significant
            list(targets = 14, width = '100px'), # PMID
            list(targets = 15, width = '150px'), # doi
            list(targets = 16, width = '80px')   # citation_count
          )
        ),
        selection = 'single',
        rownames = FALSE,
        class = 'cell-border stripe hover',
        filter = 'top',
        extensions = 'Buttons',
        colnames = c(
          'Study', 'Disease', 'Department', 'Effect Size', 'Effect Type', 
          'CI Lower', 'CI Upper', 'P Value', 'Num Studies', 'Sample Size',
          'Year', 'Journal', 'JIF Quartile', 'Significant', 'PMID', 'DOI', 'Citations'
        )
      )
      
      # 使用正确的列索引进行格式化（从0开始计数）
      dt <- dt %>%
        formatRound(columns = c(3, 5, 6, 7), digits = 3)  # Effect_Size, CI_Lower, CI_Upper, P_Value
      
      return(dt)
    })
    
    # Selected row details with new data structure
    output$selected_study_info <- renderUI({
      req(input$external_meta_table_rows_selected)
      
      selected_row <- input$external_meta_table_rows_selected
      data <- filtered_data()
      
      if (length(selected_row) > 0) {
        study <- data[selected_row, ]
        
        div(
          class = "study-details-panel",
          h4("📖 RCT Study Details"),
          fluidRow(
            column(6,
                   p(strong("Study: "), ifelse(!is.null(study$Literature_Abbreviation), study$Literature_Abbreviation, "N/A")),
                   p(strong("Disease: "), ifelse(!is.null(study$Disease), study$Disease, "N/A")),
                   p(strong("Clinical Department: "), ifelse(!is.null(study$Clinical_Department), study$Clinical_Department, "N/A")),
                   p(strong("Effect Size: "), ifelse(!is.null(study$Effect_Size), paste(study$Effect_Size, study$Effect_Size_Type), "N/A")),
                   p(strong("95% CI: "), ifelse(!is.null(study$CI_Lower) && !is.null(study$CI_Upper), 
                                                paste("[", study$CI_Lower, ", ", study$CI_Upper, "]", sep = ""), "N/A")),
                   p(strong("P Value: "), ifelse(!is.null(study$P_Value), study$P_Value, "N/A"))
            ),
            column(6,
                   p(strong("Number of Studies: "), ifelse(!is.null(study$Num_Studies), study$Num_Studies, "N/A")),
                   p(strong("Total Sample Size: "), ifelse(!is.null(study$Total_Sample_Size), format(study$Total_Sample_Size, big.mark = ","), "N/A")),
                   p(strong("Journal: "), ifelse(!is.null(study$journal), study$journal, "N/A")),
                   p(strong("Year: "), ifelse(!is.null(study$year), study$year, "N/A")),
                   p(strong("JIF Quartile: "), ifelse(!is.null(study$JIF_Quartile), study$JIF_Quartile, "N/A")),
                   p(strong("Significant: "), ifelse(!is.null(study$is_significant), 
                                                     ifelse(study$is_significant, "Yes", "No"), "N/A")),
                   p(strong("DOI: "), ifelse(!is.null(study$doi), study$doi, "N/A"))
            )
          )
        )
      }
    })
    
    # Download data (with row count limit check)
    output$download_data <- downloadHandler(
      filename = function() {
        paste("external_rct_database_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- filtered_data()
        row_count <- nrow(data)
        
        # Check if data count exceeds limit
        if (row_count > MAX_DOWNLOAD_ROWS) {
          # Show error message
          showModal(modalDialog(
            title = icon("exclamation-triangle") %>% span(" Download Limit Exceeded", style = "margin-left: 10px;"),
            tagList(
              p(style = "font-size: 16px;", 
                strong("Unable to download data due to excessive record count.")),
              p(style = "font-size: 14px;",
                "Current filter results: ", format(row_count, big.mark = ","), " records"),
              p(style = "font-size: 14px;",
                "Maximum allowed: ", format(MAX_DOWNLOAD_ROWS, big.mark = ","), " records"),
              hr(),
              p(style = "font-size: 14px; color: #666;",
                "Please refine your search criteria to reduce the number of records,",
                "or contact the system administrator if you need to download larger datasets.")
            ),
            footer = modalButton("OK"),
            easyClose = TRUE,
            size = "m"
          ))
          
          # Stop download
          return()
        }
        
        # If data volume is within limit, download normally
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    # Return filtered data for other modules to use
    return(filtered_data)
  })
}

# Test application
# ui <- fluidPage(
#   externalMetaUI("test")
# )
# 
# server <- function(input, output, session) {
#   # Create sample data matching the new structure
#   sample_data <- data.frame(
#     Literature_Abbreviation = c("Study_A", "Study_B", "Study_C", "Study_D", "Study_E"),
#     Disease = c("Diabetes", "Cardiovascular Disease", "Diabetes Complications", "Hypertension", "Cancer"),
#     Clinical_Department = c("Endocrinology", "Cardiology", "Endocrinology", "Cardiology", "Oncology"),
#     Effect_Size = c(0.75, 1.25, 0.89, 1.10, 0.65),
#     Effect_Size_Type = c("OR", "RR", "HR", "OR", "RR"),
#     CI_Lower = c(0.65, 1.10, 0.78, 0.95, 0.55),
#     CI_Upper = c(0.85, 1.40, 1.00, 1.25, 0.75),
#     P_Value = c(0.001, 0.045, 0.230, 0.012, 0.003),
#     Num_Studies = c(15, 8, 12, 6, 20),
#     Total_Sample_Size = c(10000, 2500, 8000, 1200, 30000),
#     year = c(2020, 2021, 2019, 2022, 2020),
#     journal = c("Journal A", "Journal B", "Journal C", "Journal D", "Journal E"),
#     JIF_Quartile = c("Q1", "Q2", "Q1", "Q3", "Q1"),
#     is_significant = c(TRUE, TRUE, FALSE, TRUE, TRUE),
#     PMID = c("12345678", "23456789", "34567890", "45678901", "56789012"),
#     doi = c("10.1234/studyA", "10.1234/studyB", "10.1234/studyC", "10.1234/studyD", "10.1234/studyE"),
#     citation_count = c(45, 23, 12, 8, 67),
#     Effect_Direction = c("Beneficial", "Harmful", "Beneficial", "Harmful", "Beneficial"),
#     statistical_power = c(0.85, 0.72, 0.68, 0.79, 0.91),
#     country_standardized = c("USA", "China", "UK", "Germany", "USA"),
#     stringsAsFactors = FALSE
#   )
#   
#   externalMetaServer("test", sample_data)
# }
# 
# shinyApp(ui, server)