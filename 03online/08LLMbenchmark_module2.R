# Create Enhanced Model Comparison Plot Function
create_enhanced_model_plot <- function(data, column_info, row_info, palettes) {
  
  # Set dimension parameters
  row_height <- 1.1
  row_space <- 0.1
  col_width <- 1.1
  col_space <- 0.2
  col_bigspace <- 0.5
  
  # Calculate row positions
  row_pos <- row_info %>%
    mutate(
      row_i = row_number(),
      colour_background = row_i %% 2 == 1,
      y = -row_i * (row_height + row_space),
      ymin = y - row_height / 2,
      ymax = y + row_height / 2
    )
  
  # Calculate column positions
  column_pos <- column_info %>%
    mutate(
      x = cumsum(c(0, head(width, -1)) + width/2 + c(0, rep(col_space, n()-1))),
      xmin = x - width/2,
      xmax = x + width/2
    )
  
  # Define metric groups and color mapping
  metric_groups <- list(
    group1 = list(
      circle_metrics = c("avg_response_time"),
      bar_metrics = c("match_success_rate"),
      color_palette = "Blues",
      group_name = "Matching\nPerformance"
    ),
    group2 = list(
      circle_metrics = c("studies_accuracy", "sample_accuracy"),
      bar_metrics = c("extract_accuracy"),
      color_palette = "Greens", 
      group_name = "Study\nAccuracy"
    ),
    group3 = list(
      circle_metrics = c("amstar_non_critical_accuracy", "amstar_critical_accuracy"),
      bar_metrics = c("amstar_accuracy"),
      color_palette = "Purples",
      group_name = "AMSTAR2\nAssessment"
    ),
    group4 = list(
      circle_metrics = c(),
      bar_metrics = c("overall_score"),
      color_palette = "RdYlBu",
      group_name = "Overall\nScore"
    )
  )
  
  # Create circle data
  circle_data_list <- list()
  for(group_name in names(metric_groups)) {
    group <- metric_groups[[group_name]]
    palette_name <- group$color_palette
    
    for(metric in group$circle_metrics) {
      values <- data[[metric]]
      # Calculate circle sizes
      r_values <- row_height/2 * sqrt(values)
      r_values <- rescale(r_values, to = c(0.05, 0.55))
      
      # Use group-specific color mapping
      palette_colors <- colorRampPalette(rev(brewer.pal(9, palette_name)))(length(values))
      colors <- palette_colors[rank(values, ties.method = "average", na.last = "keep")]
      
      circle_data_list[[paste(metric, group_name)]] <- data.frame(
        model_name = data$model_name,
        metric = metric,
        group = group_name,
        x0 = column_pos$x[column_pos$id == metric],
        y0 = row_pos$y[match(data$model_name, row_pos$id)],
        r = r_values,
        colors = colors,
        stringsAsFactors = FALSE
      ) %>% as_tibble()
    }
  }
  circle_data <- bind_rows(circle_data_list)
  
  # Create bar data
  rect_data_list <- list()
  for(group_name in names(metric_groups)) {
    group <- metric_groups[[group_name]]
    palette_name <- group$color_palette
    
    for(metric in group$bar_metrics) {
      values <- data[[metric]]
      palette_colors <- colorRampPalette(rev(brewer.pal(9, palette_name)))(length(values))
      bar_colors <- palette_colors[rank(values, ties.method = "average", na.last = "keep")]
      
      rect_data_list[[metric]] <- data.frame(
        model_name = data$model_name,
        metric = metric,
        group = group_name,
        value = values,
        xmin = column_pos$xmin[column_pos$id == metric],
        xmax = column_pos$xmax[column_pos$id == metric],
        ymin = row_pos$ymin[match(data$model_name, row_pos$id)] + 0.1,
        ymax = row_pos$ymax[match(data$model_name, row_pos$id)] - 0.1,
        colors = bar_colors,
        stringsAsFactors = FALSE
      ) %>% as_tibble()
    }
  }
  rect_data <- bind_rows(rect_data_list)
  
  # Create basic info text data
  text_metrics <- c("Developer", "Release_Year", "Total_Parameters_B", "Context_Length_K_tokens")
  
  text_data_list <- list()
  
  for(metric in text_metrics) {
    values <- data[[metric]]
    text_data_list[[metric]] <- data.frame(
      model_name = data$model_name,
      metric = metric,
      x = column_pos$x[column_pos$id == metric],
      y = row_pos$y[match(data$model_name, row_pos$id)],
      label = as.character(values),
      hjust = 0.5,
      vjust = 0.5,
      fontface = "plain",
      size = 3,
      color = "black",
      stringsAsFactors = FALSE
    ) %>% as_tibble()
  }
  text_data <- bind_rows(text_data_list)
  
  # Model name text
  model_text_data <- data.frame(
    x = column_pos$xmin[column_pos$id == "model_name"] + 0.3,
    y = row_pos$y,
    label = data$model_name,
    hjust = 0,
    fontface = "bold",
    size = 3.5,
    color = "black",
    stringsAsFactors = FALSE
  ) %>% as_tibble()
  
  # Add ranking text
  rank_data <- data.frame(
    x = column_pos$xmin[column_pos$id == "model_name"] - 0.5,
    y = row_pos$y,
    label = 1:nrow(data),
    hjust = 0.5,
    fontface = "bold",
    size = 4,
    color = "black",
    stringsAsFactors = FALSE
  ) %>% as_tibble()
  
  # Column titles with automatic line breaks
  column_text <- column_pos %>%
    filter(id != "model_name") %>%
    mutate(
      y = max(row_pos$ymax) + 1.2,
      label = case_when(
        id == "avg_response_time" ~ "Response\nTime (s)",
        id == "match_success_rate" ~ "Match\nSuccess Rate",
        id == "studies_accuracy" ~ "Studies\nAccuracy",
        id == "sample_accuracy" ~ "Sample\nAccuracy",
        id == "extract_accuracy" ~ "Extraction\nAccuracy",
        id == "amstar_non_critical_accuracy" ~ "AMSTAR2\nNon-critical",
        id == "amstar_critical_accuracy" ~ "AMSTAR2\nCritical",
        id == "amstar_accuracy" ~ "AMSTAR2\nAccuracy",
        id == "overall_score" ~ "Overall\nScore",
        id == "Total_Parameters_B" ~ "Parameters\n(B)",
        id == "Context_Length_K_tokens" ~ "Context\nLength (K)",
        id == "Release_Year" ~ "Release\nYear",
        id == "Developer" ~ "Developer",
        TRUE ~ name
      ),
      angle = 0,
      hjust = 0.5,
      vjust = 0.5,
      fontface = "bold",
      size = 3.0,  # Slightly smaller for multi-line text
      color = "black"
    )
  
  # Add group headers
  group_header_data <- data.frame()
  for(group_name in names(metric_groups)) {
    group <- metric_groups[[group_name]]
    group_metrics <- c(group$circle_metrics, group$bar_metrics)
    
    if(length(group_metrics) > 0) {
      group_xmin <- min(column_pos$xmin[column_pos$id %in% group_metrics])
      group_xmax <- max(column_pos$xmax[column_pos$id %in% group_metrics])
      
      group_header <- data.frame(
        x = (group_xmin + group_xmax) / 2,
        y = max(row_pos$ymax) + 2.0,
        label = group$group_name,
        hjust = 0.5,
        vjust = 0.5,
        fontface = "bold",
        size = 3.5,
        color = "black"
      )
      group_header_data <- bind_rows(group_header_data, group_header)
    }
  }
  
  # Tick marks below column titles
  segment_data <- column_pos %>%
    filter(id != "model_name") %>%
    mutate(
      y = max(row_pos$ymax) + 0.8,
      yend = max(row_pos$ymax) + 1.0,
      color = "black",
      size = 0.5
    )
  
  # Create the plot
  g <- ggplot() +
    # Background stripes
    geom_rect(
      data = row_pos %>% filter(colour_background),
      aes(xmin = min(column_pos$xmin) - 0.3, xmax = max(column_pos$xmax) + 0.3,
          ymin = ymin, ymax = ymax),
      fill = "#DDDDDD", alpha = 0.8
    ) +
    
    # Bar charts
    geom_rect(
      data = rect_data,
      aes(xmin = xmin, xmax = xmin + value * (xmax - xmin), 
          ymin = ymin, ymax = ymax, fill = colors),
      color = "black", size = 0.25
    ) +
    
    # Circle charts
    ggforce::geom_circle(
      data = circle_data,
      aes(x0 = x0, y0 = y0, r = r, fill = colors),
      color = "black", size = 0.25
    ) +
    
    # Basic info text
    geom_text(
      data = text_data,
      aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust),
      fontface = "plain", size = 3, color = "black"
    ) +
    
    # Ranking numbers
    geom_text(
      data = rank_data,
      aes(x = x, y = y, label = label, hjust = hjust),
      fontface = "bold", size = 4, color = "black"
    ) +
    
    # Model names
    geom_text(
      data = model_text_data,
      aes(x = x, y = y, label = label, hjust = hjust),
      fontface = "bold", size = 3.5, color = "black"
    ) +
    
    # Column titles
    geom_text(
      data = column_text,
      aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust),
      fontface = "bold", size = 3.0, color = "black", lineheight = 0.8
    ) +
    
    # Group headers
    geom_text(
      data = group_header_data,
      aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust),
      fontface = "bold", size = 3.5, color = "black", lineheight = 0.8
    ) +
    
    # Tick marks
    geom_segment(
      data = segment_data,
      aes(x = x, xend = x, y = y, yend = yend),
      size = 0.5, color = "black"
    ) +
    
    # Use identity scales
    scale_fill_identity() +
    scale_color_identity() +
    scale_size_identity() +
    
    # Theme settings
    theme_void() +
    theme(
      legend.position = "none",
      plot.margin = margin(1, 1, 2, 1, "cm"),
      panel.background = element_rect(fill = "white", color = NA)
    ) +
    
    # Coordinate limits
    coord_equal(
      xlim = c(min(column_pos$xmin) - 1, max(column_pos$xmax) + 0.5),
      ylim = c(min(row_pos$ymin) - 0.5, max(row_pos$ymax) + 3.0)
    )
  
  return(g)
}

# LLM Benchmarking UI
llmBenchmarkingUI <- function(id) {
  ns <- NS(id)
  tagList(
    # 自定义CSS样式 - 与部门分析模块保持一致
    tags$style(HTML("
      .benchmark-card {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border-radius: 15px;
        padding: 20px;
        margin-bottom: 20px;
        color: white;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        border: none;
      }
      
      .stats-card {
        background: white;
        border-radius: 12px;
        padding: 15px;
        margin-bottom: 15px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.08);
        border-left: 4px solid #3498DB;
      }
      
      .sidebar-panel {
        background: #f8f9fa;
        border-radius: 12px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.05);
      }
      
      .plot-container {
        background: white;
        border-radius: 12px;
        padding: 20px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.08);
        margin-bottom: 20px;
      }
      
      .btn-primary {
        background: linear-gradient(45deg, #3498DB, #2C3E50);
        border: none;
        border-radius: 25px;
        font-weight: 600;
        padding: 10px 25px;
        transition: all 0.3s ease;
      }
      
      .btn-primary:hover {
        transform: translateY(-2px);
        box-shadow:   4px 15px rgba(52, 152, 219, 0.3);
      }
      
      .nav-tabs > li > a {
        border-radius: 8px 8px 0 0;
        font-weight: 600;
        color: #2C3E50;
        transition: all 0.3s ease;
      }
      
      .nav-tabs > li.active > a {
        background: linear-gradient(45deg, #3498DB, #2C3E50);
        color: white;
        border: none;
      }
      
      .selectize-control .selectize-input {
        border-radius: 8px;
        border: 2px solid #e9ecef;
        transition: border-color 0.3s ease;
      }
      
      .selectize-control .selectize-input:focus {
        border-color: #3498DB;
        box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.25);
      }
      
      .benchmark-header {
        background: linear-gradient(135deg, #2C3E50 0%, #3498DB 100%);
        color: white;
        padding: 15px 20px;
        border-radius: 10px;
        margin-bottom: 20px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
      }
      
      .data-table-container {
        background: white;
        border-radius: 12px;
        padding: 20px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.08);
        margin-top: 20px;
      }
    ")),
    
    div(style = "padding: 20px;",
        # 顶部标题卡片 - 与部门分析模块风格一致
        div(class = "benchmark-header",
            h2("🤖 LLM Benchmarking for Meta-Analysis", style = "margin: 0; font-weight: 700;"),
            p("Comparative evaluation of large language models on meta-analysis tasks", 
              style = "margin: 5px 0 0 0; opacity: 0.9;")
        ),
        
        # Control Panel
        fluidRow(
          box(
            title = "⚙️ Benchmark Settings",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            fluidRow(
              column(3,
                     selectInput(ns("sort_by"), "Sort by:",
                                 choices = c("Overall Score" = "overall_score",
                                             "Match Success Rate" = "match_success_rate",
                                             "Response Time" = "avg_response_time",
                                             "AMSTAR Accuracy" = "amstar_accuracy"),
                                 selected = "overall_score")
              ),
              column(3,
                     selectInput(ns("sort_order"), "Sort order:",
                                 choices = c("Descending" = "desc", "Ascending" = "asc"),
                                 selected = "desc")
              ),
              column(3,
                     numericInput(ns("top_n"), "Show top N models:",
                                  value = 10, min = 1, max = 20, step = 1)
              ),
              column(3,
                     actionButton(ns("refresh_btn"), "🔄 Refresh Data", 
                                  icon = icon("refresh"),
                                  class = "btn-primary",
                                  style = "margin-top: 25px;")
              )
            )
          )
        ),
        
        # Visualization
        fluidRow(
          box(
            title = "📊 Model Performance Comparison",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotOutput(ns("benchmark_plot"), height = "700px")
          )
        ),
        
        # Data Table - 默认展开，与部门分析模块保持一致
        fluidRow(
          box(
            title = "📋 Detailed Benchmark Results",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,  # 修改为默认展开
            div(class = "data-table-container",
                DTOutput(ns("benchmark_table"))
            )
          )
        )
    )
  )
}


# LLM Benchmarking Server
llmBenchmarkingServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Use actual data with proper handling of TBA values
    load_llm_benchmark_data <- function() {
      data <- tibble::tribble(
        ~model_name, ~Developer, ~Release_Year, ~Total_Parameters_B, ~Context_Length_K_tokens, ~avg_response_time, ~match_success_rate, ~studies_accuracy, ~sample_accuracy, ~extract_accuracy, ~amstar_non_critical_accuracy, ~amstar_critical_accuracy, ~amstar_accuracy, ~overall_score,
        "Claude-3.5-Sonnet", "Anthropic", 2024, "175", 200, 0.07, 0.931034483, 0.574074074, 0.518518519, 0.546296297, 0.646090535, 0.587301587, 0.62037037, 0.625172819,
        "Deepseek-R1", "DeepSeek", 2025, "8", 131, 0.83, 0.839285714, 0.70212766, 0.638297872, 0.670212766, 0.63356974, 0.632218845, 0.632978723, 0.664686761,
        "Deepseekv3.1", "DeepSeek", 2025, "685", 128, 0.77, 0.946428571, 0.660377358, 0.641509434, 0.650943396, 0.765199161, 0.681940701, 0.728773585, 0.721466008,
        "Gemini-2.0-Flash", "Google", 2025, "TBA", 1000, 0.71, 0.844827586, 0.775510204, 0.693877551, 0.734693878, 0.700680272, 0.597667638, 0.655612245, 0.69117615,
        "Gemma-2-9b", "Google", 2024, "9", 8, 0.55, 0.879310345, 0.529411765, 0.31372549, 0.421568628, 0.586056645, 0.464985994, 0.533088235, 0.525930957,
        "Gemma-3-4b", "Google", 2025, "4", 128, 0.76, 0.724137931, 0.595238095, 0.476190476, 0.535714286, 0.412698413, 0.496598639, 0.449404762, 0.508539077,
        "Gemma-3n-E4b", "Google", 2025, "4", 32, 0.75, 0.879310345, 0.137254902, 0.058823529, 0.098039216, 0.361655773, 0.456582633, 0.403186275, 0.365780785,
        "Glm-4.5-Air", "Zhipu AI", 2025, "12", 131, 0.75, 0.879310345, 0.31372549, 0.31372549, 0.31372549, 0.459694989, 0.476190476, 0.466911765, 0.463329805,
        "GPT-4o-Mini", "OpenAI", 2024, "TBA", 128, 0.57, 0.892857143, 0.66, 0.58, 0.62, 0.708888889, 0.525714286, 0.62875, 0.63994246,
        "GPT-4o", "OpenAI", 2024, "TBA", 128, 0.71, 0.946428571, 0.547169811, 0.509433962, 0.528301887, 0.706498952, 0.60916442, 0.663915094, 0.644790731,
        "GPT-Oss-20b", "OpenAI", 2025, "20", 131, 0.59, 0.946428571, 0.566037736, 0.566037736, 0.566037736, 0.672955975, 0.665768194, 0.669811321, 0.66584681,
        "Grok-4-Fast", "xAI", 2025, "TBA", 2000, 0.81, 0.946428571, 0.660377358, 0.641509434, 0.650943396, 0.719077568, 0.660377358, 0.693396226, 0.701927972,
        "Hermes-2-Pro", "Nous Research", 2024, "8", 8, 0.65, 0.948275862, 0.018181818, 0.018181818, 0.018181818, 0.315151515, 0.472727273, 0.384090909, 0.34157001,
        "Hunyuan-A13b", "Tencent", 2025, "13", 256, 0.78, 0.844827586, 0.448979592, 0.387755102, 0.418367347, 0.585034014, 0.56851312, 0.577806122, 0.555540825,
        "Lfm-3b", "Liquid AI", 2024, "3", 32, 0.6, 0.913793103, 0.037735849, 0.018867925, 0.028301887, 0.322851153, 0.485175202, 0.393867925, 0.347350409,
        "Lfm-7b", "Liquid AI", 2024, "7", 32, 0.63, 0.931034483, 0.111111111, 0.092592593, 0.101851852, 0.341563786, 0.473544974, 0.399305556, 0.372316044,
        "Llama-3.2-3b", "Meta", 2024, "3", 128, 0.79, 0.931034483, 0.018518519, 0.018518519, 0.018518519, 0.325102881, 0.481481481, 0.393518519, 0.345521144,
        "Llama-3.3-70b", "Meta", 2024, "70", 128, 0.01, 0.944444444, 0.647058824, 0.607843137, 0.627450981, 0.71459695, 0.610644258, 0.669117647, 0.677003579,
        "Longcat-Flash-Chat", "Meituan", 2025, "560", 128, 0.52, 0.948275862, 0.2, 0.181818182, 0.190909091, 0.452525253, 0.511688312, 0.478409091, 0.443213228,
        "Mistral-7b-Instruct", "Mistral AI", 2023, "7", 32, 0.53, 0.913793103, 0.358490566, 0.301886792, 0.330188679, 0.475890985, 0.528301887, 0.498820755, 0.493331616,
        "Mistral-Nemo", "Mistral AI", 2024, "12", 128, 0.4, 0.896551724, 0.211538462, 0.173076923, 0.192307693, 0.395299145, 0.453296703, 0.420673077, 0.403738658,
        "Mixtral-8x22b", "Mistral AI", 2024, "141", 64, 0.55, 0.862068966, 0.42, 0.38, 0.4, 0.466666667, 0.457142857, 0.4625, 0.483052135,
        "Qwen-2.5-72b", "Alibaba", 2024, "72", 128, 0.75, 0.928571429, 0.653846154, 0.615384615, 0.634615385, 0.722222222, 0.645604396, 0.688701923, 0.691195437,
        "Qwen3-235b", "Alibaba", 2025, "235", 32, 0.72, 0.946428571, 0.679245283, 0.622641509, 0.650943396, 0.77148847, 0.687331536, 0.734669811, 0.724941974,
        "Tongyi-30b", "Alibaba", 2025, "30", 131, 0.79, 0.896551724, 0.634615385, 0.557692308, 0.596153847, 0.557692308, 0.549450549, 0.554086538, 0.600386628,
        "LEADS-Mistral-7b", "Wang et.al", 2025, "7", 32, 0.53, 0.913793103, 0.5745098, 0.5960784, 0.5852941, 0.5362745, 0.5489636, 0.54261905, 0.5675163,
        "ACCORD", "This study", 2025, "-", "-", 4.34, 0.946428571, 0.82551, 0.81232, 0.818915, 0.84874, 0.773432, 0.811086, 0.81192,
        "GPT-5", "OpenAI", 2024, "TBA", 128, 0.79, 0.93220339, 0.654545455, 0.618181818, 0.636363636, 0.768181818, 0.732467532, 0.795959596, 0.740673441
      )
      
      # 修复：正确处理数值列转换
      # 确保数值列是正确类型
      numeric_cols <- c("avg_response_time", "match_success_rate", "studies_accuracy", 
                        "sample_accuracy", "extract_accuracy", "amstar_non_critical_accuracy",
                        "amstar_critical_accuracy", "amstar_accuracy", "overall_score")
      
      for(col in numeric_cols) {
        data[[col]] <- as.numeric(data[[col]])
      }
      
      # 修复：正确处理Context_Length_K_tokens列
      # 将非数值值转换为NA，然后转换为数值
      data$Context_Length_K_tokens <- as.numeric(ifelse(data$Context_Length_K_tokens %in% c("TBA", "-", ""), 
                                                        NA, 
                                                        data$Context_Length_K_tokens))
      
      # 修复：正确处理Release_Year列
      data$Release_Year <- as.numeric(data$Release_Year)
      
      # 保持Total_Parameters_B为字符类型来处理TBA值
      data$Total_Parameters_B <- as.character(data$Total_Parameters_B)
      
      return(data)
    }
    
    # Load data
    benchmark_data <- reactive({
      req(input$top_n)
      
      data <- load_llm_benchmark_data()
      
      # 确保不请求超过可用行数的样本
      available_models <- nrow(data)
      top_n <- min(input$top_n, available_models)
      
      # 排序
      if (input$sort_order == "desc") {
        data <- data %>% arrange(desc(!!sym(input$sort_by)))
      } else {
        data <- data %>% arrange(!!sym(input$sort_by))
      }
      
      # 只取top_n
      data <- head(data, top_n)
      
      return(data)
    })
    
    # Create column information
    column_info <- reactive({
      tibble(
        id = c("model_name", "Developer", "Release_Year", 
               "Context_Length_K_tokens", "Total_Parameters_B", "avg_response_time", "match_success_rate", "studies_accuracy", 
               "sample_accuracy", "extract_accuracy", "amstar_non_critical_accuracy",
               "amstar_critical_accuracy", "amstar_accuracy", "overall_score"),
        name = c("Model", "Developer", "Release Year", 
                 "Context Length", "Parameters", "Response Time", "Match Success Rate", "Studies Accuracy", "Sample Accuracy", 
                 "Extraction Accuracy",  "AMSTAR2 Non-critical", "AMSTAR2 Critical", "AMSTAR2 Accuracy", "Overall Score"),
        group = c("model", "info", "info", "info", "info", "performance", "performance", "accuracy", "accuracy", 
                  "accuracy", "amstar", "amstar", "amstar", "overall"),
        width = c(2, 1.5, 1, 1, 1, 1, 1.2, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 1.5)
      )
    })
    
    # Create row information
    row_info <- reactive({
      data <- benchmark_data()
      tibble(
        id = data$model_name,
        group = "LLM Evaluation"
      )
    })
    
    # Color palette configuration
    palettes <- reactive({
      list(
        performance = "Blues",
        accuracy = "Greens", 
        amstar = "Purples",
        overall = "RdYlBu",
        info = "Greys"
      )
    })
    
    # Render plot
    output$benchmark_plot <- renderPlot({
      data <- benchmark_data()
      
      # 确保数据不为空
      req(nrow(data) > 0)
      
      col_info <- column_info()
      row_info <- row_info()
      palette_list <- palettes()
      
      create_enhanced_model_plot(data, col_info, row_info, palette_list)
    }, res = 96)
    
    # Render data table
    output$benchmark_table <- renderDT({
      data <- benchmark_data()
      
      # 确保数据不为空
      req(nrow(data) > 0)
      
      datatable(
        data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          autoWidth = TRUE,
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf')
        ),
        rownames = FALSE,
        extensions = 'Buttons'
      ) %>%
        formatPercentage(c("match_success_rate", "studies_accuracy", "sample_accuracy", 
                           "extract_accuracy", "amstar_non_critical_accuracy", 
                           "amstar_critical_accuracy", "amstar_accuracy", "overall_score"), 
                         digits = 1) %>%
        formatRound(c("avg_response_time"), digits = 2) %>%
        formatRound(c("Context_Length_K_tokens"), digits = 0)
    })
    
    # Refresh button event
    observeEvent(input$refresh_btn, {
      # 重新加载数据
      benchmark_data()
    })
  })
} 


shinyApp(ui =llmBenchmarkingUI,server =  llmBenchmarkingServer)