# 11llm_benchmark_viz_module.R
# LLM Benchmark 可视化模块

llmBenchmarkVizUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    departmentHeader("LLM Benchmark Visualization", "Interactive Model Performance Comparison"),
    
    fluidRow(
      column(3,
             wellPanel(
               style = "background: white; border-top: 3px solid #00695C;",
               h4("Data Configuration", icon = "cog"),
               
               fileInput(ns("data_file"), "Upload Benchmark Data",
                         accept = c(".csv", ".xlsx", ".xls"),
                         placeholder = "Select CSV or Excel file"),
               
               selectInput(ns("sort_by"), "Sort Models By:",
                           choices = c("Overall Score" = "overall_score",
                                       "Match Success Rate" = "match_success_rate",
                                       "Response Time" = "avg_response_time",
                                       "Studies Accuracy" = "studies_accuracy"),
                           selected = "overall_score"),
               
               sliderInput(ns("num_models"), "Number of Top Models to Display:",
                           min = 5, max = 50, value = 20, step = 5),
               
               checkboxGroupInput(ns("metric_groups"), "Display Metric Groups:",
                                  choices = c("Basic Information" = "info",
                                              "Matching Performance" = "matching",
                                              "Study Accuracy" = "accuracy",
                                              "AMSTAR2 Assessment" = "amstar",
                                              "Overall Score" = "overall"),
                                  selected = c("info", "matching", "accuracy", "amstar", "overall")),
               
               actionButton(ns("update_plot"), "Update Visualization", 
                            icon = icon("refresh"), 
                            class = "btn-primary"),
               
               hr(),
               
               downloadButton(ns("download_plot"), "Download Plot",
                              class = "btn-success")
             )
      ),
      
      column(9,
             tabBox(
               width = 12,
               title = tagList(icon("chart-bar"), "Model Performance Visualization"),
               
               tabPanel("Performance Matrix",
                        plotOutput(ns("benchmark_plot"), height = "800px"),
                        div(style = "margin-top: 20px;",
                            h4("Plot Legend"),
                            HTML("
                              <div style='font-size: 12px; line-height: 1.4;'>
                                <p><strong>Circles:</strong> Represent accuracy metrics (larger = better performance)</p>
                                <p><strong>Bars:</strong> Show success rates and overall scores (longer = better)</p>
                                <p><strong>Colors:</strong> Indicate performance levels within each metric group</p>
                                <p><strong>Ranking:</strong> Numbers on left show overall performance ranking</p>
                              </div>
                            ")
                        )
               ),
               
               tabPanel("Data Table",
                        DTOutput(ns("data_table")),
                        style = "margin-top: 20px;"
               ),
               
               tabPanel("Metric Descriptions",
                        div(style = "padding: 20px;",
                            h4("Performance Metrics Explanation"),
                            tags$ul(
                              tags$li(tags$strong("Response Time (s):"), " Average time taken to generate responses"),
                              tags$li(tags$strong("Match Rate:"), " Success rate in matching expected outputs"),
                              tags$li(tags$strong("Studies Accuracy:"), " Accuracy in study identification tasks"),
                              tags$li(tags$strong("Sample Accuracy:"), " Accuracy in sample size extraction"),
                              tags$li(tags$strong("Extract Accuracy:"), " Accuracy in data extraction tasks"),
                              tags$li(tags$strong("AMSTAR2 Non-critical:"), " Accuracy on non-critical AMSTAR2 items"),
                              tags$li(tags$strong("AMSTAR2 Critical:"), " Accuracy on critical AMSTAR2 items"),
                              tags$li(tags$strong("AMSTAR2 Acc:"), " Overall AMSTAR2 assessment accuracy"),
                              tags$li(tags$strong("Overall Score:"), " Composite performance score across all metrics")
                            )
                        )
               )
             )
      )
    )
  )
}

llmBenchmarkVizServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 响应式数据
    reactive_data <- reactiveVal()
    reactive_column_info <- reactiveVal()
    reactive_row_info <- reactiveVal()
    
    # 处理上传的数据
    observeEvent(input$data_file, {
      req(input$data_file)
      
      tryCatch({
        ext <- tools::file_ext(input$data_file$name)
        
        if (ext == "csv") {
          data <- read.csv(input$data_file$datapath, stringsAsFactors = FALSE)
        } else if (ext %in% c("xlsx", "xls")) {
          data <- readxl::read_excel(input$data_file$datapath)
        } else {
          stop("Unsupported file format")
        }
        
        # 数据预处理
        data_processed <- preprocess_benchmark_data(data)
        reactive_data(data_processed$data)
        reactive_column_info(data_processed$column_info)
        reactive_row_info(data_processed$row_info)
        
        showNotification("Data loaded successfully!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error loading data:", e$message), type = "error")
      })
    })
    
    # 默认数据
    observe({
      if (is.null(reactive_data())) {
        # 使用内置示例数据
        example_data <- create_example_data()
        reactive_data(example_data$data)
        reactive_column_info(example_data$column_info)
        reactive_row_info(example_data$row_info)
      }
    })
    
    # 过滤和排序数据
    filtered_data <- eventReactive(input$update_plot, {
      req(reactive_data())
      
      data <- reactive_data()
      
      # 排序
      if (input$sort_by %in% names(data)) {
        data <- data[order(-data[[input$sort_by]]), ]
      }
      
      # 限制显示数量
      if (nrow(data) > input$num_models) {
        data <- head(data, input$num_models)
      }
      
      data
    }, ignoreNULL = FALSE)
    
    # 生成可视化
    output$benchmark_plot <- renderPlot({
      req(filtered_data(), reactive_column_info(), reactive_row_info())
      
      create_enhanced_model_plot(
        data = filtered_data(),
        column_info = reactive_column_info(),
        row_info = reactive_row_info(),
        palettes = list(
          accuracy = "Blues",
          overall = "RdYlBu",
          info = "Greys"
        )
      )
    }, height = 800)
    
    # 数据表格
    output$data_table <- renderDT({
      req(filtered_data())
      
      datatable(
        filtered_data(),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        class = 'display'
      )
    })
    
    # 下载图表
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("llm_benchmark_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
      },
      content = function(file) {
        req(filtered_data(), reactive_column_info(), reactive_row_info())
        
        plot <- create_enhanced_model_plot(
          data = filtered_data(),
          column_info = reactive_column_info(),
          row_info = reactive_row_info(),
          palettes = list(
            accuracy = "Blues",
            overall = "RdYlBu",
            info = "Greys"
          )
        )
        
        ggsave(file, plot, width = 16, height = 12, dpi = 300, bg = "white")
      }
    )
    
    # 返回模块数据
    return(list(
      data = reactive_data,
      filtered_data = filtered_data
    ))
  })
}

# 辅助函数：数据预处理
preprocess_benchmark_data <- function(data) {
  # 确保必要的列存在
  required_columns <- c("model_name", "overall_score", "match_success_rate", 
                        "studies_accuracy", "sample_accuracy", "extract_accuracy")
  
  # 添加缺失的列（如果不存在）
  for (col in required_columns) {
    if (!col %in% names(data)) {
      data[[col]] <- runif(nrow(data), 0.5, 1.0)
    }
  }
  
  # 处理数值列
  numeric_columns <- c("overall_score", "match_success_rate", "studies_accuracy", 
                       "sample_accuracy", "extract_accuracy", "amstar_non_critical_accuracy",
                       "amstar_critical_accuracy", "amstar_accuracy", "avg_response_time")
  
  for (col in numeric_columns) {
    if (col %in% names(data)) {
      data[[col]] <- as.numeric(data[[col]])
    }
  }
  
  # 创建列信息
  column_info <- tibble(
    id = c("model_name", "Developer", "Release_Year", 
           "Context_Length_K_tokens", "Total_Parameters_B", "avg_response_time", 
           "match_success_rate", "studies_accuracy", "sample_accuracy", 
           "extract_accuracy", "amstar_non_critical_accuracy",
           "amstar_critical_accuracy", "amstar_accuracy", "overall_score"),
    name = c("Model", "Developer", "Release Year", 
             "Context (K)", "Params (B)", "Response (s)", "Match Rate", 
             "Studies Acc", "Sample Acc", "Extract Acc", 
             "AMSTAR2 Non-critical", "AMSTAR2 Critical", "AMSTAR2 Acc", "Overall Score"),
    group = c("model", "info", "info", "info", "info", "info", "accuracy", 
              "accuracy", "accuracy", "accuracy", "accuracy", "accuracy", 
              "accuracy", "overall"),
    width = c(3, 2.5, 1, 1, 1, 1, 1.2, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 1.5)
  )
  
  # 过滤存在的列
  existing_columns <- column_info$id[column_info$id %in% names(data)]
  column_info <- column_info[column_info$id %in% existing_columns, ]
  
  # 创建行信息
  row_info <- tibble(
    id = data$model_name,
    group = "LLM Evaluation"
  )
  
  # 按总体分数排序
  data <- data[order(-data$overall_score), ]
  
  return(list(
    data = data,
    column_info = column_info,
    row_info = row_info
  ))
}

# 辅助函数：创建示例数据
create_example_data <- function() {
  set.seed(123)
  
  models <- c("GPT-4", "Claude-3", "Gemini-Pro", "LLaMA-2", "Mistral", 
              "PaLM-2", "ChatGLM", "Qwen", "Baichuan", "Yi-34B",
              "Falcon", "Vicuna", "WizardLM", "CodeLlama", "Meditron")
  
  data <- tibble(
    model_name = models,
    Developer = sample(c("OpenAI", "Anthropic", "Google", "Meta", "Mistral AI",
                         "Google", "Zhipu", "Alibaba", "Baichuan", "01.AI",
                         "TII", "LM-Sys", "WizardLM", "Meta", "EPFL"), length(models)),
    Release_Year = sample(2020:2024, length(models), replace = TRUE),
    Context_Length_K_tokens = sample(c(8, 16, 32, 64, 128, 200), length(models), replace = TRUE),
    Total_Parameters_B = round(runif(length(models), 7, 540), 1),
    avg_response_time = round(runif(length(models), 0.5, 5.0), 2),
    match_success_rate = round(runif(length(models), 0.6, 0.95), 3),
    studies_accuracy = round(runif(length(models), 0.7, 0.98), 3),
    sample_accuracy = round(runif(length(models), 0.65, 0.96), 3),
    extract_accuracy = round(runif(length(models), 0.68, 0.97), 3),
    amstar_non_critical_accuracy = round(runif(length(models), 0.72, 0.99), 3),
    amstar_critical_accuracy = round(runif(length(models), 0.60, 0.94), 3),
    amstar_accuracy = round(runif(length(models), 0.66, 0.96), 3)
  )
  
  # 计算总体分数
  data$overall_score <- round(
    (data$match_success_rate + data$studies_accuracy + data$sample_accuracy + 
       data$extract_accuracy + data$amstar_accuracy) / 5, 3
  )
  
  # 排序
  data <- data[order(-data$overall_score), ]
  
  # 创建列信息
  column_info <- tibble(
    id = c("model_name", "Developer", "Release_Year", 
           "Context_Length_K_tokens", "Total_Parameters_B", "avg_response_time", 
           "match_success_rate", "studies_accuracy", "sample_accuracy", 
           "extract_accuracy", "amstar_non_critical_accuracy",
           "amstar_critical_accuracy", "amstar_accuracy", "overall_score"),
    name = c("Model", "Developer", "Release Year", 
             "Context (K)", "Params (B)", "Response (s)", "Match Rate", 
             "Studies Acc", "Sample Acc", "Extract Acc", 
             "AMSTAR2 Non-critical", "AMSTAR2 Critical", "AMSTAR2 Acc", "Overall Score"),
    group = c("model", "info", "info", "info", "info", "info", "accuracy", 
              "accuracy", "accuracy", "accuracy", "accuracy", "accuracy", 
              "accuracy", "overall"),
    width = c(3, 2.5, 1, 1, 1, 1, 1.2, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 1.5)
  )
  
  # 创建行信息
  row_info <- tibble(
    id = data$model_name,
    group = "LLM Evaluation"
  )
  
  return(list(
    data = data,
    column_info = column_info,
    row_info = row_info
  ))
}

# 可视化函数 (从您提供的代码中复制，稍作修改)
create_enhanced_model_plot <- function(data, column_info, row_info, palettes) {
  # 加载必要的包
  if (!require(ggplot2)) stop("ggplot2 package is required")
  if (!require(ggforce)) stop("ggforce package is required")
  if (!require(dplyr)) stop("dplyr package is required")
  if (!require(scales)) stop("scales package is required")
  if (!require(RColorBrewer)) stop("RColorBrewer package is required")
  
  # 设置尺寸参数
  row_height <- 1.1
  row_space <- 0.1
  col_width <- 1.1
  col_space <- 0.2
  
  # 计算行位置
  row_pos <- row_info %>%
    mutate(
      row_i = row_number(),
      colour_background = row_i %% 2 == 1,
      y = -row_i * (row_height + row_space),
      ymin = y - row_height / 2,
      ymax = y + row_height / 2
    )
  
  # 计算列位置
  column_pos <- column_info %>%
    mutate(
      x = cumsum(c(0, head(width, -1)) + width/2 + c(0, rep(col_space, n()-1))),
      xmin = x - width/2,
      xmax = x + width/2
    )
  
  # 定义指标分组和颜色映射
  metric_groups <- list(
    group1 = list(
      circle_metrics = c("avg_response_time"),
      bar_metrics = c("match_success_rate"),
      color_palette = "Blues",
      group_name = "匹配性能"
    ),
    group2 = list(
      circle_metrics = c("studies_accuracy", "sample_accuracy"),
      bar_metrics = c("extract_accuracy"),
      color_palette = "Greens", 
      group_name = "研究准确性"
    ),
    group3 = list(
      circle_metrics = c("amstar_non_critical_accuracy", "amstar_critical_accuracy"),
      bar_metrics = c("amstar_accuracy"),
      color_palette = "Purples",
      group_name = "AMSTAR2 评估"
    ),
    group4 = list(
      circle_metrics = c(),
      bar_metrics = c("overall_score"),
      color_palette = "RdYlBu",
      group_name = "综合评分"
    )
  )
  
  # 创建圆形数据
  circle_data_list <- list()
  for(group_name in names(metric_groups)) {
    group <- metric_groups[[group_name]]
    palette_name <- group$color_palette
    
    for(metric in group$circle_metrics) {
      if(metric %in% names(data)) {
        values <- data[[metric]]
        # 使用原始风格的圆形大小计算
        r_values <- row_height/2 * sqrt(values)
        r_values <- scales::rescale(r_values, to = c(0.05, 0.55))
        
        # 使用组特定的颜色映射
        palette_colors <- colorRampPalette(rev(RColorBrewer::brewer.pal(9, palette_name)))(length(values))
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
  }
  circle_data <- bind_rows(circle_data_list)
  
  # 创建条形数据
  rect_data_list <- list()
  for(group_name in names(metric_groups)) {
    group <- metric_groups[[group_name]]
    palette_name <- group$color_palette
    
    for(metric in group$bar_metrics) {
      if(metric %in% names(data)) {
        values <- data[[metric]]
        palette_colors <- colorRampPalette(rev(RColorBrewer::brewer.pal(9, palette_name)))(length(values))
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
  }
  rect_data <- bind_rows(rect_data_list)
  
  # 创建基本信息文本数据
  text_metrics <- c("Developer", "Release_Year", "Total_Parameters_B", "Context_Length_K_tokens")
  
  text_data_list <- list()
  
  for(metric in text_metrics) {
    if(metric %in% names(data)) {
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
  }
  text_data <- bind_rows(text_data_list)
  
  # 模型名称文本
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
  
  # 添加排名文本
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
  
  # 列标题
  column_text <- column_pos %>%
    filter(id != "model_name") %>%
    mutate(
      y = max(row_pos$ymax) + 1.2,
      label = name,
      angle = 0,
      hjust = 0.5,
      vjust = 0.5,
      fontface = "bold",
      size = 3.2,
      color = "black"
    )
  
  # 添加组标题
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
  
  # 列标题下方的刻度线
  segment_data <- column_pos %>%
    filter(id != "model_name") %>%
    mutate(
      y = max(row_pos$ymax) + 0.8,
      yend = max(row_pos$ymax) + 1.0,
      color = "black",
      size = 0.5
    )
  
  # 创建图表
  g <- ggplot2::ggplot() +
    # 背景色带
    ggplot2::geom_rect(
      data = row_pos %>% filter(colour_background),
      ggplot2::aes(xmin = min(column_pos$xmin) - 0.3, xmax = max(column_pos$xmax) + 0.3,
                   ymin = ymin, ymax = ymax),
      fill = "#DDDDDD", alpha = 0.8
    ) +
    
    # 条形图
    ggplot2::geom_rect(
      data = rect_data,
      ggplot2::aes(xmin = xmin, xmax = xmin + value * (xmax - xmin), 
                   ymin = ymin, ymax = ymax, fill = colors),
      color = "black", size = 0.25
    ) +
    
    # 圆形图
    ggforce::geom_circle(
      data = circle_data,
      ggplot2::aes(x0 = x0, y0 = y0, r = r, fill = colors),
      color = "black", size = 0.25
    ) +
    
    # 基本信息文本
    ggplot2::geom_text(
      data = text_data,
      ggplot2::aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust),
      fontface = "plain", size = 3, color = "black"
    ) +
    
    # 排名数字
    ggplot2::geom_text(
      data = rank_data,
      ggplot2::aes(x = x, y = y, label = label, hjust = hjust),
      fontface = "bold", size = 4, color = "black"
    ) +
    
    # 模型名称
    ggplot2::geom_text(
      data = model_text_data,
      ggplot2::aes(x = x, y = y, label = label, hjust = hjust),
      fontface = "bold", size = 3.5, color = "black"
    ) +
    
    # 列标题
    ggplot2::geom_text(
      data = column_text,
      ggplot2::aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust),
      fontface = "bold", size = 3.2, color = "black"
    ) +
    
    # 组标题
    ggplot2::geom_text(
      data = group_header_data,
      ggplot2::aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust),
      fontface = "bold", size = 3.5, color = "black"
    ) +
    
    # 刻度线
    ggplot2::geom_segment(
      data = segment_data,
      ggplot2::aes(x = x, xend = x, y = y, yend = yend),
      size = 0.5, color = "black"
    ) +
    
    # 使用identity scales
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::scale_size_identity() +
    
    # 主题设置
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(1, 1, 2, 1, "cm"),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    ) +
    
    # 坐标限制
    ggplot2::coord_equal(
      xlim = c(min(column_pos$xmin) - 1, max(column_pos$xmax) + 0.5),
      ylim = c(min(row_pos$ymin) - 0.5, max(row_pos$ymax) + 3.0)
    )
  
  return(g)
}