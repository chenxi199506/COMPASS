# global_distribution_module.R

# UI部分（保持不变）
globalDistributionUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
             departmentHeader(
               "Global Research Distribution",
               "Visualizing worldwide RCT research collaboration patterns and geographic distribution - Hover over countries to see details",
               icon = "🌍"
             )
      )
    ),
    
    # 标签页布局
    tabsetPanel(
      id = ns("main_tabs"),
      type = "tabs",
      
      # 标签页1: 全球分布地图
      tabPanel(
        "Global Distribution",
        icon = icon("map"),
        fluidRow(
          box(
            title = "Global Distribution Map",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            plotOutput(ns("global_map"), height = "600px",
                       hover = hoverOpts(id = ns("plot_hover"), delay = 100, delayType = "debounce")),
            uiOutput(ns("hover_info"))
          )
        ),
        
        fluidRow(
          box(
            title = "Map Controls",
            status = "warning",
            solidHeader = TRUE,
            width = 4,
            collapsible = TRUE,
            selectInput(ns("color_scheme"), "Color Scheme:",
                        choices = c("Green-Yellow-Red" = "default",
                                    "Blue-Purple" = "blue_purple",
                                    "Viridis" = "viridis",
                                    "Plasma" = "plasma"),
                        selected = "default"),
            sliderInput(ns("min_percentage"), "Minimum Percentage to Display:",
                        min = 0, max = 10, value = 0, step = 0.1,
                        post = "%"),
            actionButton(ns("reset_view"), "Reset View", icon = icon("refresh"))
          ),
          
          box(
            title = "Top Countries by Publication Count",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            collapsible = TRUE,
            DTOutput(ns("country_table"))
          ),
          
          box(
            title = "Country Details",
            status = "success",
            solidHeader = TRUE,
            width = 4,
            collapsible = TRUE,
            htmlOutput(ns("selected_country_info"))
          )
        )
      ),
      
      # 标签页2: 合作网络图
      tabPanel(
        "Collaboration Network",
        icon = icon("project-diagram"),
        fluidRow(
          box(
            title = "Global Scientific Collaboration Network",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            plotOutput(ns("collaboration_network"), height = "700px"),
            tags$div(
              style = "margin-top: 15px; font-size: 12px; color: #666;",
              "Note: Node size represents publication volume, line color and opacity indicate collaboration strength."
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Network Controls",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            sliderInput(ns("min_collaboration"), "Minimum Collaboration Frequency:",
                        min = 1, max = 50, value = 1, step = 1),
            sliderInput(ns("node_size_range"), "Node Size Range:",
                        min = 1, max = 20, value = c(2, 10), step = 1),
            selectInput(ns("network_color"), "Network Color Scheme:",
                        choices = c("Plasma" = "plasma", "Viridis" = "viridis", 
                                    "Inferno" = "inferno", "Magma" = "magma"),
                        selected = "plasma")
          ),
          
          box(
            title = "Top Collaborations",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            DTOutput(ns("collaboration_table"))
          )
        )
      ),
      
      # 标签页3: 引用分析
      tabPanel(
        "Citation Analysis",
        icon = icon("chart-line"),
        fluidRow(
          box(
            title = "Total Citations by Country",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            plotOutput(ns("total_citations_plot"), height = "500px")
          ),
          
          box(
            title = "Mean Citations per Study",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            plotOutput(ns("mean_citations_plot"), height = "500px")
          )
        ),
        
        fluidRow(
          box(
            title = "Citation Analysis Controls",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            sliderInput(ns("top_countries"), "Number of Top Countries to Display:",
                        min = 5, max = 30, value = 20, step = 1),
            selectInput(ns("citation_metric"), "Primary Sorting Metric:",
                        choices = c("Total Citations" = "total_citation",
                                    "Mean Citations" = "mean_citation",
                                    "Study Count" = "study_count"),
                        selected = "total_citation")
          )
        )
      ),
      
      # 标签页4: 研究指标热力图
      tabPanel(
        "Research Metrics",
        icon = icon("table"),
        fluidRow(
          box(
            title = "Research Metrics Heatmap by Country",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            plotOutput(ns("metrics_heatmap"), height = "600px")
          )
        ),
        
        fluidRow(
          box(
            title = "Heatmap Controls",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            selectInput(ns("heatmap_metric"), "Primary Metric for Country Order:",
                        choices = c("Total Citations" = "total_citation",
                                    "Mean Citations" = "mean_citation",
                                    "Study Count" = "study_count"),
                        selected = "total_citation"),
            checkboxGroupInput(ns("display_metrics"), "Metrics to Display:",
                               choices = c("Sample Size" = "Total_Sample_Size",
                                           "Citations" = "citation_count",
                                           "5-Year JIF" = "5-Year JIF",
                                           "CI Width" = "CI_Width",
                                           "Statistical Power" = "statistical_power"),
                               selected = c("Total_Sample_Size", "citation_count", 
                                            "5-Year JIF", "CI_Width", "statistical_power"))
          ),
          
          box(
            title = "Metric Descriptions",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            tags$ul(
              tags$li(tags$strong("Sample Size:"), "Median total sample size"),
              tags$li(tags$strong("Citations:"), "Median citation count"),
              tags$li(tags$strong("5-Year JIF:"), "Median 5-Year Journal Impact Factor"),
              tags$li(tags$strong("CI Width:"), "Median confidence interval width"),
              tags$li(tags$strong("Statistical Power:"), "Median statistical power")
            )
          )
        )
      )
    ),
    
    # 添加CSS样式
    tags$style(HTML("
      .hover-tooltip {
        position: absolute;
        background: rgba(255, 255, 255, 0.95);
        border: 2px solid #00695C;
        border-radius: 8px;
        padding: 12px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
        pointer-events: none;
        z-index: 1000;
        max-width: 300px;
        font-family: 'Roboto', sans-serif;
      }
      .hover-country {
        font-weight: bold;
        color: #00695C;
        font-size: 16px;
        margin-bottom: 5px;
      }
      .hover-stats {
        color: #333;
        font-size: 14px;
        line-height: 1.4;
      }
      .hover-percentage {
        color: #D73027;
        font-weight: bold;
      }
      .no-data {
        color: #999;
        font-style: italic;
      }
      .tab-content {
        background-color: #f8f9fa;
      }
    "))
  )
}

# Server部分
globalDistributionServer <- function(id, meta_db) {
  moduleServer(id, function(input, output, session) {
    
    # 响应式数据 - 使用传入的 meta_db
    processed_data <- reactive({
      tryCatch({
        # 使用传入的 meta_db 而不是重新读取
        analysis_data_with_power <- meta_db
        raw_data <- analysis_data_with_power$country_standardized
        
        # 基础数据处理
        df <- tibble(raw_string = raw_data) %>%
          mutate(paper_id = row_number()) %>%
          filter(!is.na(raw_string)) %>%
          filter(raw_string != "Multinational")
        
        df_long <- df %>%
          separate_rows(raw_string, sep = ";\\s*") %>%
          mutate(country_clean = str_trim(raw_string))
        
        df_long <- df_long %>%
          mutate(iso3 = countrycode(country_clean, origin = "country.name", destination = "iso3c")) %>%
          filter(!is.na(iso3))
        
        # 为原始数据添加paper_id
        analysis_data_with_id <- analysis_data_with_power %>%
          mutate(paper_id = row_number())
        
        # 合并原始数据用于分析
        df_with_metrics <- df_long %>%
          left_join(
            analysis_data_with_id %>% 
              select(paper_id, citation_count, `5-Year JIF`, 
                     Total_Sample_Size, CI_Width, statistical_power),
            by = "paper_id"
          )
        
        # 国家统计
        country_stats <- df_long %>%
          group_by(iso3) %>%
          summarise(
            count = n(),
            studies = paste(unique(paper_id), collapse = ", ")
          ) %>%
          mutate(percentage = count / sum(count) * 100) %>%
          arrange(desc(count))
        
        # 合作网络数据
        collaboration_edges <- df_long %>%
          group_by(paper_id) %>%
          filter(n() > 1) %>%
          summarise(combs = list(as.data.frame(t(combn(sort(unique(iso3)), 2))))) %>%
          unnest(combs) %>%
          rename(source = V1, target = V2) %>%
          group_by(source, target) %>%
          summarise(weight = n(), .groups = "drop")
        
        # 引用数据
        citation_stats <- df_with_metrics %>%
          group_by(iso3) %>%
          summarise(
            total_citation = sum(citation_count, na.rm = TRUE),
            mean_citation = mean(citation_count, na.rm = TRUE),
            study_count = n(),
            .groups = 'drop'
          )
        
        # 研究指标数据
        metrics_stats <- df_with_metrics %>%
          group_by(iso3) %>%
          summarise(
            Total_Sample_Size = median(Total_Sample_Size, na.rm = TRUE),
            citation_count = median(citation_count, na.rm = TRUE),
            `5-Year JIF` = median(`5-Year JIF`, na.rm = TRUE),
            CI_Width = median(CI_Width, na.rm = TRUE),
            statistical_power = median(statistical_power, na.rm = TRUE),
            .groups = 'drop'
          )
        
        # 地图数据 - 修复：将所有缺失值赋值为0
        world_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
          filter(iso_a3 != "ATA")
        
        map_data <- world_map %>%
          left_join(country_stats, by = c("iso_a3" = "iso3")) %>%
          # 将缺失值替换为0
          mutate(
            count = ifelse(is.na(count), 0, count),
            percentage = ifelse(is.na(percentage), 0, percentage)
          )
        
        # 国家中心点坐标
        country_centroids <- world_map %>%
          st_centroid() %>%
          st_coordinates() %>%
          as.data.frame() %>%
          bind_cols(iso3 = world_map$iso_a3) %>%
          rename(long = X, lat = Y)
        
        # 合作连线地理数据
        edges_geo <- collaboration_edges %>%
          left_join(country_centroids, by = c("source" = "iso3")) %>%
          rename(long_start = long, lat_start = lat) %>%
          left_join(country_centroids, by = c("target" = "iso3")) %>%
          rename(long_end = long, lat_end = lat) %>%
          filter(!is.na(long_start) & !is.na(long_end))
        
        list(
          df_long = df_long,
          df_with_metrics = df_with_metrics,
          country_stats = country_stats,
          citation_stats = citation_stats,
          metrics_stats = metrics_stats,
          collaboration_edges = collaboration_edges,
          edges_geo = edges_geo,
          world_map = world_map,
          map_data = map_data,
          country_centroids = country_centroids,
          total_studies = nrow(df),
          total_countries = nrow(country_stats)
        )
        
      }, error = function(e) {
        # 模拟数据生成（错误处理）
        generate_simulated_data()
      })
    })
    
    # 模拟数据生成函数 - 修复版本
    generate_simulated_data <- function() {
      set.seed(123)
      countries <- c("USA", "CHN", "GBR", "DEU", "FRA", "JPN", "CAN", "AUS", 
                     "ITA", "ESP", "KOR", "BRA", "IND", "RUS", "NLD", "SWE", 
                     "CHE", "BEL", "AUT", "DNK", "NOR", "FIN", "IRL", "PRT")
      
      # 生成基础数据
      n_studies <- 200
      df_long <- tibble(
        paper_id = 1:n_studies,
        country_clean = sample(countries, n_studies, replace = TRUE, 
                               prob = c(0.3, 0.25, 0.1, 0.08, 0.07, 0.06, 
                                        0.05, 0.04, 0.03, 0.02, rep(0.01, 14))),
        iso3 = country_clean
      ) %>%
        mutate(
          citation_count = sample(0:100, n_studies, replace = TRUE),
          `5-Year JIF` = runif(n_studies, 1, 20),
          Total_Sample_Size = sample(50:2000, n_studies, replace = TRUE),
          CI_Width = runif(n_studies, 0.1, 2),
          statistical_power = runif(n_studies, 0.3, 0.95)
        )
      
      # 计算各种统计指标
      country_stats <- df_long %>%
        group_by(iso3) %>%
        summarise(
          count = n(),
          studies = paste(unique(paper_id), collapse = ", ")
        ) %>%
        mutate(percentage = count / sum(count) * 100) %>%
        arrange(desc(count))
      
      citation_stats <- df_long %>%
        group_by(iso3) %>%
        summarise(
          total_citation = sum(citation_count, na.rm = TRUE),
          mean_citation = mean(citation_count, na.rm = TRUE),
          study_count = n(),
          .groups = 'drop'
        )
      
      metrics_stats <- df_long %>%
        group_by(iso3) %>%
        summarise(
          Total_Sample_Size = median(Total_Sample_Size, na.rm = TRUE),
          citation_count = median(citation_count, na.rm = TRUE),
          `5-Year JIF` = median(`5-Year JIF`, na.rm = TRUE),
          CI_Width = median(CI_Width, na.rm = TRUE),
          statistical_power = median(statistical_power, na.rm = TRUE),
          .groups = 'drop'
        )
      
      # 生成合作数据
      collaboration_pairs <- list()
      for(i in 1:100) {
        pair <- sample(countries, 2, replace = FALSE)
        collaboration_pairs[[i]] <- tibble(
          source = pair[1],
          target = pair[2],
          weight = sample(1:10, 1)
        )
      }
      
      collaboration_edges <- bind_rows(collaboration_pairs) %>%
        group_by(source, target) %>%
        summarise(weight = sum(weight), .groups = "drop")
      
      # 地图数据 - 修复：将所有缺失值赋值为0
      world_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
        filter(iso_a3 != "ATA")
      
      map_data <- world_map %>%
        left_join(country_stats, by = c("iso_a3" = "iso3")) %>%
        # 将缺失值替换为0
        mutate(
          count = ifelse(is.na(count), 0, count),
          percentage = ifelse(is.na(percentage), 0, percentage)
        )
      
      # 国家中心点
      country_centroids <- world_map %>%
        st_centroid() %>%
        st_coordinates() %>%
        as.data.frame() %>%
        bind_cols(iso3 = world_map$iso_a3) %>%
        rename(long = X, lat = Y)
      
      # 合作连线地理数据
      edges_geo <- collaboration_edges %>%
        left_join(country_centroids, by = c("source" = "iso3")) %>%
        rename(long_start = long, lat_start = lat) %>%
        left_join(country_centroids, by = c("target" = "iso3")) %>%
        rename(long_end = long, lat_end = lat) %>%
        filter(!is.na(long_start) & !is.na(long_end))
      
      list(
        df_long = df_long,
        df_with_metrics = df_long,
        country_stats = country_stats,
        citation_stats = citation_stats,
        metrics_stats = metrics_stats,
        collaboration_edges = collaboration_edges,
        edges_geo = edges_geo,
        world_map = world_map,
        map_data = map_data,
        country_centroids = country_centroids,
        total_studies = n_studies,
        total_countries = length(countries)
      )
    }
    
    # ==========================================================================
    # 原有功能
    # ==========================================================================
    
    # 过滤后的地图数据
    filtered_map_data <- reactive({
      data <- processed_data()
      map_data <- data$map_data
      
      if (input$min_percentage > 0) {
        map_data <- map_data %>%
          mutate(percentage = ifelse(percentage < input$min_percentage, NA, percentage))
      }
      
      map_data
    })
    
    # 颜色方案 - 修复：处理所有值为0的情况
    color_palette <- reactive({
      map_data <- filtered_map_data()
      max_percentage <- max(map_data$percentage, na.rm = TRUE)
      
      # 如果所有值都是0，设置一个小的范围
      if (max_percentage == 0) {
        max_percentage <- 1
      }
      
      switch(input$color_scheme,
             "default" = scale_fill_gradientn(
               colors = c("#91CF60", "#FEE08B", "#FC8D59", "#D73027"),
               na.value = "#f0f0f0",
               name = "Relative Share (%)",
               guide = guide_colorbar(barwidth = 15, barheight = 0.8),
               limits = c(0, max_percentage),
               breaks = pretty_breaks(n = 6)
             ),
             "blue_purple" = scale_fill_gradientn(
               colors = c("#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695"),
               na.value = "#f0f0f0",
               name = "Relative Share (%)",
               guide = guide_colorbar(barwidth = 15, barheight = 0.8),
               limits = c(0, max_percentage),
               breaks = pretty_breaks(n = 6)
             ),
             "viridis" = scale_fill_viridis_c(
               na.value = "#f0f0f0",
               name = "Relative Share (%)",
               guide = guide_colorbar(barwidth = 15, barheight = 0.8),
               option = "viridis",
               limits = c(0, max_percentage)
             ),
             "plasma" = scale_fill_viridis_c(
               na.value = "#f0f0f0",
               name = "Relative Share (%)",
               guide = guide_colorbar(barwidth = 15, barheight = 0.8),
               option = "plasma",
               limits = c(0, max_percentage)
             )
      )
    })
    
    # 渲染全球地图
    output$global_map <- renderPlot({
      map_data <- filtered_map_data()
      
      ggplot(data = map_data) +
        geom_sf(aes(fill = percentage), color = "white", size = 0.2) +
        color_palette() +
        labs(
          title = "Global Distribution of RCT Research",
          subtitle = paste("Total studies:", processed_data()$total_studies, 
                           "| Countries:", processed_data()$total_countries),
          caption = "Hover over countries to see details"
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray50"),
          plot.caption = element_text(size = 10, hjust = 0.5, color = "gray60"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()
        )
    })
    
    # 鼠标悬停信息
    output$hover_info <- renderUI({
      hover <- input$plot_hover
      map_data <- filtered_map_data()
      
      if (is.null(hover)) return(NULL)
      
      point <- st_sfc(st_point(c(hover$x, hover$y)), crs = st_crs(map_data))
      country_index <- st_intersects(point, map_data, sparse = FALSE)[1, ]
      
      if (any(country_index)) {
        country <- map_data[country_index, ]
        country_name <- country$name_long
        iso3 <- country$iso_a3
        
        country_stats <- processed_data()$country_stats %>%
          filter(iso3 == !!iso3)
        
        if (nrow(country_stats) > 0) {
          count <- country_stats$count
          percentage <- round(country_stats$percentage, 2)
          global_rank <- which(processed_data()$country_stats$iso3 == iso3)
          
          div(
            class = "hover-tooltip",
            style = paste0("left:", hover$coords_css$x + 10, "px; top:", hover$coords_css$y - 100, "px;"),
            div(class = "hover-country", country_name),
            div(class = "hover-stats",
                paste("Studies:", count)),
            div(class = "hover-stats",
                paste("Global Share:", tags$span(class = "hover-percentage", paste0(percentage, "%")), 
                      sep = " ")),
            div(class = "hover-stats",
                paste("Global Rank:", paste0("#", global_rank)))
          )
        } else {
          div(
            class = "hover-tooltip",
            style = paste0("left:", hover$coords_css$x + 10, "px; top:", hover$coords_css$y - 50, "px;"),
            div(class = "hover-country", country_name),
            div(class = "hover-stats no-data", "No research data available")
          )
        }
      } else {
        NULL
      }
    })
    
    # 选中国家信息
    selected_country <- reactiveVal(NULL)
    
    observeEvent(input$plot_hover, {
      hover <- input$plot_hover
      map_data <- filtered_map_data()
      
      if (is.null(hover)) {
        selected_country(NULL)
        return()
      }
      
      point <- st_sfc(st_point(c(hover$x, hover$y)), crs = st_crs(map_data))
      country_index <- st_intersects(point, map_data, sparse = FALSE)[1, ]
      
      if (any(country_index)) {
        country <- map_data[country_index, ]
        selected_country(country$iso_a3)
      } else {
        selected_country(NULL)
      }
    })
    
    # 渲染选中国家信息
    output$selected_country_info <- renderUI({
      country_iso3 <- selected_country()
      
      if (is.null(country_iso3)) {
        return(
          div(
            style = "text-align: center; padding: 20px; color: #666;",
            icon("mouse-pointer", class = "fa-2x"),
            br(), br(),
            "Hover over a country on the map to see detailed information"
          )
        )
      }
      
      country_stats <- processed_data()$country_stats %>%
        filter(iso3 == country_iso3)
      
      if (nrow(country_stats) == 0) {
        return(
          div(
            style = "text-align: center; padding: 20px; color: #666;",
            "No data available for selected country"
          )
        )
      }
      
      country_name <- countrycode(country_iso3, origin = "iso3c", destination = "country.name")
      global_rank <- which(processed_data()$country_stats$iso3 == country_iso3)
      total_countries <- nrow(processed_data()$country_stats)
      
      div(
        h4(country_name, style = "color: #00695C; margin-top: 0;"),
        hr(),
        div(style = "font-size: 24px; font-weight: bold; color: #D73027;",
            paste0(round(country_stats$percentage, 2), "%")),
        div(style = "color: #666; font-size: 14px;", "Global Share"),
        br(),
        div(style = "font-size: 20px; font-weight: bold; color: #4575B4;",
            country_stats$count),
        div(style = "color: #666; font-size: 14px;", "Number of Studies"),
        br(),
        div(style = "font-size: 16px; color: #2E7D32;",
            paste("Global Rank:", global_rank, "of", total_countries)),
        br(),
        div(style = "background: #E8F5E8; padding: 10px; border-radius: 5px; font-size: 12px;",
            "This country contributes ", round(country_stats$percentage, 2), 
            "% to global RCT research")
      )
    })
    
    # 渲染国家表格
    output$country_table <- renderDT({
      country_stats <- processed_data()$country_stats
      
      display_table <- country_stats %>%
        mutate(
          country_name = countrycode(iso3, origin = "iso3c", destination = "country.name"),
          country_name = ifelse(is.na(country_name), iso3, country_name),
          percentage = round(percentage, 2),
          rank = row_number()
        ) %>%
        select(rank, country_name, count, percentage) %>%
        arrange(rank)
      
      datatable(
        display_table,
        options = list(
          pageLength = 10,
          dom = 'tip',
          scrollX = TRUE,
          ordering = TRUE
        ),
        colnames = c('Rank', 'Country', 'Studies', 'Share (%)'),
        rownames = FALSE,
        selection = 'single'
      ) %>% 
        formatRound('percentage', 2)
    })
    
    # 表格选择事件
    observeEvent(input$country_table_rows_selected, {
      if (length(input$country_table_rows_selected) > 0) {
        country_stats <- processed_data()$country_stats
        selected_iso3 <- country_stats$iso3[input$country_table_rows_selected]
        selected_country(selected_iso3)
      }
    })
    
    # ==========================================================================
    # 新增功能：合作网络图
    # ==========================================================================
    
    # 过滤后的合作数据
    filtered_edges_geo <- reactive({
      data <- processed_data()
      edges_geo <- data$edges_geo
      
      edges_geo %>%
        filter(weight >= input$min_collaboration) %>%
        arrange(weight)
    })
    
    # 渲染合作网络图
    output$collaboration_network <- renderPlot({
      data <- processed_data()
      edges_geo <- filtered_edges_geo()
      
      ggplot() +
        # 底图
        geom_sf(data = data$world_map, 
                fill = "#F5F5F5", 
                color = "white", 
                size = 0.2) +
        
        # 合作连线
        geom_curve(data = edges_geo, 
                   aes(x = long_start, y = lat_start, xend = long_end, yend = lat_end, 
                       color = weight,
                       alpha = weight),
                   size = 0.3,
                   curvature = 0.2,
                   lineend = "round") +
        
        # 国家节点
        geom_point(data = data$country_stats %>% 
                     left_join(data$country_centroids, by = c("iso3" = "iso3")),
                   aes(x = long, y = lat, size = count), 
                   fill = "#3B528B", 
                   color = "white", 
                   shape = 21, 
                   stroke = 0.5) +
        
        # 标尺设置
        scale_size_continuous(
          range = input$node_size_range,
          name = "No. of Papers",
          guide = guide_legend(order = 1)
        ) +
        
        # 网络颜色方案
        scale_color_viridis_c(
          option = input$network_color,
          name = "Link Strength",
          begin = 0.1, 
          end = 0.9
        ) +
        
        scale_alpha_continuous(range = c(0.15, 0.8), guide = "none") +
        
        # 坐标系限制
        coord_sf(ylim = c(-50, 90)) +
        
        # 标注与主题
        labs(
          title = "International Collaboration Network",
          subtitle = "Co-authorship intensity and research output by country",
          x = NULL,
          y = NULL
        ) +
        
        theme_void() +
        theme(
          plot.margin = margin(10, 10, 10, 10),
          plot.title = element_text(family = "sans", face = "bold", size = 16, color = "black"),
          plot.subtitle = element_text(family = "sans", size = 11, color = "grey40"),
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.title = element_text(size = 9, face = "bold"),
          legend.text = element_text(size = 8)
        )
    })
    
    # 合作表格
    output$collaboration_table <- renderDT({
      data <- processed_data()
      collaboration_edges <- data$collaboration_edges
      
      display_table <- collaboration_edges %>%
        filter(weight >= input$min_collaboration) %>%
        mutate(
          source_name = countrycode(source, origin = "iso3c", destination = "country.name"),
          target_name = countrycode(target, origin = "iso3c", destination = "country.name"),
          source_name = ifelse(is.na(source_name), source, source_name),
          target_name = ifelse(is.na(target_name), target, target_name)
        ) %>%
        select(source_name, target_name, weight) %>%
        arrange(desc(weight)) %>%
        head(20)
      
      datatable(
        display_table,
        options = list(
          pageLength = 10,
          dom = 'tip',
          scrollX = TRUE
        ),
        colnames = c('Country A', 'Country B', 'Collaborations'),
        rownames = FALSE
      )
    })
    
    # ==========================================================================
    # 新增功能：引用分析
    # ==========================================================================
    
    # 引用数据
    citation_plot_data <- reactive({
      data <- processed_data()
      citation_stats <- data$citation_stats
      
      top_countries <- citation_stats %>%
        arrange(desc(!!sym(input$citation_metric))) %>%
        head(input$top_countries) %>%
        mutate(
          country_name = countrycode(iso3, origin = "iso3c", destination = "country.name"),
          country_name = ifelse(is.na(country_name), iso3, country_name),
          country_name = reorder(country_name, !!sym(input$citation_metric))
        )
      
      top_countries
    })
    
    # 总引用图
    output$total_citations_plot <- renderPlot({
      plot_data <- citation_plot_data()
      
      ggplot(plot_data) +
        geom_col(
          aes(x = country_name, y = total_citation, fill = total_citation),
          alpha = 0.8, width = 0.7
        ) +
        coord_flip() +
        scale_fill_gradient2(
          name = "Total Citations",
          low = "#1A9850",
          mid = "#FEE08B",
          high = "#D73027",
          midpoint = median(plot_data$total_citation)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(
          title = "Total Citations by Country",
          subtitle = paste("Top", input$top_countries, "countries by", 
                           switch(input$citation_metric,
                                  "total_citation" = "total citations",
                                  "mean_citation" = "mean citations",
                                  "study_count" = "study count")),
          x = "Country",
          y = "Total Citation Count"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10, color = "gray50"),
          axis.text.y = element_text(size = 10, face = "bold"),
          axis.text.x = element_text(size = 9),
          axis.title = element_text(size = 11, face = "bold"),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA)
        )
    })
    
    # 平均引用图
    output$mean_citations_plot <- renderPlot({
      plot_data <- citation_plot_data()
      
      ggplot(plot_data) +
        geom_col(
          aes(x = country_name, y = mean_citation, fill = mean_citation),
          alpha = 0.8, width = 0.7
        ) +
        coord_flip() +
        scale_fill_gradient2(
          name = "Mean Citations",
          low = "#1A9850",
          mid = "#FEE08B",
          high = "#D73027",
          midpoint = median(plot_data$mean_citation)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(
          title = "Mean Citations per Study by Country",
          subtitle = paste("Average citation count per study for top", 
                           input$top_countries, "countries"),
          x = "Country",
          y = "Mean Citation Count"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10, color = "gray50"),
          axis.text.y = element_text(size = 10, face = "bold"),
          axis.text.x = element_text(size = 9),
          axis.title = element_text(size = 11, face = "bold"),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA)
        )
    })
    
    # ==========================================================================
    # 新增功能：研究指标热力图
    # ==========================================================================
    
    # 热力图数据
    heatmap_data <- reactive({
      data <- processed_data()
      
      # 获取按选定指标排序的前25个国家
      top_countries <- data$citation_stats %>%
        arrange(desc(!!sym(input$heatmap_metric))) %>%
        head(25) %>%
        pull(iso3)
      
      # 合并指标数据
      combined_data <- data$metrics_stats %>%
        filter(iso3 %in% top_countries) %>%
        left_join(data$citation_stats %>% select(iso3, total_citation, mean_citation, study_count), 
                  by = "iso3") %>%
        mutate(
          country_name = countrycode(iso3, origin = "iso3c", destination = "country.name"),
          country_name = ifelse(is.na(country_name), iso3, country_name)
        ) %>%
        arrange(desc(!!sym(input$heatmap_metric)))
      
      # 转换为长格式并进行分列归一化
      heatmap_long <- combined_data %>%
        select(country_name, all_of(input$display_metrics)) %>%
        pivot_longer(
          cols = -country_name,
          names_to = "variable",
          values_to = "median_value"
        ) %>%
        mutate(
          variable = factor(variable, 
                            levels = c("Total_Sample_Size", "citation_count", "5-Year JIF", 
                                       "CI_Width", "statistical_power"),
                            labels = c("Sample Size", "Citations", "5-Year JIF", 
                                       "CI Width", "Statistical Power"))
        ) %>%
        group_by(variable) %>%
        mutate(
          scaled_for_color = (median_value - min(median_value, na.rm = TRUE)) / 
            (max(median_value, na.rm = TRUE) - min(median_value, na.rm = TRUE))
        ) %>%
        ungroup()
      
      list(
        long = heatmap_long,
        wide = combined_data
      )
    })
    
    # 渲染热力图
    output$metrics_heatmap <- renderPlot({
      data <- heatmap_data()
      heatmap_long <- data$long
      
      ggplot(heatmap_long, aes(x = variable, y = country_name)) +
        geom_tile(aes(fill = scaled_for_color), color = "white", size = 0.5) +
        geom_text(aes(label = sprintf("%.2f", median_value)), 
                  size = 3, fontface = "bold") +
        scale_fill_gradient2(
          name = "Relative Level\n(per column)",
          low = "#1A9850",
          mid = "#FEE08B",
          high = "#D73027",
          midpoint = 0.5,
          breaks = c(0, 0.5, 1),
          labels = c("Low", "Med", "High")
        ) +
        labs(
          title = "Research Metrics by Country: Median Values Heatmap",
          subtitle = paste("Top 25 countries by", 
                           switch(input$heatmap_metric,
                                  "total_citation" = "total citations",
                                  "mean_citation" = "mean citations",
                                  "study_count" = "study count"),
                           "| Colors scaled relative to each column"),
          x = "Research Metrics",
          y = "Country"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 10, color = "gray0", hjust = 0.5),
          axis.text.x = element_text(size = 10, face = "bold", angle = 45, hjust = 1),
          axis.text.y = element_text(size = 10, face = "bold"),
          axis.title = element_text(size = 11, face = "bold"),
          legend.position = "right",
          legend.title = element_text(face = "bold"),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA)
        )
    })
    
    # 重置视图
    observeEvent(input$reset_view, {
      selected_country(NULL)
      updateSliderInput(session, "min_percentage", value = 0)
      updateSelectInput(session, "color_scheme", selected = "default")
    })
    
  })
}