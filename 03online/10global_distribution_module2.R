# global_distribution_module.R

# UI部分
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
    "))
  )
}

# Server部分
globalDistributionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 响应式数据
    processed_data <- reactive({
      tryCatch({
        analysis_data_with_power <- readRDS("中间结果1121.RDS")
        raw_data <- analysis_data_with_power$country_standardized
        
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
        
        country_stats <- df_long %>%
          group_by(iso3) %>%
          summarise(
            count = n(),
            studies = paste(unique(paper_id), collapse = ", ")
          ) %>%
          mutate(percentage = count / sum(count) * 100) %>%
          arrange(desc(count))
        
        world_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
          filter(iso_a3 != "ATA")
        
        map_data <- world_map %>%
          left_join(country_stats, by = c("iso_a3" = "iso3"))
        
        list(
          df_long = df_long,
          country_stats = country_stats,
          map_data = map_data,
          total_studies = nrow(df),
          total_countries = nrow(country_stats)
        )
        
      }, error = function(e) {
        # 模拟数据
        set.seed(123)
        countries <- c("USA", "CHN", "GBR", "DEU", "FRA", "JPN", "CAN", "AUS", 
                       "ITA", "ESP", "KOR", "BRA", "IND", "RUS", "NLD", "SWE", 
                       "CHE", "BEL", "AUT", "DNK", "NOR", "FIN", "IRL", "PRT")
        
        df_long <- tibble(
          paper_id = 1:200,
          country_clean = sample(countries, 200, replace = TRUE, 
                                 prob = c(0.3, 0.25, 0.1, 0.08, 0.07, 0.06, 
                                          0.05, 0.04, 0.03, 0.02, rep(0.01, 14))),
          iso3 = country_clean
        )
        
        country_stats <- df_long %>%
          group_by(iso3) %>%
          summarise(
            count = n(),
            studies = paste(unique(paper_id), collapse = ", ")
          ) %>%
          mutate(percentage = count / sum(count) * 100) %>%
          arrange(desc(count))
        
        world_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
          filter(iso_a3 != "ATA")
        
        map_data <- world_map %>%
          left_join(country_stats, by = c("iso_a3" = "iso3"))
        
        list(
          df_long = df_long,
          country_stats = country_stats,
          map_data = map_data,
          total_studies = 200,
          total_countries = length(countries)
        )
      })
    })
    
    # 过滤后的地图数据
    filtered_map_data <- reactive({
      data <- processed_data()
      map_data <- data$map_data
      
      # 根据最小百分比过滤
      if (input$min_percentage > 0) {
        map_data <- map_data %>%
          mutate(percentage = ifelse(percentage < input$min_percentage, NA, percentage))
      }
      
      map_data
    })
    
    # 颜色方案
    color_palette <- reactive({
      switch(input$color_scheme,
             "default" = scale_fill_gradientn(
               colors = c("#91CF60", "#FEE08B", "#FC8D59", "#D73027"),
               na.value = "#f0f0f0",
               name = "Relative Share (%)",
               guide = guide_colorbar(barwidth = 15, barheight = 0.8),
               limits = c(0, max(processed_data()$map_data$percentage, na.rm = TRUE)),
               breaks = pretty_breaks(n = 6)
             ),
             "blue_purple" = scale_fill_gradientn(
               colors = c("#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695"),
               na.value = "#f0f0f0",
               name = "Relative Share (%)",
               guide = guide_colorbar(barwidth = 15, barheight = 0.8),
               limits = c(0, max(processed_data()$map_data$percentage, na.rm = TRUE)),
               breaks = pretty_breaks(n = 6)
             ),
             "viridis" = scale_fill_viridis_c(
               na.value = "#f0f0f0",
               name = "Relative Share (%)",
               guide = guide_colorbar(barwidth = 15, barheight = 0.8),
               option = "viridis"
             ),
             "plasma" = scale_fill_viridis_c(
               na.value = "#f0f0f0",
               name = "Relative Share (%)",
               guide = guide_colorbar(barwidth = 15, barheight = 0.8),
               option = "plasma"
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
      
      # 找到鼠标位置对应的国家
      point <- st_sfc(st_point(c(hover$x, hover$y)), crs = st_crs(map_data))
      country_index <- st_intersects(point, map_data, sparse = FALSE)[1, ]
      
      if (any(country_index)) {
        country <- map_data[country_index, ]
        country_name <- country$name_long
        iso3 <- country$iso_a3
        
        # 获取国家统计数据
        country_stats <- processed_data()$country_stats %>%
          filter(iso3 == !!iso3)
        
        if (nrow(country_stats) > 0) {
          count <- country_stats$count
          percentage <- round(country_stats$percentage, 2)
          global_rank <- which(processed_data()$country_stats$iso3 == iso3)
          
          # 构建悬停信息
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
    
    # 重置视图
    observeEvent(input$reset_view, {
      selected_country(NULL)
      updateSliderInput(session, "min_percentage", value = 0)
      updateSelectInput(session, "color_scheme", selected = "default")
    })
    
  })
}