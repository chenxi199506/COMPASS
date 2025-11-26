library(shiny)
library(tidyverse)
library(countrycode)
library(rnaturalearth)
library(rnaturalearthdata) # 确保安装此包
library(sf)
library(viridis)
library(ggplot2)

# ==============================================================================
# UI 部分
# ==============================================================================
globalCollaborationUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 12,
        title = "Global Evidence Synthesis",
        status = "primary",
        solidHeader = TRUE,
        p("Visualize the geographic distribution of included studies and international collaboration networks."),
        
        tabsetPanel(
          tabPanel("Distribution Map", 
                   icon = icon("globe"),
                   br(),
                   plotOutput(ns("dist_map"), height = "600px")
          ),
          tabPanel("Collaboration Network", 
                   icon = icon("project-diagram"),
                   br(),
                   plotOutput(ns("network_map"), height = "700px")
          )
        )
      )
    )
  )
}

# ==============================================================================
# Server 部分
# ==============================================================================
globalCollaborationServer <- function(id, meta_db, country_col = "Country") {
  moduleServer(id, function(input, output, session) {
    
    # 1. 数据清洗与处理 (Reactive)
    processed_data <- reactive({
      req(meta_db()) # 确保数据存在
      data <- meta_db()
      
      # 检查列是否存在
      if (!country_col %in% names(data)) {
        showNotification(paste("Column", country_col, "not found in dataset"), type = "error")
        return(NULL)
      }
      
      raw_vec <- data[[country_col]]
      
      # 基础清洗
      df <- tibble(raw_string = raw_vec) %>%
        mutate(paper_id = row_number()) %>%
        filter(!is.na(raw_string) & raw_string != "" & raw_string != "Multinational")
      
      # 拆分分号 (One Row per Country per Paper)
      df_long <- df %>%
        separate_rows(raw_string, sep = ";\\s*") %>%
        mutate(country_clean = str_trim(raw_string)) %>%
        # ISO3 转换
        mutate(iso3 = countrycode(country_clean, origin = "country.name", destination = "iso3c")) %>%
        filter(!is.na(iso3))
      
      return(df_long)
    })
    
    # 2. 准备地图数据 (Reactive)
    map_resources <- reactive({
      req(processed_data())
      df_long <- processed_data()
      
      # 获取世界地图
      world_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
        filter(iso_a3 != "ATA") # 去掉南极洲
      
      # 统计各国发文量
      country_stats <- df_long %>%
        group_by(iso3) %>%
        summarise(count = n()) %>%
        mutate(percentage = count / sum(count) * 100) %>%
        arrange(desc(count))
      
      # 合并地图数据
      map_data <- world_map %>%
        left_join(country_stats, by = c("iso_a3" = "iso3"))
      
      # 计算质心 (用于网络图)
      # 注意：st_centroid 对有些几何体可能警告，suppress一下
      country_centroids <- suppressWarnings(
        world_map %>%
          st_centroid() %>%
          st_coordinates() %>%
          as.data.frame() %>%
          bind_cols(iso3 = world_map$iso_a3) %>%
          rename(long = X, lat = Y)
      )
      
      list(world_map = world_map, map_data = map_data, country_stats = country_stats, centroids = country_centroids)
    })
    
    # 3. 准备网络边数据 (Reactive)
    edge_data <- reactive({
      req(processed_data(), map_resources())
      df_long <- processed_data()
      country_centroids <- map_resources()$centroids
      
      # 合作关系提取
      collaboration_edges <- df_long %>%
        group_by(paper_id) %>%
        filter(n() > 1) %>% 
        summarise(combs = list(as.data.frame(t(combn(sort(unique(iso3)), 2))))) %>%
        unnest(combs) %>%
        rename(source = V1, target = V2) %>%
        group_by(source, target) %>%
        summarise(weight = n(), .groups = "drop")
      
      if(nrow(collaboration_edges) == 0) return(NULL)
      
      # 添加坐标
      edges_geo <- collaboration_edges %>%
        left_join(country_centroids, by = c("source" = "iso3")) %>%
        rename(long_start = long, lat_start = lat) %>%
        left_join(country_centroids, by = c("target" = "iso3")) %>%
        rename(long_end = long, lat_end = lat) %>%
        filter(!is.na(long_start) & !is.na(long_end)) %>%
        arrange(weight) # 排序，细线在下
      
      return(edges_geo)
    })
    
    # 4. 绘图 1: 分布热力图
    output$dist_map <- renderPlot({
      res <- map_resources()
      req(res)
      
      ggplot(data = res$map_data) +
        geom_sf(aes(fill = percentage), color = "white", size = 0.1) +
        scale_fill_gradientn(
          colors = c("#91CF60", "#FEE08B", "#D73027"),
          na.value = "#F0F0F0", # 没有数据的国家设为浅灰
          name = "Relative Share (%)",
          guide = guide_colorbar(barwidth = 15, barheight = 0.8)
        ) +
        labs(title = "Global Distribution of Research Output") +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()
        )
    })
    
    # 5. 绘图 2: 合作网络图 (精修版)
    output$network_map <- renderPlot({
      res <- map_resources()
      edges <- edge_data()
      req(res)
      
      # 基础底图
      p <- ggplot() +
        geom_sf(data = res$world_map, fill = "#F5F5F5", color = "white", size = 0.2)
      
      # 如果有合作边，添加连线
      if (!is.null(edges) && nrow(edges) > 0) {
        p <- p + geom_curve(data = edges, 
                            aes(x = long_start, y = lat_start, xend = long_end, yend = lat_end, 
                                color = weight, 
                                alpha = weight), 
                            size = 0.3,             # 固定极细线条
                            curvature = 0.2,
                            lineend = "round")
      }
      
      # 添加节点
      # 合并统计数据和坐标
      nodes_df <- res$country_stats %>%
        left_join(res$centroids, by = "iso3") %>%
        filter(!is.na(long))
      
      p <- p + 
        geom_point(data = nodes_df,
                   aes(x = long, y = lat, size = count), 
                   fill = "#3B528B", color = "white", shape = 21, stroke = 0.5) +
        scale_size_continuous(range = c(2, 12), name = "No. of Papers") +
        scale_color_viridis_c(option = "plasma", name = "Link Strength", begin = 0.1, end = 0.9) +
        scale_alpha_continuous(range = c(0.2, 0.9), guide = "none") +
        coord_sf(ylim = c(-55, 90)) + # 略微调整视野
        labs(title = "International Collaboration Network", 
             subtitle = "Nodes represent publication volume; Lines represent co-authorship",
             x = NULL, y = NULL) +
        theme_void() +
        theme(
          legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, color = "grey50"),
          legend.box = "horizontal"
        )
      
      print(p)
    })
    
  })
}