library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(DT)
library(metafor)
library(shinythemes)
library(ggplot2)
library(fresh)
library(dplyr)
library(tidyr)
library(purrr)
library(ggpattern)
library(ggforce)
library(RColorBrewer)
library(scales)
library(countrycode)   # 新增：用于国家名称标准化
library(rnaturalearth) # 新增：获取世界地图数据
library(sf)            # 新增：处理地理空间数据

# --- 1. 辅助函数 ---
departmentHeader <- function(title, subtitle, icon = "📊") {
  div(class = "department-header",
      h2(paste(icon, title), style = "margin: 0; font-weight: 700; color: #2C3E50;"),
      p(subtitle, style = "margin: 5px 0 0 0; opacity: 0.8; font-size: 0.9rem;")
  )
}

# --- 2. 加载模块 ---
setwd("C://BaiduSyncdisk/DT20250220/20251017RCTagent/03online")

source("00meta_db_example.R")
source("01umar_database_module22.R")
source("02department_analysis_module3.R")
# source("03networkModule.R") # --- [已移除] Network Analysis 模块 ---
# source("04Evidence Map2.R") # --- [已移除] Evidence Map 模块 ---
source("05UMARforestplot8.R")
source("06agent_Forest10.R")
source("07manuForest10.R")
# source("08LLMbenchmark_module2.R") # --- [已移除] LLM Benchmark 模块 ---
source("09About_module.R")
source("10global_distribution_module6.R") # 全球分布模块

# --- 3. 主题设置 ---
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#00695C", # 调整为更深沉的蓝绿色
    aqua = "#26A69A",
    green = "#2ECC71",
    yellow = "#F1C40F",
    red = "#E74C3C"
  ),
  adminlte_sidebar(
    dark_bg = "#263238",
    dark_hover_bg = "#37474F",
    dark_color = "#ECEFF1"
  ),
  adminlte_global(
    content_bg = "#F4F6F9",
    box_bg = "#FFFFFF",
    info_box_bg = "#FFFFFF"
  )
)

meta_db <- readRDS("database251102")
meta_db$`Clinical Department` <-  meta_db$Clinical_Department
umar_data_clean <- meta_db

 
# --- 5. UI 定义 ---
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    use_theme(mytheme),
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600;800&family=Roboto:wght@300;400;500&display=swap"),
      tags$style(HTML("
        /* 全局字体设置 */
        body {
          font-family: 'Roboto', sans-serif;
          background-color: #F4F6F9;
        }
        h1, h2, h3, h4, h5, h6 {
          font-family: 'Montserrat', sans-serif;
        }

        /* --- [优化] 顶部 Banner --- */
        .main-title-container {
          /* 使用深邃的海洋渐变，呼应 Compass 主题 */
          background: linear-gradient(135deg, #004d40 0%, #00695c 50%, #00897b 100%);
          color: white;
          padding: 45px 0;
          margin-bottom: 0;
          box-shadow: 0 4px 20px rgba(0,0,0,0.2);
          position: relative;
          overflow: hidden;
        }
        
        /* 背景装饰纹理 (可选) */
        .main-title-container::before {
          content: '';
          position: absolute;
          top: 0; left: 0; right: 0; bottom: 0;
          background: radial-gradient(circle at 20% 50%, rgba(255,255,255,0.1) 0%, transparent 25%);
          pointer-events: none;
        }
        
        .main-title {
          font-weight: 800;
          font-size: 3.5rem;
          margin: 0;
          letter-spacing: 2px;
          text-shadow: 0 2px 10px rgba(0,0,0,0.3);
          display: flex;
          align-items: center;
          justify-content: center;
        }
        
        .compass-icon-style {
          margin-right: 20px;
          font-size: 3.2rem;
          color: #80CBC4; /* 浅青色图标，使其在深色背景上突出 */
          animation: floatIcon 6s ease-in-out infinite;
        }

        @keyframes floatIcon {
            0% { transform: translateY(0px); }
            50% { transform: translateY(-5px); }
            100% { transform: translateY(0px); }
        }
        
        .main-subtitle {
          font-size: 1.3rem;
          opacity: 0.95;
          margin-top: 12px;
          font-weight: 300;
          letter-spacing: 0.8px;
        }

        /* --- 导航栏容器 --- */
        .nav-container {
          background: white;
          padding: 0;
          box-shadow: 0 2px 8px rgba(0,0,0,0.08);
          position: sticky;
          top: 0;
          z-index: 1000;
        }
        
        .nav-tabs-custom {
          margin-bottom: 0;
          background: white;
          border: none;
        }
        
        .nav-tabs-custom .nav-tabs {
          border-bottom: none;
          display: flex;
          justify-content: center;
          flex-wrap: wrap;
        }
        
        .nav-tabs-custom .nav-tabs > li > a {
          color: #546E7A;
          border: none;
          margin: 0 2px;
          font-weight: 600;
          padding: 18px 25px;
          font-size: 15px;
          transition: all 0.3s ease;
          border-bottom: 3px solid transparent;
          border-radius: 0;
        }
        
        .nav-tabs-custom .nav-tabs > li > a:hover {
          color: #00695C;
          background-color: rgba(0, 105, 92, 0.04);
        }
        
        /* 激活状态的标签 */
        .nav-tabs-custom .nav-tabs > li.active > a {
          color: #00695C;
          background: white;
          border: none;
          border-bottom: 3px solid #00695C;
        }
        
        .nav-tabs-custom .nav-tabs > li.active > a:hover {
          background: white;
          color: #00695C;
        }

        /* 内容区域 */
        .tab-content {
          padding: 25px;
          min-height: 80vh;
          background-color: #F4F6F9;
        }
        
        /* 模块容器卡片化优化 */
        .box {
          border-top: 3px solid #00695C;
          box-shadow: 0 2px 8px rgba(0,0,0,0.08);
          border-radius: 6px;
          transition: transform 0.3s;
        }
        .box:hover {
          box-shadow: 0 4px 12px rgba(0,0,0,0.12);
        }
      "))
    ),
    
    # 顶部标题横幅
    fluidRow(
      class = "main-title-container",
      column(12,
             div(style = "text-align: center;",
                 # --- [修改] 添加指南针图标 ---
                 h1(
                   tags$span(icon("compass"), class = "compass-icon-style"),
                   "COMPASS", 
                   class = "main-title"
                 ),
                 p("Comprehensive Online Meta-Analysis Platform System", 
                   style = "font-size: 1.5rem; margin-top: 8px; font-weight: 600; color: #E0F2F1;"),
                 p("Empowering Evidence Synthesis with AI Agents and Advanced Analytics", 
                   class = "main-subtitle")
             )
      )
    ),
    
    # 导航栏区域
    fluidRow(
      class = "nav-container",
      column(12,
             div(class = "nav-tabs-custom",
                 tabsetPanel(
                   id = "main_tabs",
                   type = "tabs",
                   
                   # 1. Search Database
                   tabPanel("Search Database", 
                            icon = icon("search"),
                            externalMetaUI("external_meta_module")),
                   
                   # 2. Global Distribution (调整到第二位)
                   tabPanel("Global Distribution", 
                            icon = icon("globe-americas"),
                            globalDistributionUI("global_distribution_module")),
                   
                   # 3. Department Analysis (原第二位，现第三位)
                   tabPanel("Department Overview", 
                            icon = icon("hospital-user"),
                            departmentAnalysisUI("department_analysis_module")),
                   
                   # 4. Meta-Analysis (原第五位，现第四位)
                   tabPanel("Meta-Analysis", 
                            icon = icon("database"),
                            keywordSearchUI("keyword_search_module")),
                   
                   # 5. AI Agent (原第六位，现第五位)
                   tabPanel("AI Agent Review", 
                            icon = icon("robot"),
                            systematicReviewUI("systematic_review_module")),
                   
                   # 6. Manual Analysis (原第七位，现第六位)
                   tabPanel("Manual Analysis", 
                            icon = icon("edit"),
                            ManualMetaAnalysisUI("ManualMetaAnalysis")),
                   
                   # 7. About (原第九位，现第七位)
                   tabPanel("About", 
                            icon = icon("info-circle"),
                            aboutUI("about_module"))
                 )
             )
      )
    )
  )
)

# --- 6. Server 逻辑 ---
server <- function(input, output, session) {
  
  # 1. 数据库展示
  externalMetaServer("external_meta_module", meta_db = meta_db)
  
  # 2. 全球分布
  globalDistributionServer("global_distribution_module", meta_db = meta_db)
  
  # 3. 科室分析
  departmentAnalysisServer("department_analysis_module", umar_data = umar_data_clean)
  
  # 4. 关键词搜索/Meta
  keywordSearchServer("keyword_search_module", meta_db = umar_data_clean)
  
  # 5. AI Agent
  systematicReviewServer("systematic_review_module", meta_db = meta_db)
  
  # 6. 手动分析
  ManualMetaAnalysisServer("ManualMetaAnalysis")
  
  # 7. 关于
  aboutServer("about_module")
}

# 运行应用
shinyApp(ui, server)