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
  
  # 禁用默认头部和侧边栏，使用自定义布局
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    use_theme(mytheme),
    
    # --- 加载外部字体和图标库 ---
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;500;600;700&family=Roboto:wght@300;400;500&family=JetBrains+Mono:wght@400&display=swap"),
      # 引入 Animate.css 增加动效 (可选，这里手写简单的)
      
      tags$style(HTML("
        /* =========================================
           1. 全局基础设置 (Global Base)
           ========================================= */
        :root {
          --primary-color: #00695C;
          --secondary-color: #26A69A;
          --accent-color: #80CBC4;
          --bg-color: #F4F6F9;
          --text-dark: #2C3E50;
          --text-light: #546E7A;
          --card-shadow: 0 10px 25px -5px rgba(0, 0, 0, 0.05), 0 8px 10px -6px rgba(0, 0, 0, 0.01);
          --hover-shadow: 0 20px 25px -5px rgba(0, 0, 0, 0.1), 0 10px 10px -5px rgba(0, 0, 0, 0.04);
        }

        body {
          font-family: 'Roboto', sans-serif;
          background-color: var(--bg-color);
          color: var(--text-dark);
          line-height: 1.6;
        }
        
        h1, h2, h3, h4, h5, h6 {
          font-family: 'Montserrat', sans-serif;
          color: var(--text-dark);
        }

        /* 自定义滚动条 */
        ::-webkit-scrollbar {
          width: 8px;
          height: 8px;
        }
        ::-webkit-scrollbar-track {
          background: #f1f1f1; 
        }
        ::-webkit-scrollbar-thumb {
          background: #B0BEC5; 
          border-radius: 4px;
        }
        ::-webkit-scrollbar-thumb:hover {
          background: var(--secondary-color); 
        }

        /* =========================================
           2. 顶部 Banner (Hero Section)
           ========================================= */
        .main-title-container {
          background: linear-gradient(135deg, #004d40 0%, #00695C 100%);
          color: white;
          padding: 60px 0 50px 0;
          position: relative;
          overflow: hidden;
          border-bottom: 1px solid rgba(255,255,255,0.1);
        }
        
        /* 动态背景装饰圆 */
        .main-title-container::after {
          content: '';
          position: absolute;
          top: -50px; right: -50px;
          width: 300px; height: 300px;
          background: radial-gradient(circle, rgba(255,255,255,0.05) 0%, transparent 70%);
          border-radius: 50%;
        }

        .compass-icon-style {
          font-size: 3.5rem;
          color: var(--accent-color);
          margin-right: 25px;
          filter: drop-shadow(0 0 10px rgba(128, 203, 196, 0.3));
          animation: pulseIcon 3s infinite ease-in-out;
        }

        @keyframes pulseIcon {
          0% { transform: scale(1); opacity: 0.9; }
          50% { transform: scale(1.05); opacity: 1; }
          100% { transform: scale(1); opacity: 0.9; }
        }

        .main-title {
          font-weight: 800;
          font-size: 3.8rem;
          letter-spacing: 3px;
          text-transform: uppercase;
          margin: 0;
        }

        .main-subtitle {
          font-size: 1.2rem;
          font-weight: 300;
          color: #E0F2F1;
          letter-spacing: 1px;
          margin-top: 10px;
          opacity: 0.9;
        }

        /* =========================================
           3. 导航栏 (Navigation)
           ========================================= */
        .nav-container {
          position: sticky;
          top: 0;
          z-index: 1000;
          background: rgba(255, 255, 255, 0.95);
          backdrop-filter: blur(10px); /* 磨砂玻璃效果 */
          box-shadow: 0 4px 20px rgba(0,0,0,0.06);
          padding: 0 20px;
        }

        .nav-tabs-custom { margin-bottom: 0; }
        
        .nav-tabs-custom .nav-tabs {
          border-bottom: none;
          display: flex;
          justify-content: center;
        }
        
        .nav-tabs-custom .nav-tabs > li > a {
          color: var(--text-light);
          font-weight: 600;
          padding: 20px 25px;
          font-size: 15px;
          border: none;
          border-bottom: 3px solid transparent;
          transition: all 0.3s ease;
          background: transparent;
        }
        
        .nav-tabs-custom .nav-tabs > li > a:hover {
          color: var(--primary-color);
          background: rgba(0, 105, 92, 0.03);
        }
        
        .nav-tabs-custom .nav-tabs > li.active > a {
          color: var(--primary-color);
          border-bottom: 3px solid var(--primary-color);
          background: transparent;
        }

        /* =========================================
           4. 内容容器与卡片 (Content & Cards)
           ========================================= */
        .tab-content {
          padding: 30px;
          min-height: 80vh;
          animation: fadeIn 0.5s ease-out;
        }
        
        @keyframes fadeIn {
          from { opacity: 0; transform: translateY(10px); }
          to { opacity: 1; transform: translateY(0); }
        }

        /* 覆盖 AdminLTE 盒子样式，改为现代卡片 */
        .box {
          border: none !important;
          border-radius: 16px !important;
          background: #fff !important;
          box-shadow: var(--card-shadow) !important;
          transition: transform 0.3s cubic-bezier(0.4, 0, 0.2, 1), box-shadow 0.3s ease;
          margin-bottom: 30px !important;
          overflow: hidden; /* 防止内容溢出圆角 */
        }
        
        .box:hover {
          transform: translateY(-4px);
          box-shadow: var(--hover-shadow) !important;
        }

        .box-header {
          background-color: transparent;
          border-bottom: 1px solid #f0f0f0;
          padding: 15px 20px;
        }
        
        .box-title {
          font-family: 'Montserrat', sans-serif;
          font-weight: 700;
          font-size: 1.1rem;
          color: var(--primary-color);
        }

        .box-body {
          padding: 20px !important;
        }

        /* =========================================
           5. 输入控件美化 (Inputs & Buttons)
           ========================================= */
        /* 输入框 */
        .form-control, .selectize-input {
          border-radius: 10px !important;
          border: 1px solid #CFD8DC !important;
          background-color: #FAFAFA !important;
          padding: 10px 15px !important;
          height: auto !important;
          box-shadow: none !important;
          transition: border-color 0.3s, background-color 0.3s;
        }
        
        .form-control:focus, .selectize-input.focus {
          border-color: var(--primary-color) !important;
          background-color: #fff !important;
          box-shadow: 0 0 0 3px rgba(0, 105, 92, 0.1) !important;
        }

        /* 按钮 */
        .btn {
          border-radius: 50px !important; /* 胶囊按钮 */
          padding: 8px 25px !important;
          font-weight: 600 !important;
          letter-spacing: 0.5px;
          transition: all 0.3s ease !important;
          box-shadow: 0 2px 6px rgba(0,0,0,0.1);
          border: none !important;
        }

        .btn-primary {
          background: linear-gradient(135deg, var(--secondary-color) 0%, var(--primary-color) 100%) !important;
        }
        
        .btn-primary:hover {
          filter: brightness(1.1);
          transform: translateY(-1px);
          box-shadow: 0 4px 12px rgba(0, 105, 92, 0.3);
        }

        /* DT 表格美化 */
        table.dataTable thead th {
          background-color: #f8f9fa;
          color: var(--text-dark);
          font-weight: 600;
          border-bottom: 2px solid var(--primary-color) !important;
        }
        table.dataTable.no-footer {
          border-bottom: 1px solid #eee !important;
        }

        /* =========================================
           6. 页脚 (Footer)
           ========================================= */
        .app-footer {
          background-color: #fff;
          padding: 40px 0;
          margin-top: 60px;
          border-top: 1px solid #eee;
          text-align: center;
          color: var(--text-light);
        }
        .footer-logo {
          font-weight: 700;
          color: var(--primary-color);
          font-size: 1.2rem;
          margin-bottom: 10px;
          display: block;
        }
      "))
    ),
    
    # --- Header Banner ---
    fluidRow(
      class = "main-title-container",
      column(12,
             div(style = "text-align: center;",
                 h1(
                   tags$span(icon("compass"), class = "compass-icon-style"),
                   "COMPASS", 
                   class = "main-title"
                 ),
                 p("Comprehensive Online Meta-Analysis Platform System", 
                   style = "font-size: 1.4rem; margin-top: 8px; font-weight: 600; color: #E0F2F1;"),
                 div(
                   style = "display: inline-block; background: rgba(255,255,255,0.1); padding: 5px 20px; border-radius: 20px; margin-top: 15px;",
                   p("Empowering Evidence Synthesis with AI Agents", class = "main-subtitle", style="margin:0;")
                 )
             )
      )
    ),
    
    # --- Navigation ---
    fluidRow(
      class = "nav-container",
      column(12,
             div(class = "nav-tabs-custom",
                 tabsetPanel(
                   id = "main_tabs",
                   type = "tabs",
                   
                   tabPanel("Search Database", 
                            icon = icon("search"),
                            # 可以在这里加一个简单的 Wrapper div 来增加特定的 padding
                            div(style="margin-top: 20px;", externalMetaUI("external_meta_module"))
                   ),
                   
                   tabPanel("Global Distribution", 
                            icon = icon("globe-americas"),
                            div(style="margin-top: 20px;", globalDistributionUI("global_distribution_module"))
                   ),
                   
                   tabPanel("Department Overview", 
                            icon = icon("hospital-user"),
                            div(style="margin-top: 20px;", departmentAnalysisUI("department_analysis_module"))
                   ),
                   
                   tabPanel("Meta-Analysis", 
                            icon = icon("database"),
                            div(style="margin-top: 20px;", keywordSearchUI("keyword_search_module"))
                   ),
                   
                   tabPanel("AI Agent Review", 
                            icon = icon("robot"),
                            div(style="margin-top: 20px;", systematicReviewUI("systematic_review_module"))
                   ),
                   
                   tabPanel("Manual Analysis", 
                            icon = icon("edit"),
                            div(style="margin-top: 20px;", ManualMetaAnalysisUI("ManualMetaAnalysis"))
                   ),
                   
                   tabPanel("About", 
                            icon = icon("info-circle"),
                            div(style="margin-top: 20px;", aboutUI("about_module"))
                   )
                 )
             )
      )
    ),
    
    # --- Footer ---
    fluidRow(
      class = "app-footer",
      column(12,
             tags$span("COMPASS Platform", class = "footer-logo"),
             p("© 2025 Evidence-Based Medicine AI Lab. All Rights Reserved."),
             p("Powered by R Shiny, Metafor & LLM Agents", style = "font-size: 0.8rem; opacity: 0.7;")
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