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
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
      tags$style(HTML("
        /* 全局字体设置 */
        body {
          font-family: 'Roboto', sans-serif;
          background: linear-gradient(135deg, #f5f7fa 0%, #e4e8f0 100%);
          min-height: 100vh;
        }
        h1, h2, h3, h4, h5, h6 {
          font-family: 'Montserrat', sans-serif;
        }

        /* --- [优化] 顶部 Banner --- */
        .main-title-container {
          background: linear-gradient(135deg, 
            #004d40 0%, 
            #00695c 30%, 
            #00897b 70%, 
            #26A69A 100%);
          color: white;
          padding: 50px 0;
          margin-bottom: 0;
          box-shadow: 0 8px 32px rgba(0,0,0,0.1);
          position: relative;
          overflow: hidden;
          border-bottom: 1px solid rgba(255,255,255,0.1);
        }
        
        /* 背景装饰纹理 */
        .main-title-container::before {
          content: '';
          position: absolute;
          top: 0; left: 0; right: 0; bottom: 0;
          background: 
            radial-gradient(circle at 20% 80%, rgba(255,255,255,0.1) 0%, transparent 50%),
            radial-gradient(circle at 80% 20%, rgba(255,255,255,0.05) 0%, transparent 50%),
            radial-gradient(circle at 40% 40%, rgba(255,255,255,0.08) 0%, transparent 50%);
          pointer-events: none;
        }
        
        .main-title {
          font-weight: 800;
          font-size: 3.8rem;
          margin: 0;
          letter-spacing: 3px;
          text-shadow: 0 4px 12px rgba(0,0,0,0.3);
          display: flex;
          align-items: center;
          justify-content: center;
          background: linear-gradient(135deg, #FFFFFF 0%, #E0F2F1 100%);
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
          background-clip: text;
        }
        
        .compass-icon-style {
          margin-right: 25px;
          font-size: 3.5rem;
          background: linear-gradient(135deg, #80CBC4 0%, #E0F2F1 100%);
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
          background-clip: text;
          animation: floatIcon 8s ease-in-out infinite;
          filter: drop-shadow(0 4px 8px rgba(0,0,0,0.2));
        }

        @keyframes floatIcon {
            0% { transform: translateY(0px) rotate(0deg); }
            25% { transform: translateY(-8px) rotate(5deg); }
            50% { transform: translateY(-4px) rotate(0deg); }
            75% { transform: translateY(-6px) rotate(-5deg); }
            100% { transform: translateY(0px) rotate(0deg); }
        }
        
        .main-subtitle {
          font-size: 1.4rem;
          opacity: 0.95;
          margin-top: 15px;
          font-weight: 400;
          letter-spacing: 1px;
          color: #E0F2F1;
          text-shadow: 0 2px 4px rgba(0,0,0,0.2);
        }

        .main-subtitle-secondary {
          font-size: 1.1rem;
          opacity: 0.8;
          margin-top: 8px;
          font-weight: 300;
          color: #B2DFDB;
        }

        /* --- 导航栏容器 --- */
        .nav-container {
          background: rgba(255, 255, 255, 0.95);
          backdrop-filter: blur(10px);
          padding: 0;
          box-shadow: 0 4px 20px rgba(0,0,0,0.08);
          position: sticky;
          top: 0;
          z-index: 1000;
          border-bottom: 1px solid rgba(255,255,255,0.2);
        }
        
        .nav-tabs-custom {
          margin-bottom: 0;
          background: transparent;
          border: none;
        }
        
        .nav-tabs-custom .nav-tabs {
          border-bottom: none;
          display: flex;
          justify-content: center;
          flex-wrap: wrap;
          padding: 0 20px;
        }
        
        .nav-tabs-custom .nav-tabs > li > a {
          color: #546E7A;
          border: none;
          margin: 0 3px;
          font-weight: 600;
          padding: 20px 28px;
          font-size: 15px;
          transition: all 0.4s cubic-bezier(0.25, 0.46, 0.45, 0.94);
          border-bottom: 3px solid transparent;
          border-radius: 8px 8px 0 0;
          position: relative;
          overflow: hidden;
        }
        
        .nav-tabs-custom .nav-tabs > li > a::before {
          content: '';
          position: absolute;
          top: 0;
          left: -100%;
          width: 100%;
          height: 100%;
          background: linear-gradient(90deg, transparent, rgba(0, 105, 92, 0.1), transparent);
          transition: left 0.6s;
        }
        
        .nav-tabs-custom .nav-tabs > li > a:hover::before {
          left: 100%;
        }
        
        .nav-tabs-custom .nav-tabs > li > a:hover {
          color: #00695C;
          background: linear-gradient(135deg, rgba(0, 105, 92, 0.05) 0%, rgba(38, 166, 154, 0.05) 100%);
          transform: translateY(-2px);
          box-shadow: 0 4px 12px rgba(0, 105, 92, 0.15);
        }
        
        /* 激活状态的标签 */
        .nav-tabs-custom .nav-tabs > li.active > a {
          color: #00695C;
          background: linear-gradient(135deg, rgba(255,255,255,0.9) 0%, rgba(248,250,252,0.9) 100%);
          border: none;
          border-bottom: 3px solid #00695C;
          box-shadow: 0 4px 16px rgba(0, 105, 92, 0.2);
          transform: translateY(-1px);
        }
        
        .nav-tabs-custom .nav-tabs > li.active > a:hover {
          background: linear-gradient(135deg, rgba(255,255,255,0.95) 0%, rgba(248,250,252,0.95) 100%);
          color: #00695C;
        }

        /* 内容区域 */
        .tab-content {
          padding: 30px;
          min-height: 80vh;
          background: transparent;
        }
        
        /* 模块容器卡片化优化 */
        .box {
          border: none;
          border-top: 4px solid #00695C;
          box-shadow: 
            0 4px 20px rgba(0,0,0,0.08),
            0 2px 8px rgba(0,0,0,0.04);
          border-radius: 12px;
          transition: all 0.4s cubic-bezier(0.25, 0.46, 0.45, 0.94);
          background: rgba(255, 255, 255, 0.95);
          backdrop-filter: blur(10px);
          overflow: hidden;
          position: relative;
        }
        
        .box::before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          height: 4px;
          background: linear-gradient(90deg, #00695C, #26A69A, #80CBC4);
          opacity: 0;
          transition: opacity 0.4s;
        }
        
        .box:hover {
          box-shadow: 
            0 8px 32px rgba(0,0,0,0.12),
            0 4px 16px rgba(0,0,0,0.06);
          transform: translateY(-4px);
        }
        
        .box:hover::before {
          opacity: 1;
        }
        
        .box-header {
          background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
          border-bottom: 1px solid rgba(0,0,0,0.05);
          padding: 20px;
          border-radius: 12px 12px 0 0;
        }
        
        .box-title {
          font-weight: 700;
          color: #2C3E50;
          font-size: 1.3rem;
          display: flex;
          align-items: center;
          gap: 10px;
        }
        
        /* 自定义滚动条 */
        ::-webkit-scrollbar {
          width: 8px;
          height: 8px;
        }
        
        ::-webkit-scrollbar-track {
          background: rgba(0,0,0,0.05);
          border-radius: 4px;
        }
        
        ::-webkit-scrollbar-thumb {
          background: linear-gradient(135deg, #00695C, #26A69A);
          border-radius: 4px;
        }
        
        ::-webkit-scrollbar-thumb:hover {
          background: linear-gradient(135deg, #004D40, #00897B);
        }
        
        /* 加载动画 */
        @keyframes fadeInUp {
          from {
            opacity: 0;
            transform: translateY(30px);
          }
          to {
            opacity: 1;
            transform: translateY(0);
          }
        }
        
        .tab-pane {
          animation: fadeInUp 0.6s ease-out;
        }
        
        /* 按钮美化 */
        .btn {
          border-radius: 8px;
          font-weight: 600;
          transition: all 0.3s ease;
          border: none;
          padding: 10px 20px;
        }
        
        .btn-primary {
          background: linear-gradient(135deg, #00695C, #00897B);
          box-shadow: 0 4px 12px rgba(0, 105, 92, 0.3);
        }
        
        .btn-primary:hover {
          background: linear-gradient(135deg, #004D40, #00695C);
          transform: translateY(-2px);
          box-shadow: 0 6px 20px rgba(0, 105, 92, 0.4);
        }
        
        /* 响应式设计 */
        @media (max-width: 768px) {
          .main-title {
            font-size: 2.5rem;
            letter-spacing: 1px;
          }
          
          .compass-icon-style {
            font-size: 2.2rem;
            margin-right: 15px;
          }
          
          .main-subtitle {
            font-size: 1.1rem;
          }
          
          .nav-tabs-custom .nav-tabs > li > a {
            padding: 15px 20px;
            font-size: 14px;
          }
        }
      "))
    ),
    
    # 顶部标题横幅
    fluidRow(
      class = "main-title-container",
      column(12,
             div(style = "text-align: center; position: relative; z-index: 2;",
                 # --- [优化] 指南针图标和标题 ---
                 h1(
                   tags$span(icon("compass"), class = "compass-icon-style"),
                   "COMPASS", 
                   class = "main-title"
                 ),
                 p("Comprehensive Online Meta-Analysis Platform System", 
                   class = "main-subtitle"),
                 p("Empowering Evidence Synthesis with AI Agents and Advanced Analytics", 
                   class = "main-subtitle-secondary")
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
                   tabPanel("Search", 
                            icon = icon("search", class = "fa-lg"),
                            externalMetaUI("external_meta_module")),
                   
                   # 2. Global Distribution (调整到第二位)
                   tabPanel("Global", 
                            icon = icon("globe-americas", class = "fa-lg"),
                            globalDistributionUI("global_distribution_module")),
                   
                   # 3. Department Analysis (原第二位，现第三位)
                   tabPanel("Department", 
                            icon = icon("hospital-user", class = "fa-lg"),
                            departmentAnalysisUI("department_analysis_module")),
                   
                   # 4. Meta-Analysis (原第五位，现第四位)
                   tabPanel("Meta-Analysis", 
                            icon = icon("database", class = "fa-lg"),
                            keywordSearchUI("keyword_search_module")),
                   
                   # 5. AI Agent (原第六位，现第五位)
                   tabPanel("AI-Agent Analysis", 
                            icon = icon("robot", class = "fa-lg"),
                            systematicReviewUI("systematic_review_module")),
                   
                   # 6. Manual Analysis (原第七位，现第六位)
                   tabPanel("Manual Analysis", 
                            icon = icon("edit", class = "fa-lg"),
                            ManualMetaAnalysisUI("ManualMetaAnalysis")),
                   
                   # 7. About (原第九位，现第七位)
                   tabPanel("About", 
                            icon = icon("info-circle", class = "fa-lg"),
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