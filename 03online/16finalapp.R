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

# --- 1. 辅助函数 ---
# 统一的顶部卡片函数
departmentHeader <- function(title, subtitle, icon = "📊") {
  div(class = "department-header",
      h2(paste(icon, title), style = "margin: 0; font-weight: 700; color: #2C3E50;"),
      p(subtitle, style = "margin: 5px 0 0 0; opacity: 0.8; font-size: 0.9rem;")
  )
}

# --- 2. 加载模块 (保留您的原始路径) ---
# 注意：请确保路径在您本地是正确的
setwd("C://BaiduSyncdisk/DT20250220/20251017RCTagent/03online")

source("00meta_db_example.R")
source("01umar_database_module22.R")
source("02department_analysis_module3.R")
source("03networkModule.R")
source("04Evidence Map2.R")
source("05UMARforestplot5.R")
source("06agent_Forest10.R")
source("07manuForest10.R")
source("08LLMbenchmark_module2.R")
source("09About_module.R")

# --- 3. 主题设置 (使用 Fresh 库进行美化) ---
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#00796B", # 更专业的深青色
    aqua = "#009688",
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
umar_data_clean <-meta_db
# 定义 meta_db_network 示例数据集 (保留原逻辑)
meta_db_network <- local({
  set.seed(123)
  n_studies <- 200
  
  departments <- c("Cardiology", "Neurology", "Oncology", "Psychiatry", 
                   "Surgery", "Pediatrics", "Radiology", "Pathology",
                   "Endocrinology", "Gastroenterology", "Nephrology",
                   "Pulmonology", "Dermatology", "Orthopedics", "Urology")
  
  diseases <- c("Hypertension", "Diabetes", "Cancer", "Depression", 
                "Arthritis", "Asthma", "Stroke", "Heart Failure",
                "COPD", "Osteoporosis", "Alzheimer's", "Parkinson's",
                "HIV", "Hepatitis", "Sepsis", "Pneumonia", "Tuberculosis",
                "Malaria", "Dengue", "COVID-19")
  
  data <- data.frame(
    Clinical_Department = sample(departments, n_studies, replace = TRUE),
    Disease = sample(diseases, n_studies, replace = TRUE),
    PMID = sample(30000000:40000000, n_studies),
    Effect_Size = rnorm(n_studies, mean = 0, sd = 2),
    P_Value = runif(n_studies, 0, 0.1),
    Num_Studies = sample(1:20, n_studies, replace = TRUE),
    Total_Sample_Size = sample(50:5000, n_studies, replace = TRUE)
  )
  
  data <- data %>%
    mutate(
      CI_Lower = Effect_Size - abs(rnorm(n(), 0.5, 0.3)),
      CI_Upper = Effect_Size + abs(rnorm(n(), 0.5, 0.3)),
      Literature_Abbreviation = paste0("Study_", 1:n()),
      Effect_Direction = case_when(
        Effect_Size > 0 & P_Value < 0.05 ~ "Positive",
        Effect_Size < 0 & P_Value < 0.05 ~ "Negative",
        TRUE ~ "Not Significant"
      ),
      CI_Width = CI_Upper - CI_Lower
    )
  
  return(data)
})

# LLM Benchmarking 数据加载 (保留原逻辑)
load_llm_benchmark_data <- function() {
  if (file.exists("Benchmark251122.csv")) {
    data <- read.csv("Benchmark251122.csv")
  } else {
    set.seed(123)
    models <- c("GPT-4", "Claude-3", "Gemini-Pro", "LLaMA-2", "Mistral", 
                "ChatGLM", "Baichuan", "Qwen", "InternLM", "Yi-34B")
    
    data <- data.frame(
      model_name = models,
      Developer = c("OpenAI", "Anthropic", "Google", "Meta", "Mistral AI",
                    "Zhipu AI", "Baichuan Inc", "Alibaba", "Shanghai AI", "01.ai"),
      Release_Year = sample(2020:2024, 10),
      Context_Length_K_tokens = sample(32:128, 10),
      Total_Parameters_B = round(runif(10, 7, 70), 1),
      avg_response_time = round(runif(10, 1.5, 8.5), 1),
      match_success_rate = round(runif(10, 0.65, 0.95), 3),
      studies_accuracy = round(runif(10, 0.70, 0.98), 3),
      sample_accuracy = round(runif(10, 0.75, 0.96), 3),
      extract_accuracy = round(runif(10, 0.68, 0.94), 3),
      amstar_non_critical_accuracy = round(runif(10, 0.72, 0.97), 3),
      amstar_critical_accuracy = round(runif(10, 0.65, 0.92), 3),
      amstar_accuracy = round(runif(10, 0.70, 0.95), 3),
      overall_score = round(runif(10, 0.72, 0.96), 3)
    )
  }
  return(data)
}

# --- 5. UI 定义 ---
ui <- dashboardPage(
  skin = "blue",
  
  # 移除默认Header和Sidebar
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    use_theme(mytheme),
    tags$head(
      # 引入 Google Fonts，使界面更现代
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

        /* 静态标题区域 - COMAPSS 风格 */
        .main-title-container {
          background: linear-gradient(120deg, #1a2a6c, #b21f1f, #fdbb2d); /* 复古未来科技感渐变 */
          background: linear-gradient(135deg, #0F2027 0%, #203A43 50%, #2C5364 100%); /* 或者这种深沉的专业风格 */
          color: white;
          padding: 40px 0;
          margin-bottom: 0;
          box-shadow: 0 4px 15px rgba(0,0,0,0.15);
        }
        
        .main-title {
          font-weight: 800;
          font-size: 3.2rem;
          margin: 0;
          letter-spacing: 1px;
          text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
        }
        
        .main-subtitle {
          font-size: 1.2rem;
          opacity: 0.9;
          margin-top: 10px;
          font-weight: 300;
          letter-spacing: 0.5px;
        }

        /* 导航栏容器 */
        .nav-container {
          background: white;
          padding: 0;
          box-shadow: 0 2px 5px rgba(0,0,0,0.05);
          position: sticky;
          top: 0;
          z-index: 1000;
        }
        
        /* 导航标签优化 */
        .nav-tabs-custom {
          margin-bottom: 0;
          background: white;
          border: none;
        }
        
        .nav-tabs-custom .nav-tabs {
          border-bottom: none;
          display: flex;
          justify-content: center; /* 居中显示标签 */
          flex-wrap: wrap;
        }
        
        .nav-tabs-custom .nav-tabs > li > a {
          color: #546E7A;
          border: none;
          margin-right: 5px;
          font-weight: 600;
          padding: 18px 25px;
          font-size: 15px;
          transition: all 0.3s ease;
          border-bottom: 3px solid transparent;
        }
        
        .nav-tabs-custom .nav-tabs > li > a:hover {
          color: #00796B;
          background-color: rgba(0, 121, 107, 0.05);
        }
        
        /* 激活状态的标签 */
        .nav-tabs-custom .nav-tabs > li.active > a {
          color: #00796B;
          background: white;
          border: none;
          border-bottom: 3px solid #00796B;
        }
        
        .nav-tabs-custom .nav-tabs > li.active > a:hover {
          background: white;
          color: #00796B;
        }

        /* 内容区域 */
        .tab-content {
          padding: 20px;
          min-height: 80vh;
          background-color: #F4F6F9;
        }
        
        /* 模块容器卡片化 */
        .box {
          border-top: 3px solid #00796B;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1);
          border-radius: 8px;
        }
      "))
    ),
    
    # 顶部标题横幅
    fluidRow(
      class = "main-title-container",
      column(12,
             div(style = "text-align: center;",
                 h1("COMAPSS", class = "main-title"),
                 p("Comprehensive Online Meta-Analysis Platform System", 
                   style = "font-size: 1.6rem; margin-top: 5px; font-weight: 600;"),
                 p("Empowering Evidence Synthesis with AI Agents and Advanced Analytics", 
                   class = "main-subtitle")
             )
      )
    ),
    
    # 导航栏区域 (移除 UMAR 标签)
    fluidRow(
      class = "nav-container",
      column(12,
             div(class = "nav-tabs-custom",
                 tabsetPanel(
                   id = "main_tabs",
                   type = "tabs",
                   
                   # 1. Search
                   tabPanel("Search Database", 
                            icon = icon("search"),
                            externalMetaUI("external_meta_module")),
                   
                   # 2. Department Analysis
                   tabPanel("Department Overview", 
                            icon = icon("hospital-user"),
                            departmentAnalysisUI("department_analysis_module")),
                   
                   # 3. Network Analysis
                   tabPanel("Network Analysis", 
                            icon = icon("project-diagram"),
                            networkAnalysisUI("network_analysis_module")),
                   
                   # 4. Evidence Map
                   tabPanel("Evidence Map", 
                            icon = icon("globe-americas"),
                            evidenceMapUI("evidence_map_module")),
                   
                   # 5. Keyword/Meta Search
                   tabPanel("Meta-Analysis", 
                            icon = icon("database"),
                            keywordSearchUI("keyword_search_module")),
                   
                   # 6. AI Agent
                   tabPanel("AI Agent Review", 
                            icon = icon("robot"),
                            systematicReviewUI("systematic_review_module")),
                   
                   # 7. Manual Meta
                   tabPanel("Manual Analysis", 
                            icon = icon("edit"),
                            ManualMetaAnalysisUI("ManualMetaAnalysis")),
                   
                   # 8. Benchmark
                   tabPanel("LLM Benchmark", 
                            icon = icon("chart-line"),
                            llmBenchmarkingUI("llm_benchmarking_module")),
                   
                   # 9. About
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
  
  # 2. 科室分析
  departmentAnalysisServer("department_analysis_module", umar_data = umar_data_clean)
  
  # 3. 网络分析
  networkAnalysisServer("network_analysis_module", meta_db = meta_db_network)
  
  # 4. 证据地图
  # 注意：meta_db_example 需要在 00meta_db_example.R 中定义，这里直接调用
  if(exists("meta_db_example")){
    evidenceMapServer("evidence_map_module", meta_db = meta_db_example)
  } else {
    # 如果示例数据未加载，传入 meta_db 防止报错
    evidenceMapServer("evidence_map_module", meta_db = meta_db) 
  }
  
  # 5. 关键词搜索/Meta
  keywordSearchServer("keyword_search_module", meta_db = umar_data_clean)
  
  # 6. AI Agent
  systematicReviewServer("systematic_review_module", meta_db = meta_db)
  
  # 7. 手动分析
  ManualMetaAnalysisServer("ManualMetaAnalysis")
  
  # 8. LLM 跑分
  llmBenchmarkingServer("llm_benchmarking_module")
  
  # 9. 关于
  aboutServer("about_module")
}

# 运行应用
shinyApp(ui, server)