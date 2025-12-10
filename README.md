Building Reliable Evidence with AI: An Orchestrated Multi-Agent Framework and Methodological Audit of 1.2 Million Randomized Controlled Trials

🔍 Overview
This project introduces a scalable AI framework for reliable evidence synthesis by orchestrating multiple large language models to audit 1.2 million RCTs—the largest methodological evaluation of its kind. We address the critical challenge of LLM hallucination in scientific literature processing and propose a systematic solution that combines multi-agent consensus, structured quality control, and an interactive analysis platform.

The system comprises three core innovations:

ACCORD – A multi-agent orchestration framework that mitigates LLM hallucinations through task planning, weighted voting, and expert evaluation, achieving higher accuracy than any single model.

RCT Knowledge Base – A structured repository of 490,430 high-quality RCTs extracted from 1.2 million publications, enabling large-scale methodological audits.

COMPASS – The first online platform integrating multi-agent extraction with automated meta-analysis workflows, supporting real-time evidence synthesis and methodological exploration.

🚀 Explore the COMPASS Platform
🌐 👉 Visit the LIVE COMPASS Platform
Access COMPASS, our interactive platform for agent-driven evidence synthesis and methodological auditing of RCTs. The platform enables:

Automated literature screening and data extraction via the ACCORD multi-agent engine

Interactive meta-analysis with real-time forest plots and bias diagnostics

Large-scale methodological dashboards visualizing statistical power, p-hacking patterns, and temporal trends across 490,000+ RCTs

https://github.com/chenxi199506/ASAP/blob/master/COMPASS_preview.png

(If the preview does not load on GitHub, please click the link above to open the live version.)

🧩 System Architecture
ACCORD Multi-Agent Framework
The framework operates through four coordinated stages:

Task Planning – Decomposes extraction tasks into structured subtasks

Targeted Distribution – Assigns subtasks to LLMs based on benchmarked strengths

Weighted Ensemble Voting – Aggregates outputs using confidence-weighted consensus

Expert Evaluation – Implements closed-loop validation and calibration

COMPASS Platform Integration
Frontend: Interactive Shiny application for visualization and user-directed analysis

Backend: ACCORD agent orchestration engine and structured RCT database

Workflow Support: Fully automated, semi-automated, and manual meta-analysis pipelines

📊 Key Findings from 1.2 Million RCTs
Statistical Power: Only 60.4% of RCTs were adequately powered (≥80% power), though this proportion has improved over time.

P-Hacking Patterns: Prevalence follows a U-shaped distribution—highest in mid-tier journals and among both highly cited and uncited papers.

Model Performance: Benchmarking of 26 LLMs showed significant task-dependent variation, with no single model dominating across all extraction tasks.

System Accuracy: The ACCORD framework achieved higher consensus accuracy than any individual LLM, effectively mitigating hallucination through orchestrated verification.

💡 Implications & Future Directions
Reliable AI for Evidence Synthesis: Demonstrates that intelligent system design—beyond raw model capability—is critical for deploying trustworthy AI in biomedical research.

Continuous Methodological Monitoring: The structured RCT database enables ongoing audit of research quality, transparency, and statistical rigor across disciplines.

Scalable Synthesis Infrastructure: COMPASS provides a foundational platform for next-generation, high-throughput evidence synthesis that keeps pace with literature growth.

Open Science & Community Use: The platform and framework are designed for broad adoption, supporting reproducible meta-analysis and methodological research.

📁 Repository & Data Availability
COMPASS Platform: https://chatgptmodel.shinyapps.io/COMPASS/

ACCORD Framework Code: Available on GitHub (link to be added upon publication)

Structured RCT Database: Subset available for methodological research upon request

Benchmark Dataset: 1,049 expert-annotated publications for LLM evaluation

🇨🇳 中文简介
🔍 概述
本研究构建了一个可扩展的AI证据合成框架，通过协调多个大语言模型对120万项随机对照试验（RCT）进行了方法学审计——这是迄今为止规模最大的同类评估。我们解决了LLM在科学文献处理中的幻觉问题，并提出了一套结合多智能体共识、结构化质量控制与交互式分析平台的系统解决方案。

系统包含三大核心创新：

ACCORD – 一个多智能体协调框架，通过任务规划、加权投票与专家评估来缓解LLM幻觉，准确率超越任何单一模型。

RCT知识库 – 从120万篇文献中提取的490,430项高质量RCT结构化数据库，支持大规模方法学审计。

COMPASS – 首个将多智能体提取与自动化Meta分析工作流集成的在线平台，支持实时证据合成与方法学探索。

🚀 访问COMPASS平台
🌐 点击访问实时平台 👉 https://chatgptmodel.shinyapps.io/COMPASS/
平台支持：

通过ACCORD多智能体引擎实现自动化文献筛选与数据提取

交互式Meta分析，实时生成森林图与偏倚诊断

大规模方法学仪表板，可视化49万+RCT的统计功效、p-hacking模式及时序趋势

📊 主要发现
统计功效：仅60.4%的RCT具备足够功效（≥80%），但这一比例随时间逐步改善。

P-Hacking模式：呈现U型分布——在中层期刊以及高被引和零被引论文中最为普遍。

模型性能：26个LLM的基准测试显示显著的任务依赖性差异，无单一模型在所有提取任务中占优。

系统精度：ACCORD框架通过协调验证，实现了高于任何单一LLM的共识准确率。