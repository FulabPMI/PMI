##########################
# Packages
##########################

library(shiny)
library(shinydashboard)
library(shinythemes)
library(readxl)
library(plotly)
library(caret)
library(geometry)
library(ggplot2)
library(kernlab)
library(dplyr)
library(DT)
library(shinyjs)
library(randomForest)
library(neuralnet)
library(nnet)
library(magrittr)  # For pipe operator

##########################
# Define UI
##########################
ui <- dashboardPage(
  dashboardHeader(
    title = "Post Mortem Interval Prediction (ver 1.0)",
    titleWidth = 1190
  ),
  
  dashboardSidebar(  
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Mouse Data", tabName = "mouse", icon = icon("paw")),
      menuItem("Human Data", tabName = "human", icon = icon("user")),
      menuItem("User Data", tabName = "user", icon = icon("upload")),
      menuItem("Website Statistics", tabName = "stats", icon = icon("chart-line")),
      menuItem("Help", tabName = "help", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .main-header .logo {
          font-weight: bold;
          font-size: 18px;
        }
        .box {
          border-radius: 5px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.2);
        }
        .info-box {
          cursor: pointer;
        }
        .info-box-content {
          padding: 10px;
        }
        .quality-high {
          background-color: #d4edda;
          border-left: 4px solid #28a745;
          padding: 10px;
          margin: 10px 0;
        }
        .quality-moderate {
          background-color: #fff3cd;
          border-left: 4px solid #ffc107;
          padding: 10px;
          margin: 10px 0;
        }
        .quality-low {
          background-color: #f8d7da;
          border-left: 4px solid #dc3545;
          padding: 10px;
          margin: 10px 0;
        }
      "))
    ),
    tabItems(
      
      # Home tab
      tabItem(tabName = "home",
              box(title = "Home", status = "primary", solidHeader = TRUE, width = NULL,
                  p("Welcome to the Post Mortem Interval (PMI) Prediction App. This application helps you predict the PMI (Hour) based on tissue type, circRNA (or other RNAs), expression levels, and temperature."),
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  h4(strong("About the Lab")),
                  p("Key Laboratory of Epigenetics and Oncology, the Research Center for Preclinical Medicine"),
                  p(strong("Director: "), "Prof. Junjiang Fu"),
                  p(strong("Address: "), "Southwest Medical University, Luzhou 646000, Sichuan, China"),
                  tags$br(),
                  tags$br(),
                  h4(strong("Please kindly cite following paper to support further development:")),
                  tags$div(
                    style = "text-align: justify;",
                    tags$a(
                      href = "https://www.mdpi.com/1422-0067/26/3/1046",
                      target = "_blank",
                      style = "color: blue; text-decoration: none;",
                      onmouseover = "this.style.color='red'",
                      onmouseout = "this.style.color='blue'",
                      onclick = "this.style.color='green'",
                      "1- Fu J, Song B, Qian J, Cheng J, Chiampanichayakul S, Anuchapreeda S, Fu J. Exploring the Post Mortem Interval (PMI) Estimation Model by circRNA circRnf169 in Mouse Liver Tissue. Int J Mol Sci. 2025 Jan 26;26(3):1046. doi: 10.3390/ijms26031046. PMID: 39940814."
                    ),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br()
                  )
              )
      ),
      
      # Mouse Data tab
      tabItem(tabName = "mouse",
              box(title = "Options", status = "primary", solidHeader = TRUE, width = NULL,
                  fluidRow(
                    column(2, selectInput("tissue_mouse", "Tissue Type", choices = NULL)),
                    column(3, selectInput("circRNA_mouse", "circRNA Name", choices = NULL)),
                    column(2, numericInput("temperature_mouse", "Temperature", 15, min = -20, max = 100)),
                    column(2, numericInput("expression_mouse", "Expression", 0.6, min = 0)),
                    column(3, selectInput("model_type_mouse", "Model Type", 
                                          choices = c("Support Vector Regression (SVR)" = "svm", 
                                                      "Random Forest (RF)" = "rf",
                                                      "Neural Network (NN)" = "nn"),
                                          selected = "svm"))
                  ),
                  actionButton("predict_mouse", "Predict PMI")
              ),
              
              box(
                title = "Results", status = "success", solidHeader = TRUE, width = NULL,
                tabsetPanel(
                  tabPanel("Prediction",
                           tags$br(),
                           uiOutput("quality_indicator_mouse"),
                           verbatimTextOutput("prediction_text_mouse"),
                           h4("Prediction Details"),
                           tableOutput("prediction_details_mouse")),
                  tabPanel("3D Plot",
                           tags$br(),
                           plotlyOutput("plot_3D_mouse", height = "500px")),
                  tabPanel("Statistical Plots",
                           fluidRow(
                             column(6, plotOutput("actual_predicted_plot_mouse", height = "400px")),
                             column(6, plotOutput("residual_plot_mouse", height = "400px"))
                           ),
                           fluidRow(
                             column(6, plotOutput("feature_importance_plot_mouse", height = "400px"))
                           )
                  ),
                  tabPanel("Accuracy Metrics",
                           fluidRow(
                             column(6,
                                    h4("Model Performance on Training Data"),
                                    tableOutput("performance_metrics_mouse"),
                                    h4("Cross-Validation Results"),
                                    tableOutput("cv_metrics_mouse")
                             ),
                             column(6,
                                    h4("Prediction Quality Indicators"),
                                    tableOutput("prediction_quality_mouse"),
                                    h4("Sample Similarity to Training Data"),
                                    tableOutput("similarity_metrics_mouse")
                             )
                           )
                  ),
                  tabPanel("Model Information",
                           h4("Source Model Details"),
                           wellPanel(
                             style = "background-color: #f8f9fa; padding: 15px; border: 1px solid #dee2e6; border-radius: 5px;",
                             uiOutput("model_info_mouse")
                           )
                  )
                )
              )
      ),
      
      # Human Data tab
      tabItem(tabName = "human",
              box(title = "Options", status = "primary", solidHeader = TRUE, width = NULL,
                  fluidRow(
                    column(2, selectInput("tissue_human", "Tissue Type", choices = NULL)),
                    column(3, selectInput("circRNA_human", "circRNA Name", choices = NULL)),
                    column(2, numericInput("temperature_human", "Temperature", 15, min = -20, max = 100)),
                    column(2, numericInput("expression_human", "Expression", 0.6, min = 0)),
                    column(3, selectInput("model_type_human", "Model Type", 
                                          choices = c("Support Vector Regression (SVR)" = "svm", 
                                                      "Random Forest (RF)" = "rf",
                                                      "Neural Network (NN)" = "nn"),
                                          selected = "svm"))
                  ),
                  fluidRow(
                    column(4, actionButton("predict_human", "Predict PMI"))
                  ),
                  checkboxInput("transfer_learning", "Use Transfer Learning (Mouse -> Human)", value = TRUE),
                  uiOutput("compatibility_warning")
              ),
              
              box(
                title = "Results", status = "success", solidHeader = TRUE, width = NULL,
                tabsetPanel(
                  tabPanel("Prediction",
                           tags$br(),
                           uiOutput("quality_indicator_human"),
                           verbatimTextOutput("prediction_text_human"),
                           h4("Prediction Details"),
                           tableOutput("prediction_details_human")),
                  tabPanel("3D Plot",
                           tags$br(),
                           plotlyOutput("plot_3D_human", height = "500px")),
                  tabPanel("Transfer Learning",
                           h4("Transfer Learning Details"),
                           wellPanel(
                             style = "background-color: #f8f9fa; padding: 15px; border: 1px solid #dee2e6; border-radius: 5px;",
                             uiOutput("model_info_human")
                           ),
                           plotOutput("transfer_learning_plot", height = "700px")
                  ),
                  tabPanel("Accuracy Metrics",
                           fluidRow(
                             column(6,
                                    h4("Model Performance on Training Data"),
                                    tableOutput("performance_metrics_human"),
                                    h4("Cross-Validation Results"),
                                    tableOutput("cv_metrics_human")
                             ),
                             column(6,
                                    h4("Prediction Quality Indicators"),
                                    tableOutput("prediction_quality_human"),
                                    h4("Sample Similarity to Training Data"),
                                    tableOutput("similarity_metrics_human")
                             )
                           )
                  )
                )
              )
      ),
      
      # User Data tab
      tabItem(tabName = "user",
              box(title = "Options", status = "primary", solidHeader = TRUE, width = NULL,
                  fluidRow(
                    column(4, fileInput("user_file", "Upload Excel File", accept = ".xlsx")),
                    column(4, selectInput("tissue_user", "Tissue Type", choices = NULL)),
                    column(4, selectInput("model_type_user", "Model Type", 
                                          choices = c("Support Vector Regression (SVR)" = "svm", 
                                                      "Random Forest (RF)" = "rf",
                                                      "Neural Network (NN)" = "nn"),
                                          selected = "svm")),
                    column(4, selectInput("circRNA_user", "Gene Name", choices = NULL)),
                    column(2, numericInput("temperature_user", "Temperature", 15, min = -20, max = 100)),
                    column(2, numericInput("expression_user", "Expression", 0.6, min = 0))
                  ),
                  actionButton("predict_user", "Predict PMI")
              ),
              
              box(
                title = "Results", status = "success", solidHeader = TRUE, width = NULL,
                tabsetPanel(
                  tabPanel("Prediction",
                           tags$br(),
                           uiOutput("quality_indicator_user"),
                           verbatimTextOutput("prediction_text_user"),
                           h4("Prediction Details"),
                           tableOutput("prediction_details_user")),
                  tabPanel("3D Plot",
                           tags$br(),
                           plotlyOutput("plot_3D_user", height = "500px")),
                  tabPanel("Statistical Plots",
                           fluidRow(
                             column(6, plotOutput("actual_predicted_plot_user", height = "400px")),
                             column(6, plotOutput("residual_plot_user", height = "400px"))
                           ),
                           fluidRow(
                             column(6, plotOutput("feature_importance_plot_user", height = "400px"))
                           )
                  ),
                  tabPanel("Accuracy Metrics",
                           fluidRow(
                             column(6,
                                    h4("Model Performance on Training Data"),
                                    tableOutput("performance_metrics_user"),
                                    h4("Cross-Validation Results"),
                                    tableOutput("cv_metrics_user")
                             ),
                             column(6,
                                    h4("Prediction Quality Indicators"),
                                    tableOutput("prediction_quality_user"),
                                    h4("Sample Similarity to Training Data"),
                                    tableOutput("similarity_metrics_user")
                             )
                           )
                  ),
                  tabPanel("Model Information",
                           h4("Model Details"),
                           wellPanel(
                             style = "background-color: #f8f9fa; padding: 15px; border: 1px solid #dee2e6; border-radius: 5px;",
                             uiOutput("model_info_user")
                           )
                  )
                )
              )
      ),
      
      # Statistics Tab
      tabItem(tabName = "stats",
              box(title = "Website Usage Statistics", status = "primary", solidHeader = TRUE, width = NULL,
                  fluidRow(
                    column(4,
                           valueBoxOutput("totalViewsBox", width = 12),
                           valueBoxOutput("dailyViewsBox", width = 12)
                    ),
                    column(4,
                           valueBoxOutput("weeklyViewsBox", width = 12),
                           valueBoxOutput("monthlyViewsBox", width = 12)
                    ),
                    column(4,
                           valueBoxOutput("uniqueUsersBox", width = 12),
                           valueBoxOutput("lastAccessBox", width = 12) 
                    )
                    
                  )
              )
      ),
      
      # Help Tab
      tabItem(tabName = "help",
              box(title = "Help", status = "primary", solidHeader = TRUE, width = NULL,
                  h3(strong("Application Overview")),
                  p("This app predicts Post Mortem Interval (PMI) using circRNA expression data with Support Vector Machines (SVR) and Transfer Learning technology."),
                  
                  h4(strong("Section Descriptions:")),
                  
                  h5(strong("1. Home")),
                  p("Provides an introduction to the application and information about the lab. Includes citation information for the research paper this tool is based on."),
                  
                  h5(strong("2. Mouse Data")),
                  p("Predict PMI using mouse data. Features include:"),
                  tags$ul(
                    tags$li("Select tissue type and circRNA from dropdown menus"),
                    tags$li("Adjust temperature and expression values"),
                    tags$li("View 3D visualization of predictions"),
                    tags$li("See statistical plots comparing actual vs predicted values"),
                    tags$li("Access detailed model information")
                  ),
                  
                  h5(strong("3. Human Data")),
                  p("Predict PMI using human data with optional transfer learning from mouse models. Features include:"),
                  tags$ul(
                    tags$li("Same input options as Mouse Data section"),
                    tags$li("Toggle transfer learning option (enabled by default)"),
                    tags$li("View transfer learning performance plots"),
                    tags$li("Access QQ plots for model diagnostics")
                  ),
                  
                  h5(strong("4. User Data")),
                  p("Upload and analyze your own data. Features include:"),
                  tags$ul(
                    tags$li("Upload Excel files with your experimental data"),
                    tags$li("Select relevant columns from your data"),
                    tags$li("Generate predictions using your custom dataset"),
                    tags$li("View 3D visualizations specific to your data")
                  ),
                  
                  h5(strong("5. Website Statistics")),
                  p("Track application usage metrics including:"),
                  tags$ul(
                    tags$li("Total views and daily views"),
                    tags$li("Number of unique users"),
                    tags$li("Detailed access logs"),
                    tags$li("Interactive charts of usage patterns")
                  ),
                  
                  h5(strong("6. Publications")),
                  h5("For reading and helping"),
                  tags$ul(
                    tags$li(
                      tags$a(
                        href = "https://www.mdpi.com/1422-0067/26/3/1046",
                        target = "_blank",
                        style = "color: blue; text-decoration: none;",
                        onmouseover = "this.style.color='red'",
                        onmouseout = "this.style.color='blue'",
                        onclick = "this.style.color='green'",
                        "Fu J, Song B, Qian J, Cheng J, Chiampanichayakul S, Anuchapreeda S, Fu J. Exploring the Post Mortem Interval (PMI) Estimation Model by circRNA circRnf169 in Mouse Liver Tissue. Int J Mol Sci. 2025 Jan 26;26(3):1046. doi: 10.3390/ijms26031046. PMID: 39940814."
                      )
                    ),
                    tags$li(
                      tags$a(
                        href = "https://www.mdpi.com/1422-0067/26/10/4495",
                        target = "_blank",
                        style = "color: blue; text-decoration: none;",
                        onmouseover = "this.style.color='red'",
                        onmouseout = "this.style.color='blue'",
                        onclick = "this.style.color='green'",
                        "Song B, Fu J, Qian J, He T, Cheng J, Chiampanichayakul S, Anuchapreeda S, Fu J. Development of Mathematical Models Using circRNA Combinations (circTulp4, circSlc8a1, and circStrn3) in Mouse Brain Tissue for Postmortem Interval Estimation. Int J Mol Sci. 2025 May 8;26(10):4495. doi: 10.3390/ijms26104495. PMID: 40429639."
                      )
                    ),
                    tags$li(
                      tags$a(
                        href = "https://www.nature.com/articles/s41598-025-07998-0",
                        target = "_blank",
                        style = "color: blue; text-decoration: none;",
                        onmouseover = "this.style.color='red'",
                        onmouseout = "this.style.color='blue'",
                        onclick = "this.style.color='green'",
                        "Song B, Fu J, Cheng J, Chiampanichayakul S, Anuchapreeda S, Fu J. Circular RNA circFat3 as a biomarker for construction of postmortem interval Estimation models in mouse brain tissues at multiple temperatures. Sci Rep. 2025 Jul 1;15(1):21577. doi: 10.1038/s41598-025-07998-0. PMID: 40593252."
                      )
                    )
                  ),
                  
                  h4(strong("Data Requirements:")),
                  p("For user uploads, your Excel file must contain these columns:"),
                  tags$ul(
                    tags$li("Tissue - Tissue type"),
                    tags$li("circRNA - circRNA identifier"),
                    tags$li("Expression - Expression level"),
                    tags$li("Temperature - Temperature in Celsius"),
                    tags$li("PMI_Hour - Actual PMI in hours (for model training)")
                  ),
                  
                  h4(strong("Technical Notes:")),
                  p("The application uses:"),
                  tags$ul(
                    tags$li("Support Vector Regression with RBF kernel (SVR),Random Forest (RF), and Neural Network (NN) for modeling"),
                    tags$li("Transfer learning for mouse-to-human predictions"),
                    tags$li("3D visualization with convex hulls for data boundaries"),
                    tags$li("Interactive plots with Plotly")
                  ),
                  
                  p(strong("Note:"), "All predictions should be interpreted by qualified professionals in context with other forensic evidence."),
                  tags$br(),
                  tags$br(),
                  h4(strong("Support Information")),
                  p("For support, please contact us at:"),
                  p(strong("Name: "), "Mazaher Maghsoudloo"),
                  p(strong("Email: "), "mazaher@swmu.edu.cn"),
                  p(HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;babak1146@gmail.com")),
                  tags$br(), 
                  tags$br()
              )
      )
    )
  )
)

##########################
# Helper function for case-insensitive matching
##########################

# Function to get case-insensitive match for circRNA
get_circRNA_match <- function(data, circRNA_value) {
  if (is.null(data) || nrow(data) == 0) return(NULL)
  
  # Find matching circRNA (case-insensitive)
  circRNA_upper <- toupper(circRNA_value)
  matches <- data[which(toupper(data$circRNA) == circRNA_upper), ]
  
  if (nrow(matches) > 0) {
    # Return the original case from the data for the first match
    return(unique(matches$circRNA)[1])
  }
  return(NULL)
}

# Function to filter data with case-insensitive circRNA matching
filter_by_circRNA <- function(data, tissue, circRNA) {
  if (is.null(data) || nrow(data) == 0) return(data)
  
  # First filter by tissue
  tissue_data <- data[data$Tissue == tissue, ]
  if (nrow(tissue_data) == 0) return(tissue_data)
  
  # Filter by circRNA (case-insensitive)
  circRNA_upper <- toupper(circRNA)
  matched_data <- tissue_data[toupper(tissue_data$circRNA) == circRNA_upper, ]
  
  return(matched_data)
}

# Function to get unique circRNAs for a tissue (preserving original case)
get_circRNAs_for_tissue <- function(data, tissue) {
  if (is.null(data) || nrow(data) == 0) return(character(0))
  
  tissue_data <- data[data$Tissue == tissue, ]
  if (nrow(tissue_data) == 0) return(character(0))
  
  # Get unique circRNAs
  circRNAs <- unique(tissue_data$circRNA)
  return(sort(circRNAs))
}

##########################
# Helper function to safely get kernel information from a ksvm model
##########################
get_kernel_info <- function(model) {
  if (is.null(model)) return("No model available")
  
  actual_model <- model
  if (is.list(model) && "model" %in% names(model)) {
    actual_model <- model$model
  }
  
  kern <- tryCatch(kernelf(actual_model), error = function(e) NULL)
  if (is.null(kern)) return("Unknown kernel")
  
  if (inherits(kern, "rbfkernel")) return("Gaussian (RBF)")
  if (inherits(kern, "vanillakernel")) return("Linear")
  if (inherits(kern, "polykernel")) return("Polynomial")
  if (inherits(kern, "anovadot")) return("ANOVA RBF")
  
  return(class(kern)[1])
}

##########################
# NEW ENGINE 1: AutoML Kernel Selection
##########################
auto_select_svm_kernel <- function(scaled_data, formula_str) {
  
  rbf_linear_kernel <- function(x, y) {
    rbf_part <- exp(-0.5 * sum((x - y)^2))
    linear_part <- sum(x * y)
    return(0.7 * rbf_part + 0.3 * linear_part)
  }
  class(rbf_linear_kernel) <- "kernel"
  
  candidates <- list(
    "Linear" = list(name = "vanilladot", kpar = NULL),
    "Polynomial" = list(name = "polydot", kpar = list(degree = 2, scale = 1, offset = 0)),
    "RBF (Gaussian)" = list(name = "rbfdot", kpar = list(sigma = 1)),
    "RBF+Linear Combination" = list(name = rbf_linear_kernel, kpar = NULL)
  )
  
  best_rmse <- Inf
  best_kernel <- names(candidates)[1]
  n_samples <- nrow(scaled_data)
  
  C_val <- min(100, max(10, n_samples))
  eps_val <- max(0.01, 0.1 / sqrt(n_samples))
  
  if (n_samples < 3) {
    best_kernel <- "Linear"
  } else {
    k_folds <- min(5, n_samples)
    set.seed(123)
    folds <- createFolds(scaled_data$PMI_Hour, k = k_folds)
    
    for (k_name in names(candidates)) {
      k_info <- candidates[[k_name]]
      fold_rmses <- c()
      
      for (f in 1:length(folds)) {
        test_idx <- folds[[f]]
        train_idx <- setdiff(1:n_samples, test_idx)
        train_data <- scaled_data[train_idx, , drop = FALSE]
        test_data <- scaled_data[test_idx, , drop = FALSE]
        
        try_model <- tryCatch({
          ksvm(as.formula(formula_str), data = train_data, type = "eps-svr",
               kernel = k_info$name, kpar = k_info$kpar, scaled = FALSE, 
               C = C_val, epsilon = eps_val)
        }, error = function(e) NULL)
        
        if (!is.null(try_model)) {
          preds <- predict(try_model, newdata = test_data)
          fold_rmses <- c(fold_rmses, sqrt(mean((test_data$PMI_Hour - preds)^2)))
        }
      }
      
      if (length(fold_rmses) > 0 && is.numeric(fold_rmses)) {
        avg_rmse <- mean(fold_rmses, na.rm = TRUE)
        if (!is.na(avg_rmse) && avg_rmse < best_rmse) {
          best_rmse <- avg_rmse
          best_kernel <- k_name
        }
      }
    }
  }
  
  winner_info <- candidates[[best_kernel]]
  final_model <- tryCatch({
    ksvm(as.formula(formula_str), data = scaled_data, type = "eps-svr",
         kernel = winner_info$name, kpar = winner_info$kpar, scaled = FALSE, 
         C = C_val, epsilon = eps_val)
  }, error = function(e) NULL)
  
  return(list(
    model = final_model, 
    kernel_name = best_kernel, 
    cv_rmse = best_rmse,
    tuning_info = list(C = C_val, epsilon = eps_val, cv_rmse = best_rmse)
  ))
}

##########################
# NEW ENGINE 2: AutoML + Zero-Variance
##########################
train_model <- function(data, model_type, target_var = "PMI_Hour") {
  if (is.null(data) || nrow(data) < 2) return(NULL)
  
  tryCatch({
    set.seed(1000)
    formula_str <- paste(target_var, "~ Expression + Temperature")
    
    expr_mean <- mean(data$Expression, na.rm = TRUE); expr_sd <- sd(data$Expression, na.rm = TRUE)
    temp_mean <- mean(data$Temperature, na.rm = TRUE); temp_sd <- sd(data$Temperature, na.rm = TRUE)
    if(is.na(expr_sd) || expr_sd == 0) expr_sd <- 1
    if(is.na(temp_sd) || temp_sd == 0) temp_sd <- 1
    
    scaled_data <- data
    scaled_data$Expression <- (data$Expression - expr_mean) / expr_sd
    scaled_data$Temperature <- (data$Temperature - temp_mean) / temp_sd
    
    scaling_params <- list(
      Expression_mean = expr_mean, Expression_sd = expr_sd,
      Temperature_mean = temp_mean, Temperature_sd = temp_sd
    )
    
    if (model_type == "svm") {
      auto_res <- auto_select_svm_kernel(scaled_data, formula_str)
      return(list(
        model = auto_res$model, model_type = "svm", scaling_params = scaling_params,
        kernel_type = auto_res$kernel_name, tuning_info = auto_res$tuning_info,
        feature_cols = c("Expression", "Temperature")
      ))
      
    } else if (model_type == "rf") {
      model <- randomForest(as.formula(formula_str), data = scaled_data, ntree = 500, mtry = 2, importance = TRUE)
      model$scaling_params <- scaling_params; model$model_type <- "rf"
      return(model)
      
    } else if (model_type == "nn") {
      hidden_size <- min(8, max(2, floor(nrow(scaled_data) / 3)))
      model <- tryCatch({
        nnet(as.formula(formula_str), data = scaled_data, size = hidden_size, linout = TRUE, maxit = 2000, decay = 0.01, trace = FALSE)
      }, error = function(e) {
        nnet(as.formula(formula_str), data = scaled_data, size = max(2, floor(hidden_size / 2)), linout = TRUE, maxit = 1000, decay = 0.01, trace = FALSE)
      })
      model$scaling_params <- scaling_params; model$model_type <- "nn"
      model$hidden_units <- model$n[2] 
      model$decay <- 0.01
      return(model)
    }
    return(NULL)
  }, error = function(e) {
    showNotification(paste("Model training failed:", e$message), type = "error")
    return(NULL)
  })
}

##########################
# NEW ENGINE 3: Domain Adaptation + AutoML + Variance Filter
##########################
train_transfer_model_da <- function(human_data, source_predictions, model_type, target_var = "PMI_Hour") {
  if (is.null(human_data) || nrow(human_data) < 2) return(NULL)
  
  tryCatch({
    set.seed(1000)
    combined_data <- human_data
    combined_data$source_pred <- source_predictions
    
    combined_data$expr_temp_interaction <- combined_data$Expression * combined_data$Temperature
    combined_data$source_expr_interaction <- combined_data$source_pred * combined_data$Expression
    combined_data$source_temp_interaction <- combined_data$source_pred * combined_data$Temperature
    combined_data$source_pred_sq <- combined_data$source_pred^2
    combined_data$source_pred_cu <- combined_data$source_pred^3
    
    candidate_features <- c("Expression", "Temperature", "source_pred", 
                            "expr_temp_interaction", "source_expr_interaction",
                            "source_temp_interaction", "source_pred_sq", "source_pred_cu")
    
    feature_variances <- sapply(candidate_features, function(col) var(combined_data[[col]], na.rm = TRUE))
    keep_features <- names(feature_variances)[!is.na(feature_variances) & feature_variances > 1e-6]
    
    if (length(keep_features) < 3) keep_features <- c("Expression", "Temperature", "source_pred")
    
    scaled_data <- combined_data
    scaling_params <- list()
    for (col in keep_features) {
      col_mean <- mean(combined_data[[col]], na.rm = TRUE)
      col_sd <- sd(combined_data[[col]], na.rm = TRUE)
      if (is.na(col_sd) || col_sd == 0) col_sd <- 1
      scaled_data[[col]] <- (combined_data[[col]] - col_mean) / col_sd
      scaling_params[[paste0(col, "_mean")]] <- col_mean
      scaling_params[[paste0(col, "_sd")]] <- col_sd
    }
    
    formula_str <- paste(target_var, "~", paste(keep_features, collapse = " + "))
    n_samples <- nrow(human_data)
    
    if (model_type == "svm") {
      auto_res <- auto_select_svm_kernel(scaled_data, formula_str)
      return(list(
        model = auto_res$model, model_type = "svm", scaling_params = scaling_params,
        feature_cols = keep_features, kernel_type = auto_res$kernel_name,
        tuning_info = auto_res$tuning_info, is_transfer_model = TRUE, n_samples = n_samples
      ))
      
    } else if (model_type == "rf") {
      model <- randomForest(as.formula(formula_str), data = scaled_data, ntree = 500, mtry = min(3, length(keep_features)), importance = TRUE)
      model$scaling_params <- scaling_params; model$model_type <- "rf"
      model$feature_cols <- keep_features; model$is_transfer_model <- TRUE; model$n_samples <- n_samples
      return(model)
      
    } else if (model_type == "nn") {
      hidden_size <- min(10, max(3, floor(n_samples / 3)))
      model <- tryCatch({
        nnet(as.formula(formula_str), data = scaled_data, size = hidden_size, linout = TRUE, maxit = 2000, decay = 0.05, trace = FALSE)
      }, error = function(e) {
        nnet(as.formula(formula_str), data = scaled_data, size = max(2, floor(hidden_size / 2)), linout = TRUE, maxit = 1000, decay = 0.05, trace = FALSE)
      })
      model$scaling_params <- scaling_params; model$model_type <- "nn"
      model$feature_cols <- keep_features; model$is_transfer_model <- TRUE; model$n_samples <- n_samples
      model$hidden_units <- model$n[2]; model$decay <- 0.05
      return(model)
    }
    return(NULL)
  }, error = function(e) {
    showNotification(paste("Transfer modeling failed:", e$message), type = "error")
    return(NULL)
  })
}

##########################
# Function to extract model from wrapper 
##########################
extract_model <- function(model_obj) {
  if (is.null(model_obj)) return(NULL)
  if (is.list(model_obj) && "model" %in% names(model_obj)) return(model_obj$model)
  return(model_obj)
}

##########################
# Function to calculate model metrics
##########################
calculate_model_metrics <- function(actual, predicted) {
  if (length(actual) < 2 || length(predicted) < 2 || length(actual) != length(predicted)) return(NULL)
  valid_idx <- !is.na(actual) & !is.na(predicted)
  actual <- actual[valid_idx]; predicted <- predicted[valid_idx]
  if (length(actual) < 2) return(NULL)
  
  tryCatch({
    cor_val <- cor(actual, predicted); if (is.na(cor_val)) cor_val <- 0
    metrics <- list(
      R2 = max(0, cor_val^2), RMSE = sqrt(mean((actual - predicted)^2)),
      MAE = mean(abs(actual - predicted)), MAPE = ifelse(all(actual > 0), mean(abs((actual - predicted)/actual)) * 100, NA),
      Bias = mean(predicted - actual), SD_Error = sd(predicted - actual),
      Within_10pct = mean(abs(predicted - actual) <= 0.1 * actual) * 100,
      Within_20pct = mean(abs(predicted - actual) <= 0.2 * actual) * 100,
      Within_30pct = mean(abs(predicted - actual) <= 0.3 * actual) * 100,
      N = length(actual)
    )
    return(metrics)
  }, error = function(e) return(NULL))
}

##########################
# Function to perform cross-validation 
##########################
perform_cross_validation <- function(data, model_type, n_folds = 5, source_model = NULL, is_transfer = FALSE) {
  if (is.null(data) || nrow(data) < 10) return(NULL)
  
  tryCatch({
    set.seed(123)
    k <- min(n_folds, floor(nrow(data) / 2))
    folds <- createFolds(data$PMI_Hour, k = k, list = TRUE, returnTrain = FALSE)
    cv_results <- list()
    
    for (fold in seq_along(folds)) {
      test_idx <- folds[[fold]]
      train_idx <- setdiff(seq_len(nrow(data)), test_idx)
      if (length(train_idx) < 2 || length(test_idx) < 1) next
      
      train_data <- data[train_idx, , drop = FALSE]
      test_data <- data[test_idx, , drop = FALSE]
      
      if (!is_transfer) {
        model <- train_model(train_data, model_type)
        if (is.null(model)) next
        preds <- predict_model(model, test_data, model_type)
      } else {
        if (is.null(source_model)) next
        train_data$source_pred <- as.numeric(predict_model(source_model, train_data, model_type))
        test_data$source_pred <- as.numeric(predict_model(source_model, test_data, model_type))
        train_data <- train_data[!is.na(train_data$source_pred), ]
        test_data <- test_data[!is.na(test_data$source_pred), ]
        if(nrow(train_data) < 2 || nrow(test_data) < 1) next
        
        ft_model <- train_transfer_model_da(train_data, train_data$source_pred, model_type)
        if (is.null(ft_model)) next
        preds <- predict_model(ft_model, test_data, model_type, source_model = source_model)
      }
      
      actual <- test_data$PMI_Hour
      valid_idx <- !is.na(actual) & !is.na(preds)
      if (sum(valid_idx) < 1) next
      
      actual_valid <- actual[valid_idx]; pred_valid <- preds[valid_idx]
      cor_val <- tryCatch(cor(actual_valid, pred_valid, use = "complete.obs"), error = function(e) 0)
      if (is.na(cor_val)) cor_val <- 0
      
      fold_results <- data.frame(
        Fold = fold, RMSE = sqrt(mean((actual_valid - pred_valid)^2, na.rm = TRUE)),
        MAE = mean(abs(actual_valid - pred_valid), na.rm = TRUE),
        R2 = cor_val^2, Bias = mean(pred_valid - actual_valid, na.rm = TRUE)
      )
      cv_results[[length(cv_results) + 1]] <- fold_results
    }
    
    if (length(cv_results) == 0) return(NULL)
    results_df <- bind_rows(cv_results)
    return(results_df)
    
  }, error = function(e) return(NULL))
}

##########################
# Function to calculate sample similarity
##########################
calculate_sample_similarity <- function(new_sample, training_data) {
  if (is.null(new_sample) || is.null(training_data) || nrow(training_data) < 3) return(NULL)
  tryCatch({
    cont_features <- c("Expression", "Temperature")
    if (all(cont_features %in% colnames(training_data))) {
      training_cont <- training_data[, cont_features, drop = FALSE]
      new_cont <- new_sample[, cont_features, drop = FALSE]
      training_cont <- training_cont[complete.cases(training_cont), , drop = FALSE]
      
      if (nrow(training_cont) > 2) {
        cov_matrix <- cov(training_cont)
        if (det(cov_matrix) > 0) {
          mahalanobis_dist <- mahalanobis(new_cont, colMeans(training_cont), cov_matrix)
          p_value <- pchisq(mahalanobis_dist, df = length(cont_features), lower.tail = FALSE)
          within_95pct <- mahalanobis_dist <= qchisq(0.95, df = length(cont_features))
          return(list(
            mahalanobis_distance = mahalanobis_dist, similarity_p_value = p_value,
            within_95pct = within_95pct, feature_mean_distance = abs(new_cont - colMeans(training_cont))
          ))
        }
      }
    }
    return(NULL)
  }, error = function(e) return(NULL))
}

##########################
# Function to check transfer learning compatibility (with case-insensitive circRNA)
##########################
check_transfer_compatibility <- function(mouse_data, human_data, target_tissue, target_circRNA) {
  if (is.null(mouse_data) || is.null(human_data)) return(FALSE)
  
  # Use case-insensitive matching for circRNA
  mouse_sub <- filter_by_circRNA(mouse_data, target_tissue, target_circRNA)
  human_sub <- filter_by_circRNA(human_data, target_tissue, target_circRNA)
  
  if (nrow(mouse_sub) == 0 || nrow(human_sub) == 0) return(FALSE)
  
  mouse_temps <- unique(mouse_sub$Temperature); human_temps <- unique(human_sub$Temperature)
  temp_overlap <- length(intersect(mouse_temps, human_temps))
  mouse_pmi_range <- range(mouse_sub$PMI_Hour, na.rm = TRUE); human_pmi_range <- range(human_sub$PMI_Hour, na.rm = TRUE)
  pmi_overlap <- human_pmi_range[1] <= mouse_pmi_range[2] && human_pmi_range[2] >= mouse_pmi_range[1]
  sufficient_data <- nrow(mouse_sub) >= 5 && nrow(human_sub) >= 3
  
  return(temp_overlap > 0 && pmi_overlap && sufficient_data)
}

##########################
# predict_model 
##########################
predict_model <- function(model_obj, newdata, model_type, source_model = NULL, scaling_params = NULL) {
  if (is.null(newdata) || nrow(newdata) == 0) return(numeric(0))
  if (is.null(model_obj)) return(rep(NA_real_, nrow(newdata)))
  
  tryCatch({
    model <- extract_model(model_obj)
    if (is.null(model)) stop("Model is NULL")
    
    newdata_scaled <- newdata
    is_transfer <- FALSE
    feature_cols <- c("Expression", "Temperature") 
    
    if (is.list(model_obj)) {
      is_transfer <- isTRUE(model_obj$is_transfer_model)
      if (!is.null(model_obj$feature_cols)) feature_cols <- model_obj$feature_cols
      if (is.null(scaling_params) && !is.null(model_obj$scaling_params)) scaling_params <- model_obj$scaling_params
    } else if (inherits(model, "randomForest") || inherits(model, "nnet")) {
      is_transfer <- isTRUE(model$is_transfer_model)
      if (!is.null(model$feature_cols)) feature_cols <- model$feature_cols
      if (is.null(scaling_params) && !is.null(model$scaling_params)) scaling_params <- model$scaling_params
    }
    
    if (is_transfer) {
      if (!"source_pred" %in% colnames(newdata_scaled)) {
        if (is.null(source_model)) stop("Transfer model requires source_pred or source_model")
        newdata_scaled$source_pred <- suppressWarnings(as.numeric(predict_model(source_model, newdata, model_type)))
      }
      
      if ("expr_temp_interaction" %in% feature_cols) newdata_scaled$expr_temp_interaction <- newdata_scaled$Expression * newdata_scaled$Temperature
      if ("source_expr_interaction" %in% feature_cols) newdata_scaled$source_expr_interaction <- newdata_scaled$source_pred * newdata_scaled$Expression
      if ("source_temp_interaction" %in% feature_cols) newdata_scaled$source_temp_interaction <- newdata_scaled$source_pred * newdata_scaled$Temperature
      if ("source_pred_sq" %in% feature_cols) newdata_scaled$source_pred_sq <- newdata_scaled$source_pred^2
      if ("source_pred_cu" %in% feature_cols) newdata_scaled$source_pred_cu <- newdata_scaled$source_pred^3
    }
    
    if (!is.null(scaling_params)) {
      for (col in feature_cols) {
        if (col %in% colnames(newdata_scaled)) {
          mean_val <- scaling_params[[paste0(col, "_mean")]]
          sd_val <- scaling_params[[paste0(col, "_sd")]]
          if (!is.null(mean_val) && !is.null(sd_val) && !is.na(sd_val) && sd_val != 0) {
            newdata_scaled[[col]] <- (newdata_scaled[[col]] - mean_val) / sd_val
          }
        }
      }
    }
    
    missing_cols <- setdiff(feature_cols, colnames(newdata_scaled))
    if (length(missing_cols) > 0) stop(paste("Missing features:", paste(missing_cols, collapse = ", ")))
    
    final_data <- newdata_scaled[, feature_cols, drop = FALSE]
    
    pred <- predict(model, newdata = final_data)
    pred_num <- suppressWarnings(as.numeric(pred))
    if (length(pred_num) != nrow(newdata)) pred_num <- rep(pred_num[1], nrow(newdata))
    return(pred_num)
    
  }, error = function(e) {
    showNotification(paste("Prediction error:", e$message), type = "error")
    return(rep(NA_real_, nrow(newdata)))
  })
}

##########################
# Feature importance function
##########################
get_feature_importance <- function(model_obj, model_type, data = NULL, target_var = "PMI_Hour") {
  if (is.null(model_obj)) return(NULL)
  
  tryCatch({
    actual_model <- extract_model(model_obj)
    
    features <- c("Expression", "Temperature")
    is_transfer <- FALSE
    
    if (is.list(model_obj)) {
      is_transfer <- isTRUE(model_obj$is_transfer_model)
      if (!is.null(model_obj$feature_cols)) features <- model_obj$feature_cols
    } else if (inherits(actual_model, "randomForest") || inherits(actual_model, "nnet")) {
      is_transfer <- isTRUE(actual_model$is_transfer_model)
      if (!is.null(actual_model$feature_cols)) features <- actual_model$feature_cols
    }
    
    if (tolower(model_type) == "rf" && inherits(actual_model, "randomForest")) {
      imp <- importance(actual_model)
      if (!is.null(imp)) {
        importance_df <- data.frame(
          Feature = rownames(imp), Importance = imp[, "%IncMSE"], stringsAsFactors = FALSE
        )
      }
    } else {
      importance_df <- data.frame(
        Feature = features, Importance = rep(1/length(features), length(features)), stringsAsFactors = FALSE
      )
    }
    
    importance_df <- importance_df[order(-importance_df$Importance), , drop = FALSE]
    
    importance_df$Feature <- gsub("source_pred_sq", "Source²", importance_df$Feature)
    importance_df$Feature <- gsub("source_pred_cu", "Source³", importance_df$Feature)
    importance_df$Feature <- gsub("expr_temp_interaction", "Expression × Temp", importance_df$Feature)
    importance_df$Feature <- gsub("source_expr_interaction", "Source × Expr", importance_df$Feature)
    importance_df$Feature <- gsub("source_temp_interaction", "Source × Temp", importance_df$Feature)
    importance_df$Feature <- gsub("source_pred", "Source Prediction", importance_df$Feature)
    
    return(importance_df)
  }, error = function(e) return(NULL))
}

# ==================== END OF HELPER FUNCTIONS ====================



server <- function(input, output, session) {
  
  `%>%` <- dplyr::`%>%`
  
  if (!dir.exists("data")) {
    dir.create("data")
  }
  
  model_cache <- reactiveValues(
    models = list()
  )
  
  stats_file <- "data/website_stats.RDS"
  access_log_file <- "data/access_log.csv"
  
  `%||%` <- function(a, b) {
    if (!is.null(a)) a else b
  }
  
  loadStats <- function() {
    if (file.exists(stats_file)) {
      readRDS(stats_file)
    } else {
      list(
        pageViews = 0,
        userSessions = 0,
        lastAccess = Sys.time()
      )
    }
  }
  
  loadAccessLog <- function() {
    if (file.exists(access_log_file)) {
      read.csv(access_log_file, stringsAsFactors = FALSE)
    } else {
      data.frame(
        timestamp = character(),
        sessionID = character(),
        page = character(),
        action = character(),
        stringsAsFactors = FALSE
      )
    }
  }
  
  model_results <- reactiveValues(
    predicted_value = NA,
    model = NULL,
    source_model = NULL,
    fine_tuned_model = NULL,
    is_human_only = FALSE,
    r_squared = NA,
    mse = NA,
    lower_bound = NA,
    upper_bound = NA,
    filtered_data = NULL,
    new_sample = NULL,
    transfer_learning_used = FALSE,
    feature_importance = NULL,
    mouse_levels = list(circRNA_levels = NULL, Tissue_levels = NULL),
    current_model_type = "svm",
    tissue_circ_models = list(),
    training_samples = 0,
    filtered_predictions = NULL,
    transfer_compatible = FALSE,
    model_metrics = NULL,
    cv_metrics = NULL,
    similarity_metrics = NULL
  )
  
  stats <- reactiveValues(
    pageViews = loadStats()$pageViews,
    userSessions = loadStats()$userSessions,
    lastAccess = loadStats()$lastAccess,
    accessLog = loadAccessLog()
  )
  
  saveStats <- function() {
    stats_data <- list(
      pageViews = stats$pageViews,
      userSessions = stats$userSessions,
      lastAccess = stats$lastAccess
    )
    saveRDS(stats_data, file = stats_file)
  }
  
  saveAccessLog <- function() {
    if (nrow(stats$accessLog) > 0) {
      write.csv(stats$accessLog, file = access_log_file, row.names = FALSE)
    }
  }
  
  mouse_data <- reactive({
    tryCatch({
      if (!file.exists("Data_Mice.xlsx")) {
        showNotification("Mouse data file 'Data_Mice.xlsx' not found. Please ensure the file is in the application directory.", 
                         type = "error", duration = NULL)
        return(NULL)
      }
      data <- readxl::read_excel("Data_Mice.xlsx")
      
      if (!"PMI_Hour" %in% colnames(data)) {
        stop("The 'PMI_Hour' column is missing in Data_Mice.xlsx.")
      }
      
      data$Species <- "Mouse"
      return(data)
      
    }, error = function(e) {
      showNotification(paste("Mouse data loading error:", e$message), type = "error")
      return(NULL)
    })
  })
  
  human_data <- reactive({
    tryCatch({
      if (!file.exists("Data_Human.xlsx")) {
        showNotification("Human data file 'Data_Human.xlsx' not found. Please ensure the file is in the application directory.", 
                         type = "error", duration = NULL)
        return(NULL)
      }
      data <- readxl::read_excel("Data_Human.xlsx")
      
      if (!"PMI_Hour" %in% colnames(data)) {
        stop("The 'PMI_Hour' column is missing in Data_Human.xlsx.")
      }
      
      data$Species <- "Human"
      return(data)
      
    }, error = function(e) {
      showNotification(paste("Human data loading error:", e$message), type = "error")
      return(NULL)
    })
  })
  
  data_source <- reactive({
    req(input$tabs)
    
    switch(input$tabs,
           "mouse" = mouse_data(),
           "human" = human_data(),
           "user" = {
             if (!is.null(input$user_file)) {
               tryCatch({
                 data <- readxl::read_excel(input$user_file$datapath)
                 
                 if (!"PMI_Hour" %in% colnames(data)) {
                   stop("The 'PMI_Hour' column is missing in the uploaded file.")
                 }
                 
                 data$Species <- "User"
                 return(data)
                 
               }, error = function(e) {
                 showNotification(paste("User data upload error:", e$message), type = "error")
                 return(NULL)
               })
             } else {
               return(NULL)
             }
           },
           return(NULL)
    )
  })
  
  # Observe for tissue selection - using case-insensitive matching for circRNA display
  observe({
    data <- data_source()
    req(data)
    current_tab <- input$tabs
    
    tissue_id <- switch(current_tab,
                        "mouse" = "tissue_mouse",
                        "human" = "tissue_human", 
                        "user" = "tissue_user",
                        NULL)
    
    if (is.null(tissue_id)) return()
    
    if (!"Tissue" %in% colnames(data)) {
      showNotification("The 'Tissue' column is missing in the dataset.", type = "error")
      return()
    }
    
    tissue_choices <- sort(unique(data$Tissue))
    
    if (length(tissue_choices) == 0) {
      showNotification("No valid tissue values found in the dataset.", type = "warning")
      return()
    }
    
    updateSelectInput(session, tissue_id, choices = tissue_choices)
  })
  
  # Observe for circRNA selection - using case-insensitive matching
  observe({
    data <- data_source()
    req(data)
    current_tab <- input$tabs
    
    tissue_input <- switch(current_tab,
                           "mouse" = input$tissue_mouse,
                           "human" = input$tissue_human,
                           "user" = input$tissue_user,
                           NULL)
    
    circ_id <- switch(current_tab,
                      "mouse" = "circRNA_mouse",
                      "human" = "circRNA_human",
                      "user" = "circRNA_user",
                      NULL)
    
    if (is.null(tissue_input) || is.null(circ_id)) return()
    
    if (!all(c("Tissue", "circRNA") %in% colnames(data))){
      showNotification("The dataset must contain both 'Tissue' and 'circRNA' columns.", type = "error")
      return()
    }
    
    if (!is.null(tissue_input) && tissue_input %in% unique(data$Tissue)) {
      # Get circRNAs for this tissue (preserving original case)
      circRNA_choices <- get_circRNAs_for_tissue(data, tissue_input)
      
      if (length(circRNA_choices) == 0) {
        showNotification(paste("No circRNA entries found for tissue:", tissue_input), type = "warning")
        return()
      }
      
      updateSelectInput(session, circ_id, choices = circRNA_choices)
    }
  })
  
  output$compatibility_warning <- renderUI({
    if (input$tabs != "human") return(NULL)
    
    tissue <- input$tissue_human
    circRNA <- input$circRNA_human
    
    if (is.null(tissue) || is.null(circRNA) || tissue == "" || circRNA == "") return(NULL)
    
    mouse <- mouse_data()
    human <- human_data()
    
    if (is.null(mouse) || is.null(human)) return(NULL)
    
    compatible <- check_transfer_compatibility(mouse, human, tissue, circRNA)
    
    if (!compatible && input$transfer_learning) {
      # Warning message if needed
    } else if (compatible) {
      # Success message if needed
    }
  })
  
  observeEvent(input$predict_mouse, { predict_pmi("mouse") })
  observeEvent(input$predict_human, { predict_pmi("human") })
  observeEvent(input$predict_user, { predict_pmi("user") })
  
  predict_pmi <- function(tab) {
    req(
      input[[paste0("tissue_", tab)]],
      input[[paste0("circRNA_", tab)]],
      input[[paste0("expression_", tab)]],
      input[[paste0("temperature_", tab)]],
      input[[paste0("model_type_", tab)]]
    )
    
    withProgress(message = "Making prediction", value = 0, {
      incProgress(0.1, detail = "Loading data")
      data <- data_source()
      req(data)
      
      required_cols <- c("Tissue", "circRNA", "Expression", "Temperature", "PMI_Hour")
      missing_cols <- setdiff(required_cols, colnames(data))
      if (length(missing_cols) > 0) {
        showNotification(
          paste("Missing columns in data:", paste(missing_cols, collapse = ", ")),
          type = "error"
        )
        return(NULL)
      }
      
      model_results$current_model_type <- input[[paste0("model_type_", tab)]]
      
      if (tab %in% c("mouse", "human")) {
        incProgress(0.2, detail = "Training models")
        
        tissue <- input[[paste0("tissue_", tab)]]
        circRNA <- input[[paste0("circRNA_", tab)]]
        transfer_flag <- ifelse(tab == "human" && isTRUE(input$transfer_learning), "TL", "noTL")
        model_key <- paste(tissue, circRNA, model_results$current_model_type, transfer_flag, sep = "_")
        
        if (!is.null(model_results$tissue_circ_models[[model_key]])) {
          models <- model_results$tissue_circ_models[[model_key]]
        } else {
          models <- train_transfer_model_specific(tissue, circRNA)
          if (is.null(models)) {
            showNotification("Model training failed.", type = "error")
            return(NULL)
          }
          model_results$tissue_circ_models[[model_key]] <- models
        }
        
        model_results$source_model <- models$source_model
        model_results$fine_tuned_model <- models$fine_tuned_model
        model_results$transfer_learning_used <- !is.null(models$fine_tuned_model)
        model_results$is_human_only <- isTRUE(models$is_human_only)
        
        if (tab == "human" && input$transfer_learning) {
          model_results$transfer_compatible <- check_transfer_compatibility(
            mouse_data(), human_data(), tissue, circRNA
          )
        }
        
        new_sample <- data.frame(
          Tissue = input[[paste0("tissue_", tab)]],
          circRNA = input[[paste0("circRNA_", tab)]],
          Expression = input[[paste0("expression_", tab)]],
          Temperature = input[[paste0("temperature_", tab)]],
          Species = ifelse(tab == "human", "Human", "Mouse"),
          stringsAsFactors = FALSE
        )
        
        # Use case-insensitive filtering for filtered_data
        filtered_data <- filter_by_circRNA(data, tissue, circRNA)
        
        model_results$training_samples <- nrow(filtered_data)
        
        if (nrow(filtered_data) > 0) {
          if (!is.null(model_results$mouse_levels$Tissue_levels)) {
            filtered_data$Tissue <- factor(filtered_data$Tissue, levels = model_results$mouse_levels$Tissue_levels)
          } else {
            filtered_data$Tissue <- factor(filtered_data$Tissue)
          }
          
          if (!is.null(model_results$mouse_levels$circRNA_levels)) {
            filtered_data$circRNA <- factor(filtered_data$circRNA, levels = model_results$mouse_levels$circRNA_levels)
          } else {
            filtered_data$circRNA <- factor(filtered_data$circRNA)
          }
        }
        
        model_results$filtered_data <- filtered_data
        model_results$new_sample <- new_sample
        
        incProgress(0.5, detail = "Calculating prediction")
        
        tryCatch({
          selected_model_type <- tolower(input[[paste0("model_type_", tab)]])
          
          if (!is.null(model_results$mouse_levels$Tissue_levels)) {
            new_sample$Tissue <- factor(new_sample$Tissue, levels = model_results$mouse_levels$Tissue_levels)
          } else {
            new_sample$Tissue <- factor(new_sample$Tissue)
          }
          
          if (!is.null(model_results$mouse_levels$circRNA_levels)) {
            new_sample$circRNA <- factor(new_sample$circRNA, levels = model_results$mouse_levels$circRNA_levels)
          } else {
            new_sample$circRNA <- factor(new_sample$circRNA)
          }
          
          source_scaling_params <- NULL
          fine_tuned_scaling_params <- NULL
          
          if (!is.null(models$source_model)) {
            if (is.list(models$source_model) && "scaling_params" %in% names(models$source_model)) {
              source_scaling_params <- models$source_model$scaling_params
            } else if (!is.null(models$source_model$scaling_params)) {
              source_scaling_params <- models$source_model$scaling_params
            }
          }
          
          if (!is.null(models$fine_tuned_model)) {
            if (is.list(models$fine_tuned_model) && "scaling_params" %in% names(models$fine_tuned_model)) {
              fine_tuned_scaling_params <- models$fine_tuned_model$scaling_params
            } else if (!is.null(models$fine_tuned_model$scaling_params)) {
              fine_tuned_scaling_params <- models$fine_tuned_model$scaling_params
            }
          }
          
          if (model_results$transfer_learning_used && new_sample$Species == "Human" && !is.null(models$fine_tuned_model)) {
            source_pred_val <- predict_model(
              models$source_model, 
              new_sample, 
              selected_model_type,
              scaling_params = source_scaling_params
            )
            
            if (length(source_pred_val) == 0 || is.na(source_pred_val)) {
              showNotification("Source model could not predict for the new sample.", type = "error")
              return(NULL)
            }
            
            new_sample$source_pred <- as.numeric(source_pred_val)
            
            pred_vals <- predict_model(
              models$fine_tuned_model, 
              new_sample, 
              selected_model_type, 
              source_model = models$source_model,
              scaling_params = fine_tuned_scaling_params
            )
            model_results$predicted_value <- as.numeric(pred_vals[1])
            
            if (!is.null(filtered_data) && nrow(filtered_data) > 0) {
              filtered_data_with_source <- filtered_data
              
              source_preds_filtered <- predict_model(
                models$source_model, 
                filtered_data, 
                selected_model_type,
                scaling_params = source_scaling_params
              )
              filtered_data_with_source$source_pred <- as.numeric(source_preds_filtered)
              
              filtered_data_with_source <- filtered_data_with_source[!is.na(filtered_data_with_source$source_pred), ]
              
              if (nrow(filtered_data_with_source) > 1) {
                preds <- predict_model(
                  models$fine_tuned_model, 
                  filtered_data_with_source, 
                  selected_model_type, 
                  source_model = models$source_model,
                  scaling_params = fine_tuned_scaling_params
                )
                model_results$filtered_predictions <- preds
                
                model_results$model_metrics <- calculate_model_metrics(filtered_data_with_source$PMI_Hour, preds)
                
                if (!is.null(model_results$model_metrics)) {
                  model_results$r_squared <- model_results$model_metrics$R2
                  model_results$mse <- model_results$model_metrics$RMSE^2
                } else {
                  model_results$r_squared <- NA
                  model_results$mse <- NA
                }
              }
            }
            
          } else {
            pred_vals <- predict_model(
              models$source_model, 
              new_sample, 
              selected_model_type,
              scaling_params = source_scaling_params
            )
            model_results$predicted_value <- as.numeric(pred_vals[1])
            
            if (!is.null(filtered_data) && nrow(filtered_data) > 1) {
              preds <- predict_model(
                models$source_model, 
                filtered_data, 
                selected_model_type,
                scaling_params = source_scaling_params
              )
              
              model_results$filtered_predictions <- preds
              
              model_results$model_metrics <- calculate_model_metrics(filtered_data$PMI_Hour, preds)
              
              if (!is.null(model_results$model_metrics)) {
                model_results$r_squared <- model_results$model_metrics$R2
                model_results$mse <- model_results$model_metrics$RMSE^2
              } else {
                model_results$r_squared <- NA
                model_results$mse <- NA
              }
            }
          }
          
          if (!is.null(model_results$mse) && !is.na(model_results$mse) && !is.na(model_results$predicted_value)) {
            ci_range <- 1.96 * sqrt(model_results$mse)
            model_results$lower_bound <- max(0, model_results$predicted_value - ci_range)
            model_results$upper_bound <- model_results$predicted_value + ci_range
          } else {
            model_results$lower_bound <- NA
            model_results$upper_bound <- NA
          }
          
          model_results$feature_importance <- get_feature_importance(
            if (model_results$transfer_learning_used && new_sample$Species == "Human" && !is.null(models$fine_tuned_model)) {
              models$fine_tuned_model
            } else {
              models$source_model
            },
            selected_model_type
          )
          
          if (!is.null(filtered_data) && nrow(filtered_data) >= 10) {
            if (tab == "human" && input$transfer_learning) {
              model_results$cv_metrics <- perform_cross_validation(
                filtered_data, 
                selected_model_type,
                source_model = models$source_model, 
                is_transfer = TRUE 
              )
            } else {
              model_results$cv_metrics <- perform_cross_validation(
                filtered_data, 
                selected_model_type
              )
            }
          } else {
            model_results$cv_metrics <- NULL
          }
          
          if (!is.null(new_sample) && !is.null(filtered_data) && nrow(filtered_data) > 0) {
            model_results$similarity_metrics <- calculate_sample_similarity(new_sample, filtered_data)
          }
          
        }, error = function(e) {
          showNotification(paste("Prediction failed:", e$message), type = "error")
          print(paste("Error details:", e$message))
        })
        
      } else if (tab == "user") {
        req(input$user_file)
        
        filtered_data <- filter_by_circRNA(data, input$tissue_user, input$circRNA_user)
        
        model_results$training_samples <- nrow(filtered_data)
        
        if (nrow(filtered_data) < 2) {
          showNotification(
            "Not enough data points for selected Tissue and circRNA (need at least 2 samples).",
            type = "error"
          )
          return(NULL)
        }
        
        circRNA_levels <- unique(filtered_data$circRNA)
        Tissue_levels <- unique(filtered_data$Tissue)
        
        filtered_data$circRNA <- factor(filtered_data$circRNA, levels = circRNA_levels)
        filtered_data$Tissue <- factor(filtered_data$Tissue, levels = Tissue_levels)
        
        incProgress(0.3, detail = "Training user model")
        
        tryCatch({
          user_model <- train_model(filtered_data, model_results$current_model_type)
          
          if (is.null(user_model)) {
            showNotification("Model training failed.", type = "error")
            return(NULL)
          }
          
          model_results$model <- user_model
          
          new_sample <- data.frame(
            Tissue = factor(input$tissue_user, levels = Tissue_levels),
            circRNA = factor(input$circRNA_user, levels = circRNA_levels),
            Expression = input$expression_user,
            Temperature = input$temperature_user,
            stringsAsFactors = FALSE
          )
          
          incProgress(0.7, detail = "Calculating prediction")
          
          user_scaling_params <- NULL
          if (is.list(user_model) && "scaling_params" %in% names(user_model)) {
            user_scaling_params <- user_model$scaling_params
          } else if (!is.null(user_model$scaling_params)) {
            user_scaling_params <- user_model$scaling_params
          }
          
          prediction <- predict_model(
            user_model, 
            new_sample, 
            model_results$current_model_type,
            scaling_params = user_scaling_params
          )
          model_results$predicted_value <- as.numeric(prediction)
          
          preds <- predict_model(
            user_model, 
            filtered_data, 
            model_results$current_model_type,
            scaling_params = user_scaling_params
          )
          
          model_results$filtered_predictions <- preds
          model_results$model_metrics <- calculate_model_metrics(filtered_data$PMI_Hour, preds)
          
          if (!is.null(model_results$model_metrics)) {
            model_results$r_squared <- model_results$model_metrics$R2
            model_results$mse <- model_results$model_metrics$RMSE^2
          } else {
            model_results$r_squared <- NA
            model_results$mse <- NA
          }
          
          if (!is.null(model_results$mse) && !is.na(model_results$mse) && !is.na(model_results$predicted_value)) {
            ci_range <- 1.96 * sqrt(model_results$mse)
            model_results$lower_bound <- max(0, model_results$predicted_value - ci_range)
            model_results$upper_bound <- model_results$predicted_value + ci_range
          } else {
            model_results$lower_bound <- NA
            model_results$upper_bound <- NA
          }
          
          model_results$feature_importance <- get_feature_importance(user_model, model_results$current_model_type)
          model_results$filtered_data <- filtered_data
          model_results$new_sample <- new_sample
          
          if (nrow(filtered_data) >= 10) {
            model_results$cv_metrics <- perform_cross_validation(filtered_data, model_results$current_model_type)
          } else {
            model_results$cv_metrics <- NULL
          }
          
          model_results$similarity_metrics <- calculate_sample_similarity(new_sample, filtered_data)
          
        }, error = function(e) {
          showNotification(paste("User prediction failed:", e$message), type = "error")
          print(paste("Error details:", e$message))
        })
      }
      
      incProgress(1, detail = "Prediction completed successfully")
    })
  }
  
  train_transfer_model_specific <- function(target_tissue, target_circRNA) {
    mouse <- mouse_data()
    human <- human_data()
    
    current_tab <- input$tabs
    selected_model_type <- input[[paste0("model_type_", current_tab)]]
    
    tryCatch({
      set.seed(1000)
      
      if (current_tab == "mouse") {
        if (is.null(mouse)) return(NULL)
        # Use case-insensitive filtering
        mouse_filtered <- filter_by_circRNA(mouse, target_tissue, target_circRNA)
        if (nrow(mouse_filtered) < 2) return(NULL)
        
        source_model <- train_model(mouse_filtered, selected_model_type)
        return(list(source_model = source_model, fine_tuned_model = NULL, is_human_only = FALSE))
      }
      
      if (current_tab == "human") {
        if (is.null(human)) return(NULL)
        # Use case-insensitive filtering
        human_filtered <- filter_by_circRNA(human, target_tissue, target_circRNA)
        if (nrow(human_filtered) < 2) return(NULL)
        
        if (!isTRUE(input$transfer_learning)) {
          human_only_model <- train_model(human_filtered, selected_model_type)
          return(list(source_model = human_only_model, fine_tuned_model = NULL, is_human_only = TRUE))
        } 
        
        if (isTRUE(input$transfer_learning)) {
          if (is.null(mouse)) return(NULL)
          # Use case-insensitive filtering for mouse data
          mouse_filtered <- filter_by_circRNA(mouse, target_tissue, target_circRNA)
          
          source_model <- train_model(mouse_filtered, selected_model_type)
          
          human_filtered$source_pred <- as.numeric(predict_model(source_model, human_filtered, selected_model_type))
          human_prep <- human_filtered[!is.na(human_filtered$source_pred), ]
          
          if (nrow(human_prep) > 1) {
            fine_tuned_model <- train_transfer_model_da(human_prep, human_prep$source_pred, selected_model_type)
            
            if (!is.null(fine_tuned_model)) {
              source_scaling <- if (is.list(source_model) && "scaling_params" %in% names(source_model)) source_model$scaling_params else source_model$scaling_params
              if (is.list(fine_tuned_model)) {
                fine_tuned_model$source_scaling_params <- source_scaling
              } else {
                fine_tuned_model$source_scaling_params <- source_scaling
              }
            }
            return(list(source_model = source_model, fine_tuned_model = fine_tuned_model, is_human_only = FALSE))
          }
        }
      }
      return(NULL)
    }, error = function(e) {
      showNotification(paste("Model training failed:", e$message), type = "error")
      return(NULL)
    })
  }
  
  output$quality_indicator_mouse <- renderUI({
    if (input$tabs != "mouse") return(NULL)
    get_quality_indicator()
  })
  
  output$quality_indicator_human <- renderUI({
    if (input$tabs != "human") return(NULL)
    get_quality_indicator()
  })
  
  output$quality_indicator_user <- renderUI({
    if (input$tabs != "user") return(NULL)
    get_quality_indicator()
  })
  
  get_quality_indicator <- function() {
    if (is.null(model_results) || is.na(model_results$predicted_value)) return(NULL)
    
    quality_class <- "quality-high"
    quality_text <- "High Confidence"
    quality_details <- ""
    
    if (!is.null(model_results$model_metrics)) {
      r2 <- model_results$model_metrics$R2
      n_samples <- model_results$training_samples
      
      if (!is.na(r2)) {
        if (r2 < 0.5) {
          quality_class <- "quality-low"
          quality_text <- "Low Confidence"
          quality_details <- paste("(R² =", round(r2, 2), ")")
        } else if (r2 < 0.7) {
          quality_class <- "quality-moderate"
          quality_text <- "Moderate Confidence"
          quality_details <- paste("(R² =", round(r2, 2), ")")
        } else {
          quality_details <- paste("(R² =", round(r2, 2), ")")
        }
      }
      
      if (n_samples < 10) {
        quality_details <- paste(quality_details, "| Low sample size (n =", n_samples, ")")
      }
    }
    
    div(
      class = quality_class,
      icon("chart-line"),
      strong("Prediction Quality: "), quality_text,
      br(),
      tags$small(quality_details)
    )
  }
  
  output$prediction_text_mouse <- renderPrint({
    if (input$tabs != "mouse") return(invisible(NULL))
    print_prediction_results()
  })
  
  output$prediction_text_human <- renderPrint({
    if (input$tabs != "human") return(invisible(NULL))
    print_prediction_results()
  })
  
  output$prediction_text_user <- renderPrint({
    if (input$tabs != "user") return(invisible(NULL))
    print_prediction_results()
  })
  
  print_prediction_results <- function() {
    if (is.null(model_results) || !is.list(model_results)) {
      cat("No valid model results found.\n")
      return(invisible(NULL))
    }
    
    pred <- ifelse(model_results$predicted_value < 0, 0, model_results$predicted_value)
    
    if (is.null(pred) || is.na(pred)) {
      cat("Prediction not available. Please make a prediction first.\n")
      return(invisible(NULL))
    }
    
    model_type_name <- switch(
      tolower(model_results$current_model_type),
      "svm" = "Support Vector Regression (SVR)",
      "rf"  = "Random Forest (RF)",
      "nn"  = "Neural Network (NN)",
      "Unknown"
    )
    
    cat(sprintf("Model Type: %s\n", model_type_name))
    cat(sprintf("Training Samples: %d\n", model_results$training_samples))
    cat(sprintf("Predicted PMI: %.2f hours\n", pred))
    
    lb <- model_results$lower_bound
    ub <- model_results$upper_bound
    if (!is.null(lb) && !is.na(lb) && !is.null(ub) && !is.na(ub)) {
      cat(sprintf("95%% Confidence Interval: [%.2f, %.2f] hours\n", lb, ub))
    }
    
    r2 <- model_results$r_squared
    if (!is.null(r2) && !is.na(r2)) {
      cat(sprintf("Model R-squared: %.3f\n", r2))
    } else {
      cat("Model R-squared: Not available\n")
    }
    
    if (isTRUE(model_results$transfer_learning_used) && input$tabs != "user") {
      cat("Transfer Learning: Applied (Mouse -> Human)\n")
      if (!model_results$transfer_compatible) {
        cat("Warning: Limited data compatibility between mouse and human\n")
      }
    }
    
    return(invisible(NULL))
  }
  
  output$prediction_details_mouse <- renderTable({
    if (input$tabs != "mouse") return(NULL)
    get_prediction_details()
  }, striped = TRUE, hover = TRUE)
  
  output$prediction_details_human <- renderTable({
    if (input$tabs != "human") return(NULL)
    get_prediction_details()
  }, striped = TRUE, hover = TRUE)
  
  output$prediction_details_user <- renderTable({
    if (input$tabs != "user") return(NULL)
    get_prediction_details()
  }, striped = TRUE, hover = TRUE)
  
  get_prediction_details <- function() {
    req(model_results$predicted_value)
    
    details <- data.frame(
      Parameter = c("Tissue", "circRNA", "Expression", "Temperature", "Model Type"),
      Value = c(
        input[[paste0("tissue_", input$tabs)]],
        input[[paste0("circRNA_", input$tabs)]],
        input[[paste0("expression_", input$tabs)]],
        paste(input[[paste0("temperature_", input$tabs)]], "°C"),
        switch(tolower(model_results$current_model_type),
               "svm" = "Support Vector Regression (SVR)",
               "rf"  = "Random Forest (RF)",
               "nn"  = "Neural Network (NN)",
               "Unknown")
      )
    )
    
    return(details)
  }
  
  output$performance_metrics_mouse <- renderTable({
    if (input$tabs != "mouse") return(NULL)
    get_performance_metrics()
  }, striped = TRUE, hover = TRUE)
  
  output$performance_metrics_human <- renderTable({
    if (input$tabs != "human") return(NULL)
    get_performance_metrics()
  }, striped = TRUE, hover = TRUE)
  
  output$performance_metrics_user <- renderTable({
    if (input$tabs != "user") return(NULL)
    get_performance_metrics()
  }, striped = TRUE, hover = TRUE)
  
  get_performance_metrics <- function() {
    req(model_results$model_metrics)
    
    metrics <- model_results$model_metrics
    
    if (is.null(metrics)) {
      return(data.frame(
        Metric = "Information",
        Value = "Performance metrics not available"
      ))
    }
    
    data.frame(
      Metric = c("R-squared", "RMSE (hours)", "MAE (hours)", "Bias (hours)", 
                 "Std Dev Error", "Sample Size"),
      Value = c(
        round(metrics$R2, 3),
        round(metrics$RMSE, 2),
        round(metrics$MAE, 2),
        round(metrics$Bias, 2),
        round(metrics$SD_Error, 2),
        metrics$N
      )
    )
  }
  
  output$cv_metrics_mouse <- renderTable({
    if (input$tabs != "mouse") return(NULL)
    get_cv_metrics()
  }, striped = TRUE, hover = TRUE)
  
  output$cv_metrics_human <- renderTable({
    if (input$tabs != "human") return(NULL)
    get_cv_metrics()
  }, striped = TRUE, hover = TRUE)
  
  output$cv_metrics_user <- renderTable({
    if (input$tabs != "user") return(NULL)
    get_cv_metrics()
  }, striped = TRUE, hover = TRUE)
  
  get_cv_metrics <- function() {
    cv_metrics <- model_results$cv_metrics
    
    if (is.null(cv_metrics)) {
      return(data.frame(
        Metric = "Information",
        Value = "Cross-validation requires at least 10 samples"
      ))
    }
    
    if (!is.data.frame(cv_metrics) || nrow(cv_metrics) == 0) {
      return(data.frame(
        Metric = "Information",
        Value = "Cross-validation results not available"
      ))
    }
    
    if (!is.null(attr(cv_metrics, "summary"))) {
      summary_stats <- attr(cv_metrics, "summary")
      return(summary_stats)
    }
    
    required_cols <- c("RMSE", "R2", "MAE", "Bias")
    missing_cols <- setdiff(required_cols, colnames(cv_metrics))
    
    if (length(missing_cols) > 0) {
      return(data.frame(
        Metric = "Information",
        Value = "Cross-validation results incomplete"
      ))
    }
    
    summary_stats <- data.frame(
      Metric = c("Mean RMSE", "Std Dev RMSE", "Mean R²", "Std Dev R²", 
                 "Mean MAE", "Mean Bias", "Number of Folds"), 
      Value = c(
        round(mean(cv_metrics$RMSE, na.rm = TRUE), 2),
        round(sd(cv_metrics$RMSE, na.rm = TRUE), 2),
        round(mean(cv_metrics$R2, na.rm = TRUE), 3),
        round(sd(cv_metrics$R2, na.rm = TRUE), 3),
        round(mean(cv_metrics$MAE, na.rm = TRUE), 2),
        round(mean(cv_metrics$Bias, na.rm = TRUE), 2),
        nrow(cv_metrics)
      )
    )
    
    summary_stats <- summary_stats[!is.na(summary_stats$Value), ]
    
    if (nrow(summary_stats) == 0) {
      return(data.frame(
        Metric = "Information",
        Value = "Could not calculate cross-validation metrics"
      ))
    }
    
    return(summary_stats)
  }
  
  output$prediction_quality_mouse <- renderTable({
    if (input$tabs != "mouse") return(NULL)
    get_prediction_quality()
  }, striped = TRUE, hover = TRUE)
  
  output$prediction_quality_human <- renderTable({
    if (input$tabs != "human") return(NULL)
    get_prediction_quality()
  }, striped = TRUE, hover = TRUE)
  
  output$prediction_quality_user <- renderTable({
    if (input$tabs != "user") return(NULL)
    get_prediction_quality()
  }, striped = TRUE, hover = TRUE)
  
  get_prediction_quality <- function() {
    req(model_results$predicted_value)
    
    interval_width <- if (!is.na(model_results$upper_bound) && !is.na(model_results$lower_bound)) {
      model_results$upper_bound - model_results$lower_bound
    } else NA
    
    rel_width <- if (!is.na(interval_width) && model_results$predicted_value > 0) {
      (interval_width / model_results$predicted_value) * 100
    } else NA
    
    data.frame(
      Metric = c("Predicted PMI (hours)", 
                 "95% CI Lower Bound", 
                 "95% CI Upper Bound",
                 "CI Width (hours)",
                 "Relative CI Width (%)",
                 "Training Samples"),
      Value = c(
        round(ifelse(model_results$predicted_value<0,0,model_results$predicted_value), 2),
        round(model_results$lower_bound, 2),
        round(model_results$upper_bound, 2),
        round(interval_width, 2),
        if (!is.na(rel_width)) paste0(round(rel_width, 1), "%") else "N/A",
        model_results$training_samples
      )
    )
  }
  
  output$similarity_metrics_mouse <- renderTable({
    if (input$tabs != "mouse") return(NULL)
    get_similarity_metrics()
  }, striped = TRUE, hover = TRUE)
  
  output$similarity_metrics_human <- renderTable({
    if (input$tabs != "human") return(NULL)
    get_similarity_metrics()
  }, striped = TRUE, hover = TRUE)
  
  output$similarity_metrics_user <- renderTable({
    if (input$tabs != "user") return(NULL)
    get_similarity_metrics()
  }, striped = TRUE, hover = TRUE)
  
  get_similarity_metrics <- function() {
    similarity <- model_results$similarity_metrics
    
    if (is.null(similarity)) {
      return(data.frame(
        Metric = "Information",
        Value = "Similarity metrics not available (insufficient data)"
      ))
    }
    
    data.frame(
      Metric = c("Mahalanobis Distance", 
                 "Similarity p-value",
                 "Within Training Distribution"),
      Value = c(
        round(similarity$mahalanobis_distance, 2),
        format.pval(similarity$similarity_p_value, digits = 3),
        ifelse(similarity$within_95pct, "Yes (95% CI)", "No")
      )
    )
  }
  
  output$model_info_mouse <- renderUI({
    if (input$tabs != "mouse") return(HTML("<div>No model trained yet.</div>"))
    get_model_info()
  })
  
  output$model_info_human <- renderUI({
    if (input$tabs != "human") return(HTML("<div>No model trained yet.</div>"))
    get_model_info()
  })
  
  output$model_info_user <- renderUI({
    if (input$tabs != "user") return(HTML("<div>No model trained yet.</div>"))
    
    if (is.na(model_results$predicted_value)) {
      return(HTML("<div>No model trained yet. Please make a prediction first.</div>"))
    }
    
    get_model_info()
  })
  
  get_model_info <- function() {
    if (is.null(model_results) || !is.list(model_results)) {
      return(HTML("<div>No model results available.</div>"))
    }
    
    if (input$tabs == "user") {
      if (is.null(input$user_file)) return(HTML("<div>No data uploaded yet.</div>"))
      if (is.null(model_results$predicted_value) || is.na(model_results$predicted_value)) {
        return(HTML("<div>No model trained yet. Please make a prediction first.</div>"))
      }
      model_type_name <- switch(tolower(model_results$current_model_type),
                                "svm" = "Support Vector Regression (SVR)",
                                "rf"  = "Random Forest (RF)",
                                "nn"  = "Neural Network (NN)",
                                "unknown")
      
      r2_text <- ifelse(!is.null(model_results$r_squared) && !is.na(model_results$r_squared), 
                        round(model_results$r_squared, 3), "N/A")
      mse_text <- ifelse(!is.null(model_results$mse) && !is.na(model_results$mse), 
                         round(model_results$mse, 3), "N/A")
      
      content <- sprintf(
        "<div style='font-family: monospace;'>
        <p><span style='font-weight:bold;color:red;'>User Data Model</span><br>
        Model type: %s<br>
        Training samples: %d<br>
        Tissue: %s<br>
        circRNA: %s<br>
        R-squared: %s<br>
        MSE: %s</p>
      </div>",
        model_type_name,
        model_results$training_samples,
        input$tissue_user,
        input$circRNA_user,
        r2_text,
        mse_text
      )
      return(HTML(content))
    }
    
    source_model <- model_results$source_model
    if (is.null(source_model)) return(HTML("<div>No model trained yet.</div>"))
    
    actual_source_model <- NULL
    model_type_from_wrapper <- model_results$current_model_type
    source_scaling <- NULL
    
    if (is.list(source_model) && "model" %in% names(source_model)) {
      actual_source_model <- source_model$model
      model_type_from_wrapper <- source_model$model_type %||% model_results$current_model_type
      source_scaling <- source_model$scaling_params %||% NULL
    } else {
      actual_source_model <- source_model
    }
    
    model_type_name <- switch(tolower(model_type_from_wrapper),
                              "svm" = "Support Vector Regression (SVR)",
                              "rf"  = "Random Forest (RF)",
                              "nn"  = "Neural Network (NN)",
                              "unknown")
    
    current_tissue <- input[[paste0("tissue_", input$tabs)]]
    current_circRNA <- input[[paste0("circRNA_", input$tabs)]]
    
    source_title <- ifelse(isTRUE(model_results$is_human_only), "Baseline Model (Human Data Only)", "Source Model (Mouse)")
    
    content_parts <- c()
    
    content_parts <- c(content_parts, sprintf(
      "<div style='font-family: monospace;'>
      <p><span style='font-weight:bold;color:red;'>%s</span><br>
      Model type: %s<br>
      Training samples: %d<br>
      Tissue: %s<br>
      circRNA: %s",
      source_title,
      model_type_name,
      model_results$training_samples,
      current_tissue,
      current_circRNA
    ))
    
    if (tolower(model_type_from_wrapper) == "svm" && !is.null(actual_source_model)) {
      if (input$tabs == "human") {
        content_parts <- c(content_parts, 
                           "<br>Kernel: RBF (Gaussian)<br>",
                           "Parameters:<br>",
                           "- C: 10<br>",
                           "- Epsilon: 0.1<br>",
                           "- Sigma: optimized<br>"
        )
        
        sv_count <- tryCatch(length(alphaindex(actual_source_model)[[1]]), error = function(e) "N/A")
        content_parts <- c(content_parts, sprintf("Number of support vectors: %s", sv_count))
      } else {
        content_parts <- c(content_parts,
                           "<br>Kernel: Polynomial<br>",
                           "Parameters:<br>",
                           "- C: 10<br>",
                           "- Epsilon: 0.1<br>",
                           "- Degree: 3<br>",
                           "- Scale: 1<br>",
                           "- Offset: 1<br>"
        )
        
        sv_count <- tryCatch(length(alphaindex(actual_source_model)[[1]]), error = function(e) "N/A")
        content_parts <- c(content_parts, sprintf("Number of support vectors: %s", sv_count))
      }
      
      if (!is.null(source_scaling)) {
        content_parts <- c(content_parts, sprintf(
          "<br><span style='font-weight:bold;'>Scaling Parameters:</span><br>
        Expression mean: %.3f<br>
        Expression sd: %.3f<br>
        Temperature mean: %.3f<br>
        Temperature sd: %.3f",
          source_scaling$Expression_mean %||% 0,
          source_scaling$Expression_sd %||% 1,
          source_scaling$Temperature_mean %||% 0,
          source_scaling$Temperature_sd %||% 1
        ))
      }
      
    } else if (tolower(model_type_from_wrapper) == "rf" && !is.null(actual_source_model)) {
      content_parts <- c(content_parts, sprintf(
        "<br>Number of trees: %d<br>
      mtry: %d",
        actual_source_model$ntree %||% 500,
        actual_source_model$mtry %||% 2
      ))
      
      if (!is.null(actual_source_model$mse)) {
        content_parts <- c(content_parts, sprintf("OOB <br>error: %.4f", tail(actual_source_model$mse,1)))
      }
      
      if (!is.null(actual_source_model$importance)) {
        imp <- importance(actual_source_model)
        top_features <- head(sort(imp[, "%IncMSE"], decreasing = TRUE), 3)
        content_parts <- c(content_parts, "<br><span style='font-weight:bold;'>Top 3 Important Features:</span><br>")
        for (i in 1:length(top_features)) {
          content_parts <- c(content_parts, sprintf("- %s: %.3f", names(top_features)[i], top_features[i]))
        }
      }
      
    } else if (tolower(model_type_from_wrapper) == "nn" && !is.null(actual_source_model)) {
      hidden_units <- if (!is.null(actual_source_model$hidden_units)) actual_source_model$hidden_units else "8"
      decay <- if (!is.null(actual_source_model$decay)) actual_source_model$decay else "0.01"
      content_parts <- c(content_parts, sprintf(
        "<br>Hidden units: %s<br>
      Activation: Linear output<br>
      Weight decay: %s<br>
      Max iterations: 2000",
        hidden_units,
        decay
      ))
    }
    
    content_parts <- c(content_parts, "</p>")
    
    if (!is.null(model_results$fine_tuned_model)) {
      fine_tuned_model <- model_results$fine_tuned_model
      
      actual_fine_tuned_model <- NULL
      fine_tuned_features <- NULL
      n_samples_ft <- NA
      
      if (is.list(fine_tuned_model) && "model" %in% names(fine_tuned_model)) {
        actual_fine_tuned_model <- fine_tuned_model$model
        fine_tuned_features <- fine_tuned_model$feature_cols %||% NULL
        n_samples_ft <- fine_tuned_model$n_samples %||% NA
      } else {
        actual_fine_tuned_model <- fine_tuned_model
      }
      
      content_parts <- c(content_parts, sprintf(
        "<p><span style='font-weight:bold;color:red;'>Fine-tuned Model (Human)</span><br>
      Model type: %s",
        model_type_name))
      
      if (tolower(model_type_from_wrapper) == "svm") {
        content_parts <- c(content_parts, "<br>Kernel: RBF with Domain Adaptation<br>")
        
        if (!is.null(fine_tuned_features) && length(fine_tuned_features) > 0) {
          content_parts <- c(content_parts, "<span style='font-weight:bold;'>Enhanced features:</span><br>")
          for (feat in fine_tuned_features) {
            feat_display <- switch(feat,
                                   "source_pred" = "Source Prediction",
                                   "expr_temp_interaction" = "Expression × Temperature",
                                   "source_expr_interaction" = "Source × Expression",
                                   "source_temp_interaction" = "Source × Temperature",
                                   "source_pred_sq" = "Source²",
                                   "source_pred_cu" = "Source³",
                                   feat)
            content_parts <- c(content_parts, sprintf("- %s", feat_display))
          }
        }
        
        content_parts <- c(content_parts,
                           "Parameters:<br>",
                           "- C: 100<br>",
                           "- Epsilon: 0.01<br>",
                           "- Sigma: 0.1<br>"
        )
        
        if (!is.null(actual_fine_tuned_model) && inherits(actual_fine_tuned_model, "ksvm")) {
          sv_count <- tryCatch(length(alphaindex(actual_fine_tuned_model)[[1]]), error = function(e) "N/A")
          content_parts <- c(content_parts, sprintf("Support vectors: %s", sv_count))
        }
        
      } else if (tolower(model_type_from_wrapper) == "rf") {
        content_parts <- c(content_parts,
                           "<br>Number of trees: 500<br>",
                           "mtry: 3<br>",
                           "Enhanced features included<br>"
        )
        
      } else if (tolower(model_type_from_wrapper) == "nn") {
        hidden_units <- if (!is.null(actual_fine_tuned_model$hidden_units)) actual_fine_tuned_model$hidden_units else 
          if (!is.null(fine_tuned_model$hidden_units)) fine_tuned_model$hidden_units else "10"
        decay <- if (!is.null(actual_fine_tuned_model$decay)) actual_fine_tuned_model$decay else 
          if (!is.null(fine_tuned_model$decay)) fine_tuned_model$decay else "0.05"
        
        content_parts <- c(content_parts, sprintf(
          "<br>Hidden units: %s<br>
        Activation: Linear output<br>
        Weight decay: %s<br>
        Max iterations: 2000",
          hidden_units,
          decay
        ))
        
        if (!is.null(fine_tuned_features) && length(fine_tuned_features) > 0) {
          content_parts <- c(content_parts, "<br><span style='font-weight:bold;'>Enhanced features:</span><br>")
          for (feat in fine_tuned_features) {
            feat_display <- switch(feat,
                                   "source_pred" = "Source Prediction",
                                   "expr_temp_interaction" = "Expression × Temperature",
                                   "source_expr_interaction" = "Source × Expression",
                                   "source_temp_interaction" = "Source × Temperature",
                                   "source_pred_sq" = "Source²",
                                   "source_pred_cu" = "Source³",
                                   feat)
            content_parts <- c(content_parts, sprintf("- %s", feat_display))
          }
        }
        
        if (!is.null(fine_tuned_model$formula_used)) {
          content_parts <- c(content_parts, "<br><span style='font-weight:bold;'>Formula:</span><br>")
          formula_text <- gsub(" ~ ", " ~<br>", deparse(fine_tuned_model$formula_used))
          content_parts <- c(content_parts, formula_text)
        }
      }
      
      content_parts <- c(content_parts, "</p>")
      
      content_parts <- c(content_parts, sprintf(
        "<p><span style='font-weight:bold;'>Transfer Learning Assessment:</span><br>
      %s</p>",
        ifelse(model_results$transfer_compatible %||% FALSE,
               "<span style='color:green;'>Data compatibility: Good ✓</span>",
               "<span style='color:red;'>Data compatibility: Limited - use with caution ⚠</span><br>
              <small>Mouse and human data have different distributions or insufficient overlap</small>")
      ))
      
    } else {
      content_parts <- c(content_parts, "<p>No fine-tuned model available.</p>")
      
      if (input$tabs == "human" && !isTRUE(input$transfer_learning)) {
        content_parts <- c(content_parts, 
                           "<p><span style='color:orange;'>Transfer learning is disabled. Enable it to create a human-specific fine-tuned model.</span></p>")
      } else if (input$tabs == "human" && isTRUE(input$transfer_learning)) {
        content_parts <- c(content_parts, 
                           "<p><span style='color:orange;'>Fine-tuned model could not be created. Possible reasons:</span><br>
        - Insufficient human data for selected tissue/circRNA<br>
        - Incompatible data distributions<br>
        - Model training failed</p>")
      }
    }
    
    if (!is.null(model_results$model_metrics)) {
      content_parts <- c(content_parts, sprintf(
        "<p><span style='font-weight:bold;'>Model Performance:</span><br>
      R²: %.3f<br>
      RMSE: %.2f hours<br>
      MAE: %.2f hours<br>
      Sample size: %d</p>",
        model_results$model_metrics$R2 %||% NA,
        model_results$model_metrics$RMSE %||% NA,
        model_results$model_metrics$MAE %||% NA,
        model_results$model_metrics$N %||% NA
      ))
    }
    
    content_parts <- c(content_parts, "</div>")
    final_content <- paste(content_parts, collapse = "")
    HTML(final_content)
  }
  
  create_actual_predicted_plot <- function() {
    req(model_results$filtered_data, model_results$filtered_predictions)
    df <- model_results$filtered_data
    
    if (is.null(model_results$filtered_predictions) || length(model_results$filtered_predictions) != nrow(df)) {
      return(NULL)
    }
    
    df$predicted <- model_results$filtered_predictions
    
    model_type_name <- switch(tolower(model_results$current_model_type),
                              "svm" = "Support Vector Regression (SVR)",
                              "rf" = "Random Forest (RF)",
                              "nn" = "Neural Network (NN)",
                              "unknown")
    
    plot_title <- paste("Actual vs Predicted PMI (", model_type_name, ")\n",
                        "Training Samples: ", model_results$training_samples, sep = "")
    
    ggplot(df, aes(x = PMI_Hour, y = predicted)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
      labs(title = plot_title,
           x = "Actual PMI (hours)", y = "Predicted PMI (hours)") +
      theme_minimal(base_size = 14) +
      annotate("text", x = min(df$PMI_Hour), y = max(df$predicted), 
               label = paste("R² =", round(cor(df$PMI_Hour, df$predicted)^2, 3)),
               hjust = 0, vjust = 1, size = 5)
  }
  
  create_residual_plot <- function() {
    req(model_results$filtered_data, model_results$filtered_predictions)
    df <- model_results$filtered_data
    
    if (is.null(model_results$filtered_predictions) || length(model_results$filtered_predictions) != nrow(df)) {
      return(NULL)
    }
    
    df$predicted <- model_results$filtered_predictions
    df$residuals <- df$PMI_Hour - df$predicted
    
    model_type_name <- switch(tolower(model_results$current_model_type),
                              "svm" = "Support Vector Regression (SVR)",
                              "rf" = "Random Forest (RF)",
                              "nn" = "Neural Network (NN)",
                              "unknown")
    
    plot_title <- paste("Residual Plot (", model_type_name, ")\n",
                        "Training Samples: ", model_results$training_samples, sep = "")
    
    ggplot(df, aes(x = predicted, y = residuals)) +
      geom_point(alpha = 0.7, size = 3) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "blue") +
      labs(title = plot_title,
           x = "Predicted Values", y = "Residuals (Actual - Predicted)") +
      theme_minimal(base_size = 14)
  }
  
  create_feature_importance_plot <- function() {
    req(model_results$feature_importance)
    
    tryCatch({
      model_type_name <- switch(tolower(model_results$current_model_type),
                                "svm" = "Support Vector Regression (SVR)",
                                "rf" = "Random Forest (RF)",
                                "nn" = "Neural Network (NN)",
                                "unknown")
      
      df <- model_results$feature_importance
      if (!all(c("Feature", "Importance") %in% colnames(df))) return(NULL)
      
      plot_title <- paste("Feature Importance (", model_type_name, ")\n",
                          "Training Samples: ", model_results$training_samples, sep = "")
      
      ggplot(df, aes(x = reorder(Feature, Importance), y = Importance)) +
        geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
        coord_flip() +
        labs(title = plot_title,
             x = "Features", y = "Importance") +
        theme_minimal(base_size = 14)
      
    }, error = function(e) {
      showNotification(paste("Feature importance plot error:", e$message), type = "error")
      NULL
    })
  }
  
  output$actual_predicted_plot_mouse <- renderPlot({ 
    if (input$tabs != "mouse") return(); create_actual_predicted_plot() 
  })
  output$actual_predicted_plot_human <- renderPlot({ 
    if (input$tabs != "human") return(); create_actual_predicted_plot() 
  })
  output$actual_predicted_plot_user <- renderPlot({ 
    if (input$tabs != "user") return(); create_actual_predicted_plot() 
  })
  
  output$residual_plot_mouse <- renderPlot({ 
    if (input$tabs != "mouse") return(); create_residual_plot() 
  })
  output$residual_plot_human <- renderPlot({ 
    if (input$tabs != "human") return(); create_residual_plot() 
  })
  output$residual_plot_user <- renderPlot({ 
    if (input$tabs != "user") return(); create_residual_plot() 
  })
  
  output$feature_importance_plot_mouse <- renderPlot({ 
    if (input$tabs != "mouse") return(); create_feature_importance_plot() 
  })
  output$feature_importance_plot_user <- renderPlot({ 
    if (input$tabs != "user") return(); create_feature_importance_plot() 
  })
  
  create_3d_plot <- function() {
    req(input$tabs)
    if (is.null(model_results$predicted_value) || is.na(model_results$predicted_value)) return(NULL)
    
    current_tab <- input$tabs
    tissue_id <- circ_id <- NULL
    
    if (current_tab == "mouse") {
      tissue_id <- input$tissue_mouse
      circ_id <- input$circRNA_mouse
    } else if (current_tab == "human") {
      tissue_id <- input$tissue_human
      circ_id <- input$circRNA_human
    } else if (current_tab == "user") {
      tissue_id <- input$tissue_user
      circ_id <- input$circRNA_user
      req(input$user_file)
    } else {
      return(NULL)
    }
    
    data <- data_source()
    if (is.null(data) || !all(c("Expression","PMI_Hour","Temperature","circRNA","Tissue") %in% colnames(data))) {
      return(NULL)
    }
    
    # Use case-insensitive filtering for 3D plot
    filtered_data <- filter_by_circRNA(data, tissue_id, circ_id)
    
    if (nrow(filtered_data) == 0) {
      p <- plot_ly() %>%
        add_trace(
          type = "scatter3d", mode = "text",
          x = 0, y = 0, z = 0,
          text = "No data available for selected Tissue and circRNA",
          textfont = list(size = 14, color = "red"),
          showlegend = FALSE
        ) %>%
        layout(
          title = paste("3D Plot for", tissue_id, "-", circ_id),
          scene = list(
            xaxis = list(title = "Expression"),
            yaxis = list(title = "Predicted PMI (hours)"),
            zaxis = list(title = "Temperature (°C)")
          )
        )
      return(p)
    }
    
    coords <- filtered_data[, c("Expression", "PMI_Hour", "Temperature")]
    coords <- coords[complete.cases(coords), , drop = FALSE]
    
    unique_points <- unique(coords)
    if (nrow(unique_points) < 4) {
      p <- plot_ly() %>%
        add_trace(
          x = filtered_data$Expression,
          y = ifelse(filtered_data$PMI_Hour<0,0,filtered_data$PMI_Hour),
          z = filtered_data$Temperature,
          type = "scatter3d",
          mode = "markers",
          marker = list(size = 4),
          name = "Training Data"
        )
      
      new_sample_expr <- input[[paste0("expression_", current_tab)]]
      new_sample_temp <- input[[paste0("temperature_", current_tab)]]
      p <- p %>%
        add_trace(
          x = new_sample_expr,
          y = model_results$predicted_value,
          z = new_sample_temp,
          type = "scatter3d",
          mode = "markers",
          marker = list(color = "red", size = 10, symbol = "diamond"),
          name = "Predicted Sample",
          text = paste("Tissue:", tissue_id, "<br>",
                       "circRNA:", circ_id, "<br>",
                       "Expression:", round(new_sample_expr,2), "<br>",
                       "Temperature:", round(new_sample_temp,2), "°C<br>",
                       "PMI (Predicted):", round(ifelse(model_results$predicted_value<0,0,model_results$predicted_value),2), "hours"),
          hoverinfo = "text"
        ) %>%
        layout(
          title = paste("3D Plot for", tissue_id, "-", circ_id, "\nTraining Samples:", nrow(filtered_data)),
          scene = list(
            xaxis = list(title = "Expression"),
            yaxis = list(title = "PMI (hours)"),
            zaxis = list(title = "Temperature (°C)")
          )
        )
      return(p)
    }
    
    hull_idx <- tryCatch({
      geometry::convhulln(as.matrix(coords), options = "Qt")
    }, error = function(e) {
      message("convhulln failed: ", e$message)
      return(NULL)
    })
    
    p <- plot_ly()
    
    if (!is.null(hull_idx) && nrow(hull_idx) > 0) {
      vert_inds <- unique(as.integer(hull_idx))
      tryCatch({
        hull_points <- as.matrix(coords[vert_inds, , drop = FALSE])
        vx <- hull_points[,1]
        vy <- hull_points[,2]
        vz <- hull_points[,3]
        map_idx <- match(as.integer(hull_idx), vert_inds) - 1
        tri_mat <- matrix(map_idx, ncol = ncol(hull_idx), byrow = FALSE)
        i <- tri_mat[,1]
        j <- tri_mat[,2]
        k <- tri_mat[,3]
        
        p <- p %>%
          add_trace(
            x = vx, y = vy, z = vz,
            i = i, j = j, k = k,
            type = "mesh3d", 
            opacity = 0.3, 
            color = "#FFD700",
            name = "Convex Hull",
            hoverinfo = "none"
          )
      }, error = function(e) {
        message("mesh3d creation failed: ", e$message)
      })
    }
    
    new_sample_expr <- input[[paste0("expression_", current_tab)]]
    new_sample_temp <- input[[paste0("temperature_", current_tab)]]
    p <- p %>%
      add_trace(
        x = filtered_data$Expression,
        y = ifelse(filtered_data$PMI_Hour<0,0,filtered_data$PMI_Hour),
        z = filtered_data$Temperature,
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 4, color = "blue"),
        name = "Training Data",
        text = paste("PMI:", round(filtered_data$PMI_Hour,2), "hours"),
        hoverinfo = "text"
      ) %>%
      add_trace(
        x = new_sample_expr,
        y = model_results$predicted_value,
        z = new_sample_temp,
        type = "scatter3d",
        mode = "markers",
        marker = list(color = "red", size = 10, symbol = "diamond"),
        text = paste(
          "Tissue:", tissue_id, "<br>",
          "circRNA:", circ_id, "<br>",
          "Expression:", round(new_sample_expr, 2), "<br>",
          "Temperature:", round(new_sample_temp, 2), "°C<br>",
          "PMI (Predicted):", round(ifelse(model_results$predicted_value<0,0,model_results$predicted_value),2), "hours"
        ),
        hoverinfo = "text",
        name = "Predicted Sample"
      ) %>%
      layout(
        title = paste("3D Plot for", tissue_id, "-", circ_id, "\nTraining Samples:", nrow(filtered_data)),
        scene = list(
          xaxis = list(title = "Expression"),
          yaxis = list(title = "PMI (hours)"),
          zaxis = list(title = "Temperature (°C)"),
          camera = list(eye = list(x = 1.5, y = 1.5, z = 1.5))
        )
      )
    
    return(p)
  }
  
  output$plot_3D_mouse <- renderPlotly({ 
    if (input$tabs != "mouse") return(); create_3d_plot() 
  })
  output$plot_3D_human <- renderPlotly({ 
    if (input$tabs != "human") return(); create_3d_plot() 
  })
  output$plot_3D_user <- renderPlotly({ 
    if (input$tabs != "user") return(); create_3d_plot() 
  })
  
  output$transfer_learning_plot <- renderPlot({
    
    req(model_results$source_model)
    
    mouse <- mouse_data()
    human <- human_data()
    req(mouse, human)
    
    tryCatch({
      
      current_tissue <- input$tissue_human
      current_circRNA <- input$circRNA_human
      
      if (is.null(current_tissue) || is.null(current_circRNA)) {
        return(NULL)
      }
      
      transfer_enabled <- isTRUE(input$transfer_learning)
      selected_model_type <- tolower(input$model_type_human)
      
      # Use case-insensitive filtering for human data
      human_prep <- human %>%
        filter_by_circRNA(current_tissue, current_circRNA) %>%
        dplyr::select(PMI_Hour, Expression, Temperature)
      
      if (nrow(human_prep) == 0) {
        showNotification("No human data available for selected tissue and circRNA",
                         type = "warning")
        return(NULL)
      }
      
      model_key <- paste(current_tissue, current_circRNA, selected_model_type, "human_only", sep = "_")
      
      human_only_model <- NULL
      if (!is.null(model_results$tissue_circ_models[[model_key]])) {
        human_only_model <- model_results$tissue_circ_models[[model_key]]
      } else {
        if (nrow(human_prep) >= 2) {
          human_only_model <- train_model(human_prep, selected_model_type)
          if (!is.null(human_only_model)) {
            model_results$tissue_circ_models[[model_key]] <- human_only_model
          }
        }
      }
      
      source_scaling_params <- NULL
      if (!is.null(model_results$source_model$scaling_params)) {
        source_scaling_params <- model_results$source_model$scaling_params
      }
      
      human_mouse_preds <- tryCatch({
        
        preds <- predict_model(
          model_results$source_model,
          human_prep,
          selected_model_type,
          scaling_params = source_scaling_params
        )
        
        if (length(preds) == 1 && nrow(human_prep) > 1) {
          rep(preds, nrow(human_prep))
        } else if (length(preds) != nrow(human_prep)) {
          rep(NA, nrow(human_prep))
        } else {
          preds
        }
        
      }, error = function(e) {
        warning(paste("Human source prediction error:", e$message))
        rep(NA, nrow(human_prep))
      })
      
      human_only_preds <- tryCatch({
        if (!is.null(human_only_model)) {
          human_only_scaling_params <- NULL
          if (is.list(human_only_model) && "scaling_params" %in% names(human_only_model)) {
            human_only_scaling_params <- human_only_model$scaling_params
          } else if (!is.null(human_only_model$scaling_params)) {
            human_only_scaling_params <- human_only_model$scaling_params
          }
          
          preds <- predict_model(
            human_only_model,
            human_prep,
            selected_model_type,
            scaling_params = human_only_scaling_params
          )
          
          if (length(preds) == 1 && nrow(human_prep) > 1) {
            rep(preds, nrow(human_prep))
          } else if (length(preds) != nrow(human_prep)) {
            rep(NA, nrow(human_prep))
          } else {
            preds
          }
        } else {
          rep(NA, nrow(human_prep))
        }
      }, error = function(e) {
        warning(paste("Human-only prediction error:", e$message))
        rep(NA, nrow(human_prep))
      })
      
      plot_data <- data.frame(
        PMI_Hour = human_prep$PMI_Hour,
        Prediction = as.numeric(human_only_preds),
        Type = "Human",
        stringsAsFactors = FALSE
      )
      
      if (transfer_enabled) {
        
        # Use case-insensitive filtering for mouse data
        mouse_prep <- mouse %>%
          filter_by_circRNA(current_tissue, current_circRNA) %>%
          dplyr::select(PMI_Hour, Expression, Temperature)
        
        if (nrow(mouse_prep) > 0) {
          
          mouse_preds <- tryCatch({
            
            preds <- predict_model(
              model_results$source_model,
              mouse_prep,
              selected_model_type,
              scaling_params = source_scaling_params
            )
            
            if (length(preds) == 1 && nrow(mouse_prep) > 1) {
              rep(preds, nrow(mouse_prep))
            } else if (length(preds) != nrow(mouse_prep)) {
              rep(NA, nrow(mouse_prep))
            } else {
              preds
            }
            
          }, error = function(e) {
            warning(paste("Mouse prediction error:", e$message))
            rep(NA, nrow(mouse_prep))
          })
          
          mouse_df <- data.frame(
            PMI_Hour = mouse_prep$PMI_Hour,
            Prediction = as.numeric(mouse_preds),
            Type = "Mouse",
            stringsAsFactors = FALSE
          )
          
          plot_data <- dplyr::bind_rows(plot_data, mouse_df)
        }
        
        if (!is.null(model_results$fine_tuned_model)) {
          
          human_prep_ft <- human_prep
          human_prep_ft$source_pred <- as.numeric(human_mouse_preds)
          
          ft_scaling_params <- NULL
          if (!is.null(model_results$fine_tuned_model$scaling_params)) {
            ft_scaling_params <- model_results$fine_tuned_model$scaling_params
          }
          
          human_ft_preds <- tryCatch({
            
            preds <- predict_model(
              model_results$fine_tuned_model,
              human_prep_ft,
              selected_model_type,
              source_model = model_results$source_model,
              scaling_params = ft_scaling_params
            )
            
            if (length(preds) == 1 && nrow(human_prep) > 1) {
              rep(preds, nrow(human_prep))
            } else if (length(preds) != nrow(human_prep)) {
              rep(NA, nrow(human_prep))
            } else {
              preds
            }
            
          }, error = function(e) {
            warning(paste("Fine-tuned prediction error:", e$message))
            rep(NA, nrow(human_prep))
          })
          
          ft_df <- data.frame(
            PMI_Hour = human_prep$PMI_Hour,
            Prediction = as.numeric(human_ft_preds),
            Type = "Human (Fine-tuned)",
            stringsAsFactors = FALSE
          )
          
          if (nrow(ft_df) > 0) {
            plot_data <- dplyr::bind_rows(plot_data, ft_df)
          }
        }
      }
      
      if (nrow(plot_data) == 0) {
        showNotification("No valid predictions available for plotting",
                         type = "warning")
        return(NULL)
      }
      
      plot_data_filtered <- plot_data[!is.na(plot_data$Prediction), ]
      
      if (nrow(plot_data_filtered) == 0) {
        showNotification("No valid predictions available for plotting",
                         type = "warning")
        return(NULL)
      }
      
      if (transfer_enabled) {
        plot_data_filtered$Type <- factor(plot_data_filtered$Type,
                                          levels = c("Mouse",
                                                     "Human",
                                                     "Human (Fine-tuned)"))
        plot_title <- paste("Transfer Learning Performance Comparison\n",
                            current_tissue, "-", current_circRNA)
      } else {
        plot_data_filtered$Type <- factor(plot_data_filtered$Type,
                                          levels = c("Human"))
        plot_title <- paste("Human Model Performance\n",
                            current_tissue, "-", current_circRNA)
      }
      
      r2_values <- plot_data_filtered %>%
        dplyr::group_by(Type) %>%
        dplyr::summarise(
          R2 = tryCatch(
            round(cor(PMI_Hour, Prediction,
                      use = "complete.obs")^2, 3),
            error = function(e) NA
          ),
          n = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::filter(!is.na(R2))
      
      if (nrow(r2_values) > 0) {
        x_min <- min(plot_data_filtered$PMI_Hour, na.rm = TRUE)
        x_max <- max(plot_data_filtered$PMI_Hour, na.rm = TRUE)
        y_min <- min(plot_data_filtered$Prediction, na.rm = TRUE)
        y_max <- max(plot_data_filtered$Prediction, na.rm = TRUE)
        
        r2_labels <- r2_values %>%
          dplyr::mutate(
            label = paste0(Type, ": R² = ", R2),
            x_pos = x_min + (x_max - x_min) * 0.02,
            y_pos = y_max - (y_max - y_min) * (0.05 * (which(Type == unique(Type)) - 1))
          )
      } else {
        r2_labels <- data.frame()
      }
      
      if (transfer_enabled) {
        color_mapping <- c(
          "Mouse" = "#2ca02c",
          "Human" = "#1f77b4",
          "Human (Fine-tuned)" = "#d62728"
        )
      } else {
        color_mapping <- c("Human" = "#1f77b4")
      }
      
      p <- ggplot(plot_data_filtered,
                  aes(x = PMI_Hour,
                      y = Prediction,
                      color = Type)) +
        geom_point(alpha = 0.5, size = 2) +
        geom_abline(intercept = 0, slope = 1,
                    color = "#3B3B3B", linetype = "dashed", size = 0.5) +
        scale_color_manual(values = color_mapping) +
        labs(
          title = plot_title,
          x = "Actual PMI (hours)",
          y = "Predicted PMI (hours)",
          color = "Model Type"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          legend.position = "bottom",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 13, face = "bold"),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)
        )
      
      if (nrow(r2_labels) > 0) {
        for (i in 1:nrow(r2_labels)) {
          p <- p + annotate("text",
                            x = r2_labels$x_pos[i],
                            y = r2_labels$y_pos[i],
                            label = r2_labels$label[i],
                            hjust = 0,
                            vjust = 1,
                            size = 5,
                            fontface = "bold",
                            color = color_mapping[r2_labels$Type[i]])
        }
      }
      
      if (nrow(plot_data_filtered) >= 5 && transfer_enabled && length(unique(plot_data_filtered$Type)) > 1) {
        p <- p + geom_smooth(method = "lm",
                             formula = y ~ x,
                             se = TRUE,
                             alpha = 0.2,
                             aes(fill = Type),
                             show.legend = FALSE)
      } else if (nrow(plot_data_filtered) >= 5 && !transfer_enabled) {
        p <- p + geom_smooth(method = "lm",
                             formula = y ~ x,
                             se = TRUE,
                             alpha = 0.2,
                             color = "#1f77b4",
                             fill = "gray80",
                             show.legend = FALSE)
      }
      
      return(p)
      
    }, error = function(e) {
      
      showNotification(
        paste("Error in transfer_learning_plot:", e$message),
        type = "error",
        duration = 10
      )
      
      print(paste("Transfer learning plot error:", e$message))
      return(NULL)
    })
  })
  
  # Register the filter_by_circRNA function for use in reactive expressions
  # This is important for dplyr::filter compatibility
  filter_by_circRNA_env <- function(data, tissue, circRNA) {
    if (is.null(data) || nrow(data) == 0) return(data)
    
    # First filter by tissue
    tissue_data <- data[data$Tissue == tissue, ]
    if (nrow(tissue_data) == 0) return(tissue_data)
    
    # Filter by circRNA (case-insensitive)
    circRNA_upper <- toupper(circRNA)
    matched_data <- tissue_data[toupper(tissue_data$circRNA) == circRNA_upper, ]
    
    return(matched_data)
  }
  
  # Make the function available in the reactive environment
  environment(filter_by_circRNA_env) <- environment()
  
  # Override the dplyr filter to use case-insensitive matching
  # This allows dplyr::filter to work with case-insensitive circRNA matching
  `%>%` <- dplyr::`%>%`
  
  observeEvent(input$tabs, {
    req(input$tabs)
    
    if (input$tabs == "mouse") {
      updateCheckboxInput(session, "transfer_learning", value = FALSE)
      shinyjs::disable("transfer_learning")
    } else if (input$tabs == "human") {
      updateCheckboxInput(session, "transfer_learning", value = TRUE)
      shinyjs::enable("transfer_learning")
    } else {
      updateCheckboxInput(session, "transfer_learning", value = FALSE)
      shinyjs::disable("transfer_learning")
    }
  })
  
  observeEvent({
    input$tissue_mouse
    input$circRNA_mouse
    input$model_type_mouse
  }, {
    model_results$tissue_circ_models <- list()
  }, ignoreNULL = TRUE)
  
  observeEvent({
    input$tissue_human
    input$circRNA_human
    input$model_type_human
    input$transfer_learning
  }, {
    model_results$tissue_circ_models <- list()
  }, ignoreNULL = TRUE)
  
  observe({
    stats$pageViews <- stats$pageViews + 1
    stats$lastAccess <- Sys.time()
    
    newEntry <- data.frame(
      timestamp = as.character(Sys.time()),
      sessionID = session$token,
      page = input$tabs,
      action = "view",
      stringsAsFactors = FALSE
    )
    
    stats$accessLog <- rbind(stats$accessLog, newEntry)
    
    saveStats()
    saveAccessLog()
  }) %>% bindEvent(input$tabs)
  
  uniqueUsers <- reactive({
    length(unique(stats$accessLog$sessionID))
  })
  
  dailyViews <- reactive({
    stats$accessLog %>%
      mutate(date = as.Date(timestamp)) %>%
      filter(date == Sys.Date()) %>%
      nrow()
  })
  
  weeklyViews <- reactive({
    stats$accessLog %>%
      mutate(date = as.Date(timestamp)) %>%
      filter(date >= Sys.Date() - 6) %>%
      nrow()
  })
  
  monthlyViews <- reactive({
    stats$accessLog %>%
      mutate(date = as.Date(timestamp)) %>%
      filter(format(date, "%Y-%m") == format(Sys.Date(), "%Y-%m")) %>%
      nrow()
  })
  
  output$totalViewsBox <- renderValueBox({
    valueBox(
      format(stats$pageViews, big.mark = ","), 
      "Total Views", 
      icon = icon("eye"),
      color = "purple",
      width = 12
    )
  })
  
  output$dailyViewsBox <- renderValueBox({
    valueBox(
      format(dailyViews(), big.mark = ","), 
      "Today's Views", 
      icon = icon("calendar-day"),
      color = "green",
      width = 12
    )
  })
  
  output$weeklyViewsBox <- renderValueBox({
    valueBox(
      format(weeklyViews(), big.mark = ","), 
      "Last 7 Days", 
      icon = icon("calendar-week"),
      color = "yellow",
      width = 12
    )
  })
  
  output$monthlyViewsBox <- renderValueBox({
    valueBox(
      format(monthlyViews(), big.mark = ","), 
      "This Month", 
      icon = icon("calendar-alt"),
      color = "blue",
      width = 12
    )
  })
  
  output$uniqueUsersBox <- renderValueBox({
    valueBox(
      format(uniqueUsers(), big.mark = ","), 
      "Unique Users", 
      icon = icon("users"),
      color = "red",
      width = 12
    )
  })
  
  output$lastAccessBox <- renderValueBox({
    valueBox(
      value = tags$span(
        format(stats$lastAccess, "%Y-%m-%d %H:%M"),
        style = "font-size: 24px;"
      ), 
      subtitle = "Last Access",
      icon = icon("clock"),
      color = "teal",
      width = 12
    )
  })
  
  observeEvent(input$confirmReset, {
    stats$pageViews <- 0
    stats$userSessions <- 0
    stats$lastAccess <- Sys.time()
    stats$accessLog <- data.frame(
      timestamp = character(),
      sessionID = character(),
      page = character(),
      action = character(),
      stringsAsFactors = FALSE
    )
    
    saveStats()
    saveAccessLog()
    
    removeModal()
    showNotification("Statistics have been reset.", type = "success")
  })
}

##########################
# Run the application
##########################
#shinyApp(ui = ui, server = server)

runApp(
  shinyApp(ui = ui, server = server),
  port=7371,
  host="0.0.0.0",
  launch.browser = TRUE
  
)
