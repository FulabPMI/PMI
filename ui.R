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
library(gridExtra)
library(grid)

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
        .plotly, .shiny-plot-output, .plot-container, .js-plotly-plot {
          cursor: context-menu !important;
        }
        .plot-container:hover, .js-plotly-plot:hover {
          outline: 2px solid #3c8dbc !important;
          outline-offset: 2px;
        }
      ")),
      # Load libraries with proper error handling
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.4.1/html2canvas.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jspdf/2.5.1/jspdf.umd.min.js"),
      tags$script(HTML(paste0("
      // Store the last saved plot data to avoid duplicate saves
        var lastSavedPlotId = null;
        var lastSaveTime = 0;
        
        // Wait for libraries to load
        function waitForLibraries(callback) {
          var checkInterval = setInterval(function() {
            if (typeof html2canvas !== 'undefined' && 
                typeof window.jspdf !== 'undefined' && 
                window.jspdf && window.jspdf.jsPDF) {
              clearInterval(checkInterval);
              callback();
            }
          }, 100);
          setTimeout(function() {
            clearInterval(checkInterval);
            console.warn('Libraries may not have loaded completely');
            callback();
          }, 10000);
        }
        
        // ========== FIXED: Function to save Plotly plots as PDF ==========
async function savePlotlyAsPDF(plotElement, filename) {

  // Prevent duplicate saves within 2 seconds
  const now = Date.now();
  const plotId = plotElement.id || ('plot_' + now);

  if (
    lastSavedPlotId === plotId &&
    (now - lastSaveTime) < 2000
  ) {
    console.log('Duplicate save prevented');
    return;
  }

  if (
    typeof window.jspdf === 'undefined' ||
    !window.jspdf.jsPDF
  ) {
    alert('PDF library is still loading.');
    return;
  }

  // Notification
  const notification = document.createElement('div');
  notification.textContent = '📄 Generating PDF...';
  notification.style.cssText =
    'position:fixed;bottom:20px;right:20px;background:#333;color:#fff;padding:10px;border-radius:5px;z-index:99999;';
  document.body.appendChild(notification);

  try {

    let imgData = null;

    //----------------------------------------------------------
    // Find the CORRECT Plotly graph for THIS plotElement
    //----------------------------------------------------------

    let plotDiv = null;

    // Case 1: plotElement IS the plotly div
    if (
      plotElement.classList &&
      (
        plotElement.classList.contains('js-plotly-plot') ||
        plotElement.classList.contains('plotly-graph-div')
      )
    ) {
      plotDiv = plotElement;
    }

    // Case 2: plotly div inside container
    if (!plotDiv) {
      plotDiv = plotElement.querySelector(
        '.js-plotly-plot, .plotly-graph-div'
      );
    }

    // Case 3: closest ancestor
    if (!plotDiv && plotElement.closest) {
      plotDiv = plotElement.closest(
        '.js-plotly-plot, .plotly-graph-div'
      );
    }

    if (!plotDiv) {
      throw new Error('Could not find Plotly graph');
    }

    //----------------------------------------------------------
    // Export with Plotly.toImage (preferred)
    //----------------------------------------------------------

    if (
      typeof Plotly !== 'undefined' &&
      Plotly.toImage
    ) {

      try {

        // Force redraw so latest state is captured
        await Plotly.redraw(plotDiv);

        const width =
          plotDiv._fullLayout?.width ||
          plotDiv.clientWidth ||
          1200;

        const height =
          plotDiv._fullLayout?.height ||
          plotDiv.clientHeight ||
          900;

        imgData = await Plotly.toImage(plotDiv, {
          format: 'png',
          width: width,
          height: height,
          scale: 3
        });

        console.log('Plotly.toImage succeeded');

      } catch (err) {
        console.error('Plotly.toImage failed', err);
      }
    }

    //----------------------------------------------------------
    // Fallback html2canvas
    //----------------------------------------------------------

    if (!imgData && typeof html2canvas !== 'undefined') {

      const canvas = await html2canvas(plotDiv, {
        scale: 3,
        backgroundColor: '#ffffff',
        useCORS: true,
        logging: false
      });

      imgData = canvas.toDataURL('image/png');

      console.log('html2canvas fallback succeeded');
    }

    if (!imgData) {
      throw new Error('Failed to capture plot');
    }

    //----------------------------------------------------------
    // Create PDF
    //----------------------------------------------------------

    const jsPDF = window.jspdf.jsPDF;

    const pdf = new jsPDF({
      orientation: 'landscape',
      unit: 'mm',
      format: 'a4'
    });

    const pdfWidth =
      pdf.internal.pageSize.getWidth();

    const pdfHeight =
      pdf.internal.pageSize.getHeight();

    const img = new Image();

    img.onload = function () {

      const ratio = Math.min(
        pdfWidth / img.width,
        pdfHeight / img.height
      );

      const finalWidth =
        img.width * ratio;

      const finalHeight =
        img.height * ratio;

      const x =
        (pdfWidth - finalWidth) / 2;

      const y =
        (pdfHeight - finalHeight) / 2;

      pdf.addImage(
        imgData,
        'PNG',
        x,
        y,
        finalWidth,
        finalHeight
      );

      const timestamp =
        new Date()
          .toISOString()
          .replace(/[:.]/g, '-');

      pdf.save(
        `${filename}_${timestamp}.pdf`
      );

      notification.textContent =
        '✅ PDF saved';

      notification.style.background =
        '#28a745';

      setTimeout(() => {
        notification.remove();
      }, 2000);

      lastSavedPlotId = plotId;
      lastSaveTime = Date.now();
    };

    img.onerror = function () {

      pdf.addImage(
        imgData,
        'PNG',
        0,
        0,
        pdfWidth,
        pdfHeight
      );

      const timestamp =
        new Date()
          .toISOString()
          .replace(/[:.]/g, '-');

      pdf.save(
        `${filename}_${timestamp}.pdf`
      );

      notification.remove();
    };

    img.src = imgData;

  }
  catch (err) {

    console.error(err);

    notification.textContent =
      '❌ ' + err.message;

    notification.style.background =
      '#dc3545';

    setTimeout(() => {
      notification.remove();
    }, 3000);
  }
}

  // ========== FIXED: Function to save ggplot plot as PDF ==========

// ======================================================
// IMPROVED GGPlot PDF Export
// ======================================================
async function saveGgplotAsPDF(plotElement, filename) {

  const now = Date.now();
  const plotId =
    plotElement.id ||
    ('ggplot_' + now);

  if (
    lastSavedPlotId === plotId &&
    (now - lastSaveTime) < 2000
  ) {
    console.log('Duplicate save prevented');
    return;
  }

  if (
    typeof window.jspdf === 'undefined' ||
    !window.jspdf.jsPDF
  ) {
    alert('PDF library is loading.');
    return;
  }

  const notification =
    document.createElement('div');

  notification.textContent =
    '📄 Generating PDF...';

  notification.style.cssText =
    'position:fixed;' +
    'bottom:20px;' +
    'right:20px;' +
    'background:#333;' +
    'color:#fff;' +
    'padding:10px;' +
    'border-radius:5px;' +
    'z-index:99999;';

  document.body.appendChild(notification);

  try {

    //--------------------------------------------------
    // Locate actual plot node
    //--------------------------------------------------

    let target = plotElement;

    const img =
      plotElement.querySelector('img');

    const canvas =
      plotElement.querySelector('canvas');

    if (img) {
      target = img;
    } else if (canvas) {
      target = canvas;
    }

    //--------------------------------------------------
    // Ensure image is fully loaded
    //--------------------------------------------------

    if (
      target.tagName === 'IMG' &&
      !target.complete
    ) {
      await new Promise((resolve) => {
        target.onload = resolve;
        target.onerror = resolve;
      });
    }

    //--------------------------------------------------
    // Capture image
    //--------------------------------------------------

    const capturedCanvas =
      await html2canvas(target, {
        scale: 3,
        useCORS: true,
        allowTaint: false,
        backgroundColor: '#ffffff',
        logging: false
      });

    const imgData =
      capturedCanvas.toDataURL(
        'image/png',
        1.0
      );

    //--------------------------------------------------
    // Create PDF
    //--------------------------------------------------

    const jsPDF =
      window.jspdf.jsPDF;

    const pdf =
      new jsPDF({
        orientation:
          capturedCanvas.width >
          capturedCanvas.height
            ? 'landscape'
            : 'portrait',
        unit: 'mm',
        format: 'a4'
      });

    const pdfWidth =
      pdf.internal.pageSize.getWidth();

    const pdfHeight =
      pdf.internal.pageSize.getHeight();

    const imageObj =
      new Image();

    imageObj.onload = function () {

      const ratio = Math.min(
        pdfWidth / imageObj.width,
        pdfHeight / imageObj.height
      );

      const finalWidth =
        imageObj.width * ratio;

      const finalHeight =
        imageObj.height * ratio;

      const x =
        (pdfWidth - finalWidth) / 2;

      const y =
        (pdfHeight - finalHeight) / 2;

      pdf.addImage(
        imgData,
        'PNG',
        x,
        y,
        finalWidth,
        finalHeight
      );

      const timestamp =
        new Date()
          .toISOString()
          .replace(/[:.]/g, '-');

      pdf.save(
        `${filename}_${timestamp}.pdf`
      );

      notification.textContent =
        '✅ PDF saved';

      notification.style.background =
        '#28a745';

      setTimeout(() => {
        notification.remove();
      }, 2000);

      lastSavedPlotId = plotId;
      lastSaveTime = Date.now();
    };

    imageObj.onerror = function () {

      pdf.addImage(
        imgData,
        'PNG',
        0,
        0,
        pdfWidth,
        pdfHeight
      );

      pdf.save(
        `${filename}.pdf`
      );

      notification.remove();
    };

    imageObj.src = imgData;

  }
  catch (err) {

    console.error(
      'GGPlot export failed:',
      err
    );

    notification.textContent =
      '❌ ' + err.message;

    notification.style.background =
      '#dc3545';

    setTimeout(() => {
      notification.remove();
    }, 3000);
  }
}


        // Right-click handler
    function handlePlotRightClick(e) {

    e.preventDefault();
    e.stopPropagation();

    //--------------------------------------------------
    // FIRST: ggplot
    //--------------------------------------------------

    let ggplotElement =
        e.target.closest('.shiny-plot-output');

    if (ggplotElement) {

        let filename =
            ggplotElement.id || 'ggplot_plot';

        saveGgplotAsPDF(
            ggplotElement,
            filename
        );

        return false;
    }

    //--------------------------------------------------
    // SECOND: Plotly
    //--------------------------------------------------

    let plotlyElement =
        e.target.closest('.js-plotly-plot') ||
        e.target.closest('.plotly-graph-div');

    if (plotlyElement) {

        let filename = '3D_Plot';

        savePlotlyAsPDF(
            plotlyElement,
            filename
        );

        return false;
    }
    }

        
        // Initialize when libraries are ready
        waitForLibraries(function() {
          document.removeEventListener('contextmenu', handlePlotRightClick, true);
          document.addEventListener('contextmenu', handlePlotRightClick, true);
          console.log('PDF save functionality initialized');
        });
      ")))
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
                  actionButton("predict_mouse", "Predict PMI"),
                  p(style = "margin-top: 10px; color: #666; font-size: 12px;", 
                    "ℹ️ Tip: Right-click on any plot to save it as PDF")
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
                           div(class = "plot-container", id = "plot_3D_container_mouse",
                               plotlyOutput("plot_3D_mouse", height = "500px")),
                           p(style = "text-align: center; color: #666; font-size: 12px; margin-top: 5px;", 
                             "Right-click anywhere on the plot to save as PDF")),
                  tabPanel("Statistical Plots",
                           fluidRow(
                             column(6, div(class = "plot-container", plotOutput("actual_predicted_plot_mouse", height = "400px"))),
                             column(6, div(class = "plot-container", plotOutput("residual_plot_mouse", height = "400px")))
                           ),
                           fluidRow(
                             column(6, div(class = "plot-container", plotOutput("feature_importance_plot_mouse", height = "400px")))
                           ),
                           p(style = "text-align: center; color: #666; font-size: 12px;", 
                             "Right-click on any plot to save as PDF")),
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
                  uiOutput("compatibility_warning"),
                  p(style = "margin-top: 10px; color: #666; font-size: 12px;", 
                    "ℹ️ Tip: Right-click on any plot to save it as PDF")
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
                           div(class = "plot-container", id = "plot_3D_container_human",
                               plotlyOutput("plot_3D_human", height = "500px")),
                           p(style = "text-align: center; color: #666; font-size: 12px; margin-top: 5px;", 
                             "Right-click anywhere on the plot to save as PDF")),
                  tabPanel("Transfer Learning",
                           h4("Transfer Learning Details"),
                           wellPanel(
                             style = "background-color: #f8f9fa; padding: 15px; border: 1px solid #dee2e6; border-radius: 5px;",
                             uiOutput("model_info_human")
                           ),
                           div(class = "plot-container", plotOutput("transfer_learning_plot", height = "700px")),
                           p(style = "text-align: center; color: #666; font-size: 12px; margin-top: 5px;", 
                             "Right-click on the plot to save as PDF")
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
                  actionButton("predict_user", "Predict PMI"),
                  p(style = "margin-top: 10px; color: #666; font-size: 12px;", 
                    "ℹ️ Tip: Right-click on any plot to save it as PDF")
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
                           div(class = "plot-container", id = "plot_3D_container_user",
                               plotlyOutput("plot_3D_user", height = "500px")),
                           p(style = "text-align: center; color: #666; font-size: 12px; margin-top: 5px;", 
                             "Right-click anywhere on the plot to save as PDF")),
                  tabPanel("Statistical Plots",
                           fluidRow(
                             column(6, div(class = "plot-container", plotOutput("actual_predicted_plot_user", height = "400px"))),
                             column(6, div(class = "plot-container", plotOutput("residual_plot_user", height = "400px")))
                           ),
                           fluidRow(
                             column(6, div(class = "plot-container", plotOutput("feature_importance_plot_user", height = "400px")))
                           ),
                           p(style = "text-align: center; color: #666; font-size: 12px;", 
                             "Right-click on any plot to save as PDF")),
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
                    tags$li("Access detailed model information"),
                    tags$li("Right-click on any plot to save it as PDF")
                  ),
                  
                  h5(strong("3. Human Data")),
                  p("Predict PMI using human data with optional transfer learning from mouse models. Features include:"),
                  tags$ul(
                    tags$li("Same input options as Mouse Data section"),
                    tags$li("Toggle transfer learning option (enabled by default)"),
                    tags$li("View transfer learning performance plots"),
                    tags$li("Right-click on any plot to save it as PDF")
                  ),
                  
                  h5(strong("4. User Data")),
                  p("Upload and analyze your own data. Features include:"),
                  tags$ul(
                    tags$li("Upload Excel files with your experimental data"),
                    tags$li("Select relevant columns from your data"),
                    tags$li("Generate predictions using your custom dataset"),
                    tags$li("View 3D visualizations specific to your data"),
                    tags$li("Right-click on any plot to save it as PDF")
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
                    tags$li("Support Vector Regression with RBF kernel (SVR), Random Forest (RF), and Neural Network (NN) for modeling"),
                    tags$li("Transfer learning for mouse-to-human predictions"),
                    tags$li("3D visualization with convex hulls for data boundaries"),
                    tags$li("Interactive plots with Plotly"),
                    tags$li("Right-click on any plot to save as PDF")
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
# Helper function for case-insensitive matching of circRNA names
##########################
normalize_circRNA <- function(x) {
  tolower(trimws(as.character(x)))
}

get_circRNA_match <- function(data, target_circRNA, tissue_filter = NULL) {
  if (!is.null(tissue_filter)) {
    data_subset <- data[data$Tissue == tissue_filter, ]
  } else {
    data_subset <- data
  }
  
  unique_circRNAs <- unique(data_subset$circRNA)
  
  if (target_circRNA %in% unique_circRNAs) {
    return(target_circRNA)
  }
  
  target_norm <- normalize_circRNA(target_circRNA)
  for (circ in unique_circRNAs) {
    if (normalize_circRNA(circ) == target_norm) {
      return(circ)
    }
  }
  
  return(NULL)
}

filter_by_case_insensitive_circRNA <- function(data, tissue, circRNA) {
  data_subset <- data[data$Tissue == tissue, ]
  actual_circRNA <- get_circRNA_match(data, circRNA, tissue)
  
  if (is.null(actual_circRNA)) {
    return(data[FALSE, ])
  }
  
  data[data$Tissue == tissue & data$circRNA == actual_circRNA, ]
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
    
    expr_mean <- mean(data$Expression, na.rm = TRUE)
    expr_sd <- sd(data$Expression, na.rm = TRUE)
    temp_mean <- mean(data$Temperature, na.rm = TRUE)
    temp_sd <- sd(data$Temperature, na.rm = TRUE)
    
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
      model$scaling_params <- scaling_params
      model$model_type <- "rf"
      return(model)
      
    } else if (model_type == "nn") {
      hidden_size <- min(8, max(2, floor(nrow(scaled_data) / 3)))
      model <- tryCatch({
        nnet(as.formula(formula_str), data = scaled_data, size = hidden_size, linout = TRUE, maxit = 2000, decay = 0.01, trace = FALSE)
      }, error = function(e) {
        nnet(as.formula(formula_str), data = scaled_data, size = max(2, floor(hidden_size / 2)), linout = TRUE, maxit = 1000, decay = 0.01, trace = FALSE)
      })
      model$scaling_params <- scaling_params
      model$model_type <- "nn"
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
      model$scaling_params <- scaling_params
      model$model_type <- "rf"
      model$feature_cols <- keep_features
      model$is_transfer_model <- TRUE
      model$n_samples <- n_samples
      return(model)
      
    } else if (model_type == "nn") {
      hidden_size <- min(10, max(3, floor(n_samples / 3)))
      model <- tryCatch({
        nnet(as.formula(formula_str), data = scaled_data, size = hidden_size, linout = TRUE, maxit = 2000, decay = 0.05, trace = FALSE)
      }, error = function(e) {
        nnet(as.formula(formula_str), data = scaled_data, size = max(2, floor(hidden_size / 2)), linout = TRUE, maxit = 1000, decay = 0.05, trace = FALSE)
      })
      model$scaling_params <- scaling_params
      model$model_type <- "nn"
      model$feature_cols <- keep_features
      model$is_transfer_model <- TRUE
      model$n_samples <- n_samples
      model$hidden_units <- model$n[2]
      model$decay <- 0.05
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
  actual <- actual[valid_idx]
  predicted <- predicted[valid_idx]
  if (length(actual) < 2) return(NULL)
  
  tryCatch({
    cor_val <- cor(actual, predicted)
    if (is.na(cor_val)) cor_val <- 0
    metrics <- list(
      R2 = max(0, cor_val^2), 
      RMSE = sqrt(mean((actual - predicted)^2)),
      MAE = mean(abs(actual - predicted)), 
      MAPE = ifelse(all(actual > 0), mean(abs((actual - predicted)/actual)) * 100, NA),
      Bias = mean(predicted - actual), 
      SD_Error = sd(predicted - actual),
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
      
      actual_valid <- actual[valid_idx]
      pred_valid <- preds[valid_idx]
      cor_val <- tryCatch(cor(actual_valid, pred_valid, use = "complete.obs"), error = function(e) 0)
      if (is.na(cor_val)) cor_val <- 0
      
      fold_results <- data.frame(
        Fold = fold, 
        RMSE = sqrt(mean((actual_valid - pred_valid)^2, na.rm = TRUE)),
        MAE = mean(abs(actual_valid - pred_valid), na.rm = TRUE),
        R2 = cor_val^2, 
        Bias = mean(pred_valid - actual_valid, na.rm = TRUE)
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
            mahalanobis_distance = mahalanobis_dist, 
            similarity_p_value = p_value,
            within_95pct = within_95pct, 
            feature_mean_distance = abs(new_cont - colMeans(training_cont))
          ))
        }
      }
    }
    return(NULL)
  }, error = function(e) return(NULL))
}

##########################
# Function to check transfer learning compatibility (case-insensitive)
##########################
check_transfer_compatibility <- function(mouse_data, human_data, target_tissue, target_circRNA) {
  if (is.null(mouse_data) || is.null(human_data)) return(FALSE)
  
  mouse_sub <- filter_by_case_insensitive_circRNA(mouse_data, target_tissue, target_circRNA)
  human_sub <- filter_by_case_insensitive_circRNA(human_data, target_tissue, target_circRNA)
  
  if (nrow(mouse_sub) == 0 || nrow(human_sub) == 0) return(FALSE)
  
  mouse_temps <- unique(mouse_sub$Temperature)
  human_temps <- unique(human_sub$Temperature)
  temp_overlap <- length(intersect(mouse_temps, human_temps))
  mouse_pmi_range <- range(mouse_sub$PMI_Hour, na.rm = TRUE)
  human_pmi_range <- range(human_sub$PMI_Hour, na.rm = TRUE)
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
          Feature = rownames(imp), 
          Importance = imp[, "%IncMSE"], 
          stringsAsFactors = FALSE
        )
      } else {
        importance_df <- data.frame(
          Feature = features, 
          Importance = rep(1/length(features), length(features)), 
          stringsAsFactors = FALSE
        )
      }
    } else {
      importance_df <- data.frame(
        Feature = features, 
        Importance = rep(1/length(features), length(features)), 
        stringsAsFactors = FALSE
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

