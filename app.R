# UI Code -------------------------

ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = strong("FunCirc"), 
    titleWidth = 250
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 250, 
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Query circRNA Essentiality", tabName = "ess"),
      menuItem("Query circRNA Clinical Expression", tabName = "exp"),
      HTML('&nbsp;'),
      tags$div(
        HTML("<center><h4>He Lab @ UHN</h4><center>")
      )
    )
  ),
  
  # Body
  dashboardBody(
    # Custom CSS for Styling and Enabling Scrolling
    tags$head(
      tags$style(HTML('
        /* Adjust sidebar menu item font size */
        .sidebar-menu > li > a {
          font-size: 16px;
        }
        /* Adjust overall font size */
        body {
          font-size: 14px;
        }
        /* Adjust header background color */
        .skin-blue .main-header .logo, .skin-blue .main-header .navbar {
          background-color: #0D405E;
        }
        /* Adjust sidebar background color */
        .skin-blue .main-sidebar {
          background-color: #80C7F2;
        }
        /* Active tab background color */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
          background-color: #0D405E;
        }
        /* Sidebar menu item color */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
          background-color: #80C7F2;
          color: white;
        }
        /* Sidebar menu item hover */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
          background-color: #0D405E;
        }
        /* Enable vertical scrolling in the content area */
        .content-wrapper, .right-side {
          overflow-y: auto;
          min-height: calc(100vh - 50px);
        }
      '))
    ),
    
    tabItems(
      # Home Tab
      tabItem(
        tabName = "home",
        HTML('&nbsp;'),
        fluidRow(
          column(6, div(style = "padding-right: 20px;", imageOutput("home_img1"))),  # Reduced width to make space for the offset
          column(6),  # Empty column for space
          column(5, imageOutput("home_img2"))   # Adjust width accordingly
        ),
        br(),
        hr(),
        # Hyperlinked text
        HTML('<h4><strong><a href="https://www.hansenhelab.org/" target="_blank" style="text-decoration: underline; color: blue;">He Lab @ UHN</a></strong></h4>'),
        p(style = "text-align: justify; font-size: 16px",
          "FunCirc is a database made with 
  the purpose of being a resource of functional circRNAs, integrating several circRNA screen studies")
    ),
      
      # Essentiality Query Tab
      tabItem(tabName = "ess",
              fluidRow(
                column(12,
                       selectizeInput("study", "Select circRNA Screening Study",
                                      choices = c("", "Chen et al.", "Her et al.", "Li et al.", "Liu et al."),
                                      selected = NULL),
                       
                       # Dynamic UI for Tissue Type (Displayed Only When "Li et al." is Selected)
                       uiOutput("tissue_type_ui"),
                       
                       # Dynamic UI for Cell Line Selection (Liu et al.)
                       uiOutput("cell_line_ui"),
                       
                       # Dynamic UI for circRNA selection
                       uiOutput("circRNA_ui"),
                       
                       # Dynamic UI for Timepoint selection
                       uiOutput("timepoint_ui"),
                       
                       DT::dataTableOutput("table")
                )
              ),
              fluidRow(
                column(12, align = "center",
                       # Conditional Panels for Plotting based on Study
                       
                       # Chen et al. Plots
                       conditionalPanel(
                         condition = "input.study == 'Chen et al.'",
                         div(style = "margin-bottom: 100px;",
                             plotOutput("dotplot_circ_chen", width = "850px", height = "400px")
                         ),
                         plotOutput("dotplot_linear_chen", width = "850px", height = "400px")
                       ),
                       
                       # Her et al. Plots
                       conditionalPanel(
                         condition = "input.study == 'Her et al.'",
                         div(style = "margin-bottom: 100px;",
                             plotOutput("dotplot_circ", width = "850px", height = "400px")
                         ),
                         plotOutput("dotplot_linear", width = "850px", height = "400px")
                       ),
                       
                       # Liu et al. Plot
                       conditionalPanel(
                         condition = "input.study == 'Liu et al.'",
                         plotOutput("liu_plot", width = "800px", height = "600px")
                       ),
                       
                       # Li et al. Plot
                       conditionalPanel(
                         condition = "input.study == 'Li et al.'",
                         plotOutput("li_dotplot", width = "850px", height = "400px")
                       )
                )
              ),
              div(style = "height: 500px;")
      ),
      
      # Clinical Expression Tab
      tabItem(tabName = "exp",
              fluidRow(
                column(12,
                       selectizeInput("dataset", "Select circRNA Clinical Expression Dataset", 
                                      choices = c("", "Arul et al.", "Canadian Prostate Cancer Genome", "In-house Breast Cohort"), 
                                      selected = NULL),
                       selectizeInput("circRNA_clinical", "Query circRNA Clinical Expression", 
                                      choices = "", 
                                      selected = NULL),
                       DT::dataTableOutput("express_table")
                )
              ),
              fluidRow(
                column(width = 12, align = "center",
                       plotOutput("clinical_plot")
                )
              ),
              div(style = "height: 500px;")
      )
    )
  )
)

# Server Code -------------------------
server <- function(input, output, session) {
  
  # Reactive values to store data for studies and clinical datasets
  study_data <- reactiveValues()
  clinical_data <- reactiveValues()
  
  # Reactive Values for Essentiality Query
  query <- reactiveValues(data = NULL)
  
  # Reactive Values for Clinical Expression
  clinical_query <- reactiveValues(data = NULL)
  
  # Reset Outputs When Selections Change -------------------------
  
  # Reset outputs when 'study' changes
  observeEvent(input$study, {
    # Reset inputs
    updateSelectizeInput(session, "circRNA", choices = NULL)
    updateSelectInput(session, "timepoint", choices = NULL)
    updateSelectizeInput(session, "cell_line", choices = NULL)
    updateSelectizeInput(session, "tissue_type", choices = NULL)
    
    # Reset reactive values
    query$data <- NULL
    
    # Clear outputs
    output$table <- DT::renderDataTable(NULL)
    output$dotplot_circ <- renderPlot(NULL)
    output$dotplot_linear <- renderPlot(NULL)
    output$liu_plot <- renderPlot(NULL)
    output$li_dotplot <- renderPlot(NULL)
    output$dotplot_circ_chen <- renderPlot(NULL)
    output$dotplot_linear_chen <- renderPlot(NULL)
    
    # Load data for the selected study if not already loaded
    req(input$study)
    
    # Remove any previously loaded data (unload)
    studies <- c("Her et al.", "Liu et al.", "Li et al.", "Chen et al.")
    for (study in studies) {
      if (study != input$study) {
        study_data[[study]] <- NULL
      }
    }
    gc()
    
    # Now, load data for the selected study if not already loaded
    if (is.null(study_data[[input$study]])) {
      # Load data for the selected study
      if (input$study == "Her et al.") {
        # Load data for Her et al.
        screen_data <- all_data$screen_data
        filtered_annotation <- all_data$filtered_annotation
        # Store data in study_data
        study_data[["Her et al."]] <- list(
          screen_data = screen_data,
          filtered_annotation = filtered_annotation
        )
      } else if (input$study == "Liu et al.") {
        # Load data for Liu et al.
        liu_data_list <- all_data$liu_data_list
        annotation <- all_data$annotation
        study_data[["Liu et al."]] <- list(
          liu_data_list = liu_data_list,
          annotation = annotation
        )
      } else if (input$study == "Li et al.") {
        # Load data for Li et al.
        li_et_al_data_list <- all_data$li_et_al_data_list
        annotation_li <- all_data$annotation
        study_data[["Li et al."]] <- list(
          li_et_al_data_list = li_et_al_data_list,
          annotation_li = annotation_li
        )
      } else if (input$study == "Chen et al.") {
        # Load data for Chen et al.
        T8_merged_list <- all_data$T8_merged_list
        T16_merged_list <- all_data$T16_merged_list
        chen_annotation <- all_data$chen_annotation
        all_circ_data <- all_data$all_circ_data
        all_linear_data <- all_data$all_linear_data
        study_data[["Chen et al."]] <- list(
          T8_merged_list = T8_merged_list,
          T16_merged_list = T16_merged_list,
          chen_annotation = chen_annotation,
          all_circ_data = all_circ_data,
          all_linear_data = all_linear_data
        )
      }
    }
  })
  
  # Reset outputs when 'dataset' changes
  observeEvent(input$dataset, {
    # Reset inputs
    updateSelectizeInput(session, "circRNA_clinical", choices = NULL)
    
    # Reset reactive values
    clinical_query$data <- NULL
    
    # Clear outputs
    output$express_table <- DT::renderDataTable(NULL)
    output$clinical_plot <- renderPlot(NULL)
    
    # Load data for the selected dataset if not already loaded
    req(input$dataset)
    
    # Remove any previously loaded data
    datasets <- c("Arul et al.", "Canadian Prostate Cancer Genome", "In-house Breast Cohort")
    for (dataset in datasets) {
      if (dataset != input$dataset) {
        clinical_data[[dataset]] <- NULL
      }
    }
    gc()
    # Load data for the selected dataset
    if (is.null(clinical_data[[input$dataset]])) {
      if (input$dataset == "Arul et al.") {
        # Load arul data
        arul <- all_data$arul
        # Remove date-like patterns
        # date_pattern_rows <- grep("^\\d{2}-[A-Za-z]{3}$|^[A-Za-z]{3}-\\d{2}$", arul$gene)
        # arul <- arul[-date_pattern_rows, ]
        clinical_data[["Arul et al."]] <- arul
      } else if (input$dataset == "Canadian Prostate Cancer Genome") {
        merged_CPC <- all_data$merged_CPC
        clinical_data[["Canadian Prostate Cancer Genome"]] <- merged_CPC
      } else if (input$dataset == "In-house Breast Cohort") {
        bca_merged <- all_data$bca_merged
        clinical_data[["In-house Breast Cohort"]] <- bca_merged
      }
    }
  })
  
  # Reactive Expressions for Data Based on Selected Study -------------------------
  
  # Reactive data based on selected study
  filtered_annotation_data <- reactive({
    req(input$study)
    if (input$study == "Her et al.") {
      data <- study_data[["Her et al."]]$filtered_annotation
      return(data)
    } else if (input$study == "Liu et al.") {
      req(input$cell_line)
      data <- study_data[["Liu et al."]]$liu_data_list[[input$cell_line]]
      return(data)
    } else if (input$study == "Li et al.") {
      req(input$tissue_type)
      data <- study_data[["Li et al."]]$li_et_al_data_list[[input$tissue_type]]
      return(data)
    } else if (input$study == "Chen et al.") {
      data <- study_data[["Chen et al."]]$chen_annotation
      return(data)
    } else {
      return(NULL)
    }
  })
  
  # Reactive data for screen_data_study based on selected study
  screen_data_study <- reactive({
    req(input$study)
    if (input$study == "Her et al.") {
      data <- study_data[["Her et al."]]$screen_data
      return(data)
    } else if (input$study == "Liu et al.") {
      req(input$cell_line)
      data <- study_data[["Liu et al."]]$liu_data_list[[input$cell_line]]
      return(data)
    } else if (input$study == "Li et al.") {
      req(input$tissue_type)
      data <- study_data[["Li et al."]]$li_et_al_data_list[[input$tissue_type]]
      return(data)
    } else if (input$study == "Chen et al.") {
      # Return the combined data for Chen et al.
      data <- list(circ = study_data[["Chen et al."]]$all_circ_data,
                   linear = study_data[["Chen et al."]]$all_linear_data)
      return(data)
    } else {
      return(NULL)
    }
  })
  
  # Reactive data for clinical datasets
  ClinicalDataset <- reactive({
    req(input$dataset)
    dataset <- clinical_data[[input$dataset]]
    if (!is.null(dataset)) {
      dataset <- dataset[!is.na(dataset$gene),]
    } else {
      dataset <- NULL
    }
    return(dataset)
  })
  
  # Names for expression datasets
  names_exp <- reactive({
    req(input$dataset)
    selected_dataset <- ClinicalDataset()
    clinical_choices <- selected_dataset %>% distinct(circID, .keep_all=TRUE)
    return(clinical_choices)
  })
  
  # Dynamic UI Rendering -------------------------
  
  # Dynamic UI for Tissue Type (Li et al.)
  output$tissue_type_ui <- renderUI({
    req(input$study == "Li et al.")
    selectizeInput("tissue_type", "Select Tissue Type",
                   choices = c("", "Colon", "Pancreas", "Brain", "Skin"),
                   selected = NULL,
                   options = list(placeholder = 'Choose a tissue type'))
  })
  
  # Dynamic UI for Cell Line Selection (Liu et al.)
  output$cell_line_ui <- renderUI({
    req(input$study == "Liu et al.")
    selectizeInput("cell_line", "Select Cell Line",
                   choices = c("", "HT29", "293FT", "HeLa"), 
                   selected = NULL,
                   options = list(placeholder = 'Choose a cell type'))
  })
  
  # Dynamic UI for circRNA Selection
  output$circRNA_ui <- renderUI({
    req(input$study)
    
    if (input$study == "Her et al.") {
      selectizeInput("circRNA", "Query circRNA Essentiality",
                     choices = c("", unique(filtered_annotation_data()$gene)),  # Ensure 'filtered_annotation_data' is defined
                     options = list(placeholder = 'Choose a circRNA'))
    } else if (input$study == "Chen et al.") {
      circRNA_choices <- unique(filtered_annotation_data()$gene)
      selectizeInput("circRNA", "Query circRNA Essentiality",
                     choices = c("", circRNA_choices), selected = NULL,
                     options = list(placeholder = 'Choose a circRNA'))
    } else if (input$study == "Liu et al.") {
      if (!is.null(input$cell_line) && input$cell_line != "") {
        selected_data <- filtered_annotation_data()
        circRNA_choices <- unique(selected_data$gene[!is.na(selected_data$gene)])
        selectizeInput("circRNA", "Query circRNA Essentiality",
                       choices = c("", circRNA_choices), selected = NULL,
                       options = list(placeholder = 'Choose a circRNA'))
      } else {
        selectizeInput("circRNA", "Query circRNA Essentiality",
                       choices = c(""), selected = NULL)
      }
    } else if (input$study == "Li et al.") {
      if (!is.null(input$tissue_type) && input$tissue_type != "") {
        selected_data <- filtered_annotation_data()
        circRNA_choices <- unique(selected_data$gene[!is.na(selected_data$gene)])
        selectizeInput("circRNA", "Query circRNA Essentiality",
                       choices = c("", circRNA_choices), selected = NULL,
                       options = list(placeholder = 'Choose a circRNA'))
      } else {
        selectizeInput("circRNA", "Query circRNA Essentiality",
                       choices = c(""), selected = NULL)
      }
    } else {
      selectizeInput("circRNA", "Query circRNA Essentiality",
                     choices = c(""), selected = NULL)
    }
  })
  
  # Dynamic UI for Timepoint Selection
  output$timepoint_ui <- renderUI({
    req(input$study)
    
    if (input$study == "Her et al.") {
      selectizeInput("timepoint", "Select timepoints to compare",
                     choices = c("", "T8vsT0", "T16vsT0"),
                     options = list(placeholder = 'Select a time comparison'), selected = NULL)
    } else if (input$study == "Chen et al.") {
      selectizeInput("timepoint", "Select timepoints to compare",
                     choices = c("", "T8vsT0", "T16vsT0"),
                     options = list(placeholder = 'Select a time comparison'), selected = NULL)
    } else if (input$study == "Liu et al.") {
      selectInput("timepoint", "Select timepoints to compare",
                  choices = c("D1 vs D30"), selected = "D1 vs D30")
    } else if (input$study == "Li et al.") {
      selectInput("timepoint", "Select timepoints to compare",
                  choices = c("T0 vs T21"), selected = "T0 vs T21")
    } else {
      selectInput("timepoint", "Select timepoints to compare",
                  choices = c(""), selected = NULL)
    }
  })
  
  # Update circRNA choices for clinical datasets
  observe({
    req(input$dataset)
    updateSelectizeInput(session, "circRNA_clinical",
                         choices = c("", unique(names_exp()$gene)), 
                         options = list(placeholder = 'Choose a circRNA'), server = TRUE)
  })
  
  # Data Table Rendering -------------------------
  
  # Update and Render Data Table Based on Selection
  observe({
    req(input$study)
    
    # Check if 'input$circRNA' and 'input$timepoint' are available and not empty
    if (!is.null(input$circRNA) && input$circRNA != "" &&
        !is.null(input$timepoint) && input$timepoint != "") {
      
      if (input$study == "Her et al.") {
        query$data <- filtered_annotation_data() %>% filter(gene == input$circRNA)
        columns2show <- c("ENT","gene", "flanking", "numE", "lengthE",
                          "index", "name", "start", "end")
        
        # Check if 'query$data' is valid and contains the necessary columns
        if (!is.null(query$data) && nrow(query$data) > 0 && all(columns2show %in% colnames(query$data))) {
          output$table <- DT::renderDataTable({
            DT::datatable(query$data[columns2show], 
                          selection = 'single',
                          options = list(dom = 't', scrollX = TRUE),
                          rownames = FALSE)
          })
        } else {
          output$table <- DT::renderDataTable(NULL)
        }
      } else if (input$study == "Liu et al.") {
        query$data <- filtered_annotation_data() %>% filter(gene == input$circRNA)
        columns_to_show_liu <- c("ENT","gene", "flanking", "numE", "lengthE",
                                 "index", "name", "start", "end", "Rank", "CDCscreen score")
        
        if (!is.null(query$data) && nrow(query$data) > 0 && all(columns_to_show_liu %in% colnames(query$data))) {
          output$table <- DT::renderDataTable({
            DT::datatable(query$data[columns_to_show_liu], 
                          selection = 'single',
                          options = list(dom = 't', scrollX = TRUE),
                          rownames = FALSE)
          })
        } else {
          output$table <- DT::renderDataTable(NULL)
        }
      } else if (input$study == "Li et al.") {
        req(input$tissue_type)
        query$data <- filtered_annotation_data() %>% filter(gene == input$circRNA)
        columns_to_show_li <- c("circRNA_ID", "gene", "flanking", "numE", "lengthE", "index", "name", "start", "end")
        
        if (!is.null(query$data) && nrow(query$data) > 0 && all(columns_to_show_li %in% colnames(query$data))) {
          output$table <- DT::renderDataTable({
            DT::datatable(query$data[columns_to_show_li], 
                          selection = 'single',
                          options = list(dom = 't', scrollX = TRUE),
                          rownames = FALSE)
          })
        } else {
          output$table <- DT::renderDataTable(NULL)
        }
      } else if (input$study == "Chen et al.") {
        query$data <- filtered_annotation_data() %>% filter(gene == input$circRNA)
        columns2show <- c("ENT", "gene", "flanking", "numE", "lengthE",
                          "index", "name", "start", "end")
        
        if (!is.null(query$data) && nrow(query$data) > 0 && all(columns2show %in% colnames(query$data))) {
          output$table <- DT::renderDataTable({
            DT::datatable(query$data[columns2show],
                          selection = 'single',
                          options = list(dom = 't', scrollX = TRUE),
                          rownames = FALSE)
          })
        } else {
          output$table <- DT::renderDataTable(NULL)
        }
      }
    } else {
      # If required inputs are missing, clear the table
      output$table <- DT::renderDataTable(NULL)
    }
  })
  
  # Plotting for Selected Study -------------------------
  
  # The plotting code is included here, updated to use data from 'study_data' and 'clinical_data'
  
  # Plotting
  observe({
    req(input$study)
    
    if (input$study == "Her et al." || input$study == "Chen et al.") {
      # For Her et al. and Chen et al.
      if (!is.null(input$circRNA) && input$circRNA != "" &&
          !is.null(input$table_rows_selected) &&
          !is.null(input$timepoint) && input$timepoint != "") {
        
        if (input$study == "Her et al.") {
          # Existing plotting code for Her et al.
          # Define spot size and color functions
          spot.size.s <- function(x) {0.1 + (2 * abs(x))}
          spot.colour.function <- function(x) {
            colours <- rep("white", length(x))
            colours[sign(x) == -1] <- default.colours(2, palette.type = "dotmap")[1]
            colours[sign(x) == 1] <- default.colours(2, palette.type = "dotmap")[2]
            return(colours)
          }
          key.sizes <- seq(-2,2,1)
          
          selRow <- query$data[input$table_rows_selected,]
          req(nrow(selRow) > 0)
          
          selected_linearID <- selRow[["gene"]]
          selected_circID <- selRow[["X"]]
          # Extract the gene and index
          selected_gene <- selRow[["gene"]]
          selected_indices_string <- selRow[["index"]]  # Assuming this is the comma-separated index
          
          # Split the indices string into a vector of numbers
          selected_indices <- as.numeric(unlist(strsplit(selected_indices_string, ",")))
          
          # Format the index string based on the number of indices
          if (length(selected_indices) <= 3) {
            index_string <- paste(selected_indices, collapse = ",")
          } else {
            index_string <- paste(min(selected_indices), "-", max(selected_indices), sep = "")
          }
          
          # Initialize variables
          dots_linear_df <- NULL
          pvals_linear_df <- NULL
          
          if ("T8vsT0" %in% input$timepoint) {
            # DF selected linear or circ - T8 vs T0
            selected_circDF <- screen_data_study() %>% filter(id == selected_circID)
            selected_linearDF <- screen_data_study() %>% filter(id == selected_linearID)
            
            # Build DF circ for dotmap input - T8 vs T0
            log2FC_circ <- selected_circDF$T8vsT0.gene_summary.neg.lfc
            
            # Cell lines
            cell_line_circ <- selected_circDF$sample
            
            # pvals
            pos_pvals_circ <- selected_circDF$T8vsT0.gene_summary.pos.p.value
            neg_pvals_circ <- selected_circDF$T8vsT0.gene_summary.neg.p.value
            
            # dots for dotmap -> create new DF with cell lines and log2FC
            dots_circ_df <- data.frame(cell_line_circ, log2FC_circ)
            dots_circ_df <- as.data.frame(t(dots_circ_df))
            dots_circ_df <- dots_circ_df %>%
              row_to_names(row_number = 1)
            dots_circ_df[] <- lapply(dots_circ_df, type.convert, as.is = TRUE)
            dots_circ_df <- dots_circ_df[rep(1, 2),]
            # Construct the circRNA name with the gene and index
            circID_to_name <- paste("circ", selected_gene, "(", index_string, ")", sep = "")
            rownames(dots_circ_df) <- c(paste(circID_to_name, "pos", sep = "_"),
                                        paste(circID_to_name, "neg", sep = "_"))
            
            # same as above but make new data frame with cell lines, pos and neg pvals
            pvals_circ_df <- data.frame(cell_line_circ, pos_pvals_circ, neg_pvals_circ)
            pvals_circ_df <- as.data.frame(t(pvals_circ_df))
            pvals_circ_df <- pvals_circ_df %>%
              row_to_names(row_number = 1)
            pvals_circ_df[] <- lapply(pvals_circ_df, type.convert, as.is = TRUE)
            
            # Build DF linear for dotmap input - T8 vs T0
            log2FC_linear <- selected_linearDF$T8vsT0.gene_summary.neg.lfc
            cell_line_linear <- selected_linearDF$sample
            pos_pvals_linear <- selected_linearDF$T8vsT0.gene_summary.pos.p.value
            neg_pvals_linear <- selected_linearDF$T8vsT0.gene_summary.neg.p.value
            
            if (!is.null(selected_linearDF) && nrow(selected_linearDF) > 0) {
              dots_linear_df <- data.frame(cell_line_linear, log2FC_linear)
              dots_linear_df <- as.data.frame(t(dots_linear_df))
              dots_linear_df <- dots_linear_df %>%
                row_to_names(row_number = 1)
              dots_linear_df[] <- lapply(dots_linear_df, type.convert, as.is = TRUE)
              dots_linear_df <- dots_linear_df[rep(1, 2),]
              rownames(dots_linear_df) <- c(paste(selected_linearID, "pos", sep = "_"),
                                            paste(selected_linearID, "neg", sep = "_"))
              
              pvals_linear_df <- data.frame(cell_line_linear, pos_pvals_linear, neg_pvals_linear)
              pvals_linear_df <- as.data.frame(t(pvals_linear_df))
              pvals_linear_df <- pvals_linear_df %>%
                row_to_names(row_number = 1)
              pvals_linear_df[] <- lapply(pvals_linear_df, type.convert, as.is = TRUE)
            } else {
              # Linear data is missing
              dots_linear_df <- NULL
              pvals_linear_df <- NULL
            }
          } else if ("T16vsT0" %in% input$timepoint) { # if T16vsT0 is selected
            selected_circDF <- screen_data_study() %>% filter(id == selected_circID)
            selected_linearDF <- screen_data_study() %>% filter(id == selected_linearID)
            
            # Build DF circ for dotmap input - T16 vs T0
            log2FC_circ <- selected_circDF$T16vsT0.gene_summary.neg.lfc
            cell_line_circ <- selected_circDF$sample
            pos_pvals_circ <- selected_circDF$T16vsT0.gene_summary.pos.p.value
            neg_pvals_circ <- selected_circDF$T16vsT0.gene_summary.neg.p.value
            
            dots_circ_df <- data.frame(cell_line_circ, log2FC_circ)
            dots_circ_df <- as.data.frame(t(dots_circ_df))
            dots_circ_df <- dots_circ_df %>%
              row_to_names(row_number = 1)
            dots_circ_df[] <- lapply(dots_circ_df, type.convert, as.is = TRUE)
            dots_circ_df <- dots_circ_df[rep(1, 2),]
            # Construct the circRNA name with the gene and index
            circID_to_name <- paste("circ", selected_gene, "(", index_string, ")", sep = "")
            rownames(dots_circ_df) <- c(paste(circID_to_name, "pos", sep = "_"),
                                        paste(circID_to_name, "neg", sep = "_"))
            
            pvals_circ_df <- data.frame(cell_line_circ, pos_pvals_circ, neg_pvals_circ)
            pvals_circ_df <- as.data.frame(t(pvals_circ_df))
            pvals_circ_df <- pvals_circ_df %>%
              row_to_names(row_number = 1)
            pvals_circ_df[] <- lapply(pvals_circ_df, type.convert, as.is = TRUE)
            
            # Build DF linear for dotmap input - T16 vs T0
            # Check if linear data exists
            if (!is.null(selected_linearDF) && nrow(selected_linearDF) > 0) {
              log2FC_linear <- selected_linearDF$T16vsT0.gene_summary.neg.lfc
              cell_line_linear <- selected_linearDF$sample
              pos_pvals_linear <- selected_linearDF$T16vsT0.gene_summary.pos.p.value
              neg_pvals_linear <- selected_linearDF$T16vsT0.gene_summary.neg.p.value
              
              dots_linear_df <- data.frame(cell_line_linear, log2FC_linear)
              dots_linear_df <- as.data.frame(t(dots_linear_df))
              dots_linear_df <- dots_linear_df %>%
                row_to_names(row_number = 1)
              dots_linear_df[] <- lapply(dots_linear_df, type.convert, as.is = TRUE)
              dots_linear_df <- dots_linear_df[rep(1, 2),]
              rownames(dots_linear_df) <- c(paste(selected_linearID, "pos", sep = "_"),
                                            paste(selected_linearID, "neg", sep = "_"))
              
              pvals_linear_df <- data.frame(cell_line_linear, pos_pvals_linear, neg_pvals_linear)
              pvals_linear_df <- as.data.frame(t(pvals_linear_df))
              pvals_linear_df <- pvals_linear_df %>%
                row_to_names(row_number = 1)
              pvals_linear_df[] <- lapply(pvals_linear_df, type.convert, as.is = TRUE)
            } else {
              # Linear data is missing
              dots_linear_df <- NULL
              pvals_linear_df <- NULL
            }
          } else {
            # Handle case where timepoint is not selected or invalid
            return(NULL)
          }
          
          # Output plot -> dotplot_circ 
          output$dotplot_circ <- renderPlot({
            create.dotmap(x = dots_circ_df,
                          bg.data = pvals_circ_df, 
                          spot.size.function = spot.size.s,
                          main = "circRNA essentiality",
                          main.cex = 2,
                          xaxis.cex = 1.5,
                          yaxis.cex = 2,
                          xaxis.tck = 0,
                          yaxis.tck = 0,
                          xlab.label = 'p-value',
                          xlab.cex = 2,
                          ylab.cex = 2,
                          left.padding = 2,
                          right.padding = 2, 
                          bottom.padding = 10,
                          top.padding = 10, 
                          pch = 21,
                          pch.border.col = 'transparent',
                          spot.colour.function = spot.colour.function,
                          key = list(
                            space = 'right',
                            points = list(
                              cex = spot.size.s(key.sizes),
                              col = spot.colour.function(key.sizes),
                              pch = 19
                            ),
                            text = list(
                              lab = as.character(key.sizes),
                              cex = 3,
                              adj = 1
                            ),
                            title = 'log\u2082FC',
                            cex = 1.5,
                            background = 'white'
                          ),
                          key.top = 1,
                          colourkey = TRUE,
                          colour.scheme = c("black", "white"),
                          colour.centering.value = 0.05,
                          at = c(0, 0.05, 0.2, 0.8, 1),
                          colourkey.labels.at = c(0.05, 0.2, 0.8, 1),
                          colourkey.labels = c(0.05, 0.2, 0.8, 1),
                          bg.alpha = 1,
                          na.spot.size = 3,
                          colourkey.cex = 2,
                          add.grid = TRUE,
                          col.lwd = 1,
                          style = 'Nature',
                          width = 12,
                          height = 15,
                          col.colour = 'grey', 
                          row.colour = 'grey')
          },
          width = function() { 850 }, height = function() { 400 })
          
          # Output plot -> dotplot_linear 
          # Conditionally render linear plot
          if (!is.null(dots_linear_df) && !is.null(pvals_linear_df)) {
            output$dotplot_linear <- renderPlot({
              create.dotmap(x = dots_linear_df,
                            bg.data = pvals_linear_df, 
                            spot.size.function = spot.size.s,
                            main = "linear essentiality",
                            main.cex = 2,
                            xaxis.cex = 1.5,
                            yaxis.cex = 2,
                            xaxis.tck = 0,
                            yaxis.tck = 0,
                            xlab.label = 'p-value',
                            xlab.cex = 2,
                            ylab.cex = 2,
                            left.padding = 2,
                            right.padding = 2, 
                            bottom.padding = 10,
                            top.padding = 10,  
                            pch = 21,
                            pch.border.col = 'transparent',
                            spot.colour.function = spot.colour.function,
                            key = list(
                              space = 'right',
                              points = list(
                                cex = spot.size.s(key.sizes),
                                col = spot.colour.function(key.sizes),
                                pch = 19
                              ),
                              text = list(
                                lab = as.character(key.sizes),
                                cex = 3,
                                adj = 1
                              ),
                              title = 'log\u2082FC',
                              cex = 1.5,
                              background = 'white'
                            ),
                            key.top = 1,
                            colourkey = TRUE,
                            colour.scheme = c("black", "white"),
                            colour.centering.value = 0.05,
                            at = c(0, 0.05, 0.2, 0.8, 1),
                            colourkey.labels.at = c(0.05, 0.2, 0.8, 1),
                            colourkey.labels = c(0.05, 0.2, 0.8, 1),
                            bg.alpha = 1,
                            na.spot.size = 3,
                            colourkey.cex = 2,
                            add.grid = TRUE,
                            col.lwd = 1,
                            style = 'Nature',
                            width = 12,
                            height = 15,
                            col.colour = 'grey', 
                            row.colour = 'grey')
            },
            width = function() { 850 }, height = function() { 400 })
          } else {
            output$dotplot_linear <- renderPlot(NULL)
            showNotification("No corresponding linear data available for this circRNA.", type = "warning")
          }
        } else if (input$study == "Chen et al.") {
          # Define spot size and color functions
          spot.size.s <- function(x) {0.1 + (2 * abs(x))}
          spot.colour.function <- function(x) {
            colours <- rep("white", length(x))
            colours[sign(x) == -1] <- default.colours(2, palette.type = "dotmap")[1]
            colours[sign(x) == 1] <- default.colours(2, palette.type = "dotmap")[2]
            return(colours)
          }
          key.sizes <- seq(-2,2,1)
          
          selRow <- query$data[input$table_rows_selected,]
          req(nrow(selRow) > 0)
          
          # Use 'screenID' instead of 'id' for matching
          selected_circID <- selRow[["screenID"]]
          selected_gene <- selRow[["gene"]]
          selected_indices_string <- selRow[["index"]]  # Assuming 'index' column is available
          
          # Handle the indices as before
          selected_indices <- as.numeric(unlist(strsplit(selected_indices_string, ",")))
          if (length(selected_indices) <= 3) {
            index_string <- paste(selected_indices, collapse = ",")
          } else {
            index_string <- paste(min(selected_indices), "-", max(selected_indices), sep = "")
          }
          # Initialize variables
          dots_linear_df <- NULL
          pvals_linear_df <- NULL
          
          # Extract data for the selected circRNA across all cell lines
          screen_data <- screen_data_study()
          circ_data <- screen_data$circ %>% filter(id == selected_circID, timepoint == input$timepoint)
          linear_data <- screen_data$linear %>% filter(id == selected_circID, timepoint == input$timepoint)
          
          # Build DF circ for dotmap input
          log2FC_circ <- circ_data$neg.lfc
          cell_line_circ <- circ_data$cell_line
          pos_pvals_circ <- circ_data$pos.p.value
          neg_pvals_circ <- circ_data$neg.p.value
          
          # Create data frames for dotmap
          dots_circ_df <- data.frame(cell_line_circ, log2FC_circ)
          dots_circ_df <- as.data.frame(t(dots_circ_df))
          dots_circ_df <- dots_circ_df %>%
            row_to_names(row_number = 1)
          dots_circ_df[] <- lapply(dots_circ_df, type.convert, as.is = TRUE)
          dots_circ_df <- dots_circ_df[rep(1, 2),]
          # Construct the circRNA name with the gene and index
          circID_to_name <- paste("circ", selected_gene, "(", index_string, ")", sep = "")
          rownames(dots_circ_df) <- c(paste(circID_to_name, "pos", sep = "_"),
                                      paste(circID_to_name, "neg", sep = "_"))
          
          pvals_circ_df <- data.frame(cell_line_circ, pos_pvals_circ, neg_pvals_circ)
          pvals_circ_df <- as.data.frame(t(pvals_circ_df))
          pvals_circ_df <- pvals_circ_df %>%
            row_to_names(row_number = 1)
          pvals_circ_df[] <- lapply(pvals_circ_df, type.convert, as.is = TRUE)
          
          # Check if linear data exists
          if (!is.null(linear_data) && nrow(linear_data) > 0) {
            # Repeat for linear data
            log2FC_linear <- linear_data$neg.lfc
            cell_line_linear <- linear_data$cell_line
            pos_pvals_linear <- linear_data$pos.p.value
            neg_pvals_linear <- linear_data$neg.p.value
            
            dots_linear_df <- data.frame(cell_line_linear, log2FC_linear)
            dots_linear_df <- as.data.frame(t(dots_linear_df))
            dots_linear_df <- dots_linear_df %>%
              row_to_names(row_number = 1)
            dots_linear_df[] <- lapply(dots_linear_df, type.convert, as.is = TRUE)
            dots_linear_df <- dots_linear_df[rep(1, 2),]
            rownames(dots_linear_df) <- c(paste(selected_gene, "pos", sep = "_"),
                                          paste(selected_gene, "neg", sep = "_"))
            
            pvals_linear_df <- data.frame(cell_line_linear, pos_pvals_linear, neg_pvals_linear)
            pvals_linear_df <- as.data.frame(t(pvals_linear_df))
            pvals_linear_df <- pvals_linear_df %>%
              row_to_names(row_number = 1)
            pvals_linear_df[] <- lapply(pvals_linear_df, type.convert, as.is = TRUE)
          } else {
            # Linear data is missing
            dots_linear_df <- NULL
            pvals_linear_df <- NULL
          }
          
          # Render the plots
          output$dotplot_circ_chen <- renderPlot({
            create.dotmap(x = dots_circ_df,
                          bg.data = pvals_circ_df, 
                          spot.size.function = spot.size.s,
                          main = "circRNA essentiality",
                          main.cex = 2,
                          xaxis.cex = 1.5,
                          yaxis.cex = 2,
                          xaxis.tck = 0,
                          yaxis.tck = 0,
                          xlab.label = 'p-value',
                          xlab.cex = 2,
                          ylab.cex = 2,
                          left.padding = 2,
                          right.padding = 2, 
                          bottom.padding = 10,
                          top.padding = 10, 
                          pch = 21,
                          pch.border.col = 'transparent',
                          spot.colour.function = spot.colour.function,
                          key = list(
                            space = 'right',
                            points = list(
                              cex = spot.size.s(key.sizes),
                              col = spot.colour.function(key.sizes),
                              pch = 19
                            ),
                            text = list(
                              lab = as.character(key.sizes),
                              cex = 3,
                              adj = 1
                            ),
                            title = 'log\u2082FC',
                            cex = 1.5,
                            background = 'white'
                          ),
                          key.top = 1,
                          colourkey = TRUE,
                          colour.scheme = c("black", "white"),
                          colour.centering.value = 0.05,
                          at = c(0, 0.05, 0.2, 0.8, 1),
                          colourkey.labels.at = c(0.05, 0.2, 0.8, 1),
                          colourkey.labels = c(0.05, 0.2, 0.8, 1),
                          bg.alpha = 1,
                          na.spot.size = 3,
                          colourkey.cex = 2,
                          add.grid = TRUE,
                          col.lwd = 1,
                          style = 'Nature',
                          width = 12,
                          height = 15,
                          col.colour = 'grey', 
                          row.colour = 'grey')
          },
          width = function() { 850 }, height = function() { 400 })
          
          # Output plot -> dotplot_linear 
          # Conditionally render linear plot
          if (!is.null(dots_linear_df) && !is.null(pvals_linear_df)) {
            output$dotplot_linear_chen <- renderPlot({
              create.dotmap(x = dots_linear_df,
                            bg.data = pvals_linear_df, 
                            spot.size.function = spot.size.s,
                            main = "linear essentiality",
                            main.cex = 2,
                            xaxis.cex = 1.5,
                            yaxis.cex = 2,
                            xaxis.tck = 0,
                            yaxis.tck = 0,
                            xlab.label = 'p-value',
                            xlab.cex = 2,
                            ylab.cex = 2,
                            left.padding = 2,
                            right.padding = 2, 
                            bottom.padding = 10,
                            top.padding = 10,  
                            pch = 21,
                            pch.border.col = 'transparent',
                            spot.colour.function = spot.colour.function,
                            key = list(
                              space = 'right',
                              points = list(
                                cex = spot.size.s(key.sizes),
                                col = spot.colour.function(key.sizes),
                                pch = 19
                              ),
                              text = list(
                                lab = as.character(key.sizes),
                                cex = 3,
                                adj = 1
                              ),
                              title = 'log\u2082FC',
                              cex = 1.5,
                              background = 'white'
                            ),
                            key.top = 1,
                            colourkey = TRUE,
                            colour.scheme = c("black", "white"),
                            colour.centering.value = 0.05,
                            at = c(0, 0.05, 0.2, 0.8, 1),
                            colourkey.labels.at = c(0.05, 0.2, 0.8, 1),
                            colourkey.labels = c(0.05, 0.2, 0.8, 1),
                            bg.alpha = 1,
                            na.spot.size = 3,
                            colourkey.cex = 2,
                            add.grid = TRUE,
                            col.lwd = 1,
                            style = 'Nature',
                            width = 12,
                            height = 15,
                            col.colour = 'grey', 
                            row.colour = 'grey')
            },
            width = function() { 850 }, height = function() { 400 })
          } else {
            output$dotplot_linear_chen <- renderPlot(NULL)
            showNotification("No corresponding linear data available for this circRNA.", type = "warning")
          }
        }
      } else {
        # Clear plots if inputs are missing
        if (input$study == "Her et al.") {
          output$dotplot_circ <- renderPlot(NULL)
          output$dotplot_linear <- renderPlot(NULL)
        } else if (input$study == "Chen et al.") {
          output$dotplot_circ_chen <- renderPlot(NULL)
          output$dotplot_linear_chen <- renderPlot(NULL)
        }
      }
    } else if (input$study == "Liu et al.") {
      # For Liu et al., timepoint is not required
      if (!is.null(input$circRNA) && input$circRNA != "" &&
          !is.null(input$table_rows_selected)) {
        
        # Existing plotting code for Liu et al.
        selected_data <- query$data[input$table_rows_selected,]
        req(nrow(selected_data) > 0)
        
        data_to_plot <- screen_data_study()
        selected_gene <- selected_data$Gene
        
        output$liu_plot <- renderPlot({
          # Filter out rows with NA values in Rank or CDCscreen score
          data_to_plot <- screen_data_study() %>% 
            filter(!is.na(Rank), !is.na(`CDCscreen score`))
          selected_data <- query$data[input$table_rows_selected,]
          
          req(nrow(selected_data) > 0)
          
          # Extract the gene and the index column
          selected_gene <- selected_data$gene
          selected_indices_string <- selected_data$index  # Assuming this is the comma-separated index column
          
          # Split the indices string into a vector of numbers
          selected_indices <- as.numeric(unlist(strsplit(selected_indices_string, ",")))
          
          # Format the index string based on the number of indices
          if (length(selected_indices) <= 3) {
            index_string <- paste(selected_indices, collapse = ",")
          } else {
            index_string <- paste(min(selected_indices), "-", max(selected_indices), sep = "")
          }
          
          # Construct the circRNA label
          selected_circRNA_ID <- paste("circ", selected_gene, "(", index_string, ")", sep = "")
          
          ggplot(data_to_plot, aes(x = as.numeric(Rank), y = as.numeric(`CDCscreen score`))) +
            geom_smooth(method = "loess", color = "black", se = FALSE) +  # Line of best fit for all circRNAs
            geom_point(color = "grey") +  # All circRNA points
            geom_point(data = selected_data, aes(x = as.numeric(Rank), y = as.numeric(`CDCscreen score`)), 
                       color = "red", size = 4) +  # Highlight selected circRNA with larger point size
            geom_hline(yintercept = 2, linetype = "dashed", color = "blue") +  # Threshold line at y = 2
            ggrepel::geom_text_repel(data = selected_data, aes(x = as.numeric(Rank), y = as.numeric(`CDCscreen score`), label = selected_circRNA_ID), 
                                     color = "purple", size = 5, fontface = "bold", box.padding = 0.5) +  # Use ggrepel for labels
            labs(title = paste("CDCscreen Scores in", input$cell_line),
                 x = "Rank of circRNAs",
                 y = "CDCscreen score") +
            theme_minimal() +
            theme(
              plot.title = element_text(size = 18, face = "bold"),  # Increase title size
              axis.title.x = element_text(size = 16),  # Increase x-axis title size
              axis.title.y = element_text(size = 16),  # Increase y-axis title size
              axis.text.x = element_text(size = 14),   # Increase x-axis tick text size
              axis.text.y = element_text(size = 14),   # Increase y-axis tick text size
              plot.margin = margin(10, 10, 10, 20)     # Increase plot margins to allow more space for the labels
            ) +
            xlim(min(as.numeric(data_to_plot$Rank), na.rm = TRUE), max(as.numeric(data_to_plot$Rank), na.rm = TRUE)) +  # Adjust x-axis limits
            ylim(min(as.numeric(data_to_plot$`CDCscreen score`), na.rm = TRUE), max(as.numeric(data_to_plot$`CDCscreen score`), na.rm = TRUE))  # Adjust y-axis limits
        }, width = 800, height = 600) 
      } else {
        output$liu_plot <- renderPlot(NULL)
      }
    } else if (input$study == "Li et al.") {
      # For Li et al., timepoint is not required
      if (!is.null(input$circRNA) && input$circRNA != "" &&
          !is.null(input$table_rows_selected)) {
        
        # Plotting logic for Li et al.
        # Extract the selected row
        selected_row <- query$data[input$table_rows_selected,]
        req(nrow(selected_row) > 0)
        
        # Extract circRNA_ID and gene
        selected_gene <- selected_row$gene
        selected_indices_string <- selected_row$index  # Assuming this is the column with the comma-separated indices
        
        # Split the indices string into a vector of numbers
        selected_indices <- as.numeric(unlist(strsplit(selected_indices_string, ",")))
        
        # Format the index string based on the number of indices
        if (length(selected_indices) <= 3) {
          index_string <- paste(selected_indices, collapse = ",")
        } else {
          index_string <- paste(min(selected_indices), "-", max(selected_indices), sep = "")
        }
        
        # Construct the circRNA name
        selected_circRNA_ID <- paste("circ", selected_gene, "(", index_string, ")", sep = "")
        
        # Determine tissue type and get the data
        tissue_type <- input$tissue_type
        tissue_data <- study_data[["Li et al."]]$li_et_al_data_list[[tissue_type]]
        
        # Get the cell line columns by excluding known columns
        known_columns <- c("circRNA_ID", "gene", "flanking", "numE", 
                           "lengthE", "index", "name", "start", "end", 
                           "X", "type", "pos", "strand", "chr", "id",
                           "ENT", "ENS")
        cell_line_cols <- setdiff(names(tissue_data), known_columns)
        
        # Extract log2FC values for the selected circRNA across the relevant cell lines
        log2fc_values <- selected_row %>% select(all_of(cell_line_cols))
        
        # Create a data frame for plotting
        plot_data <- data.frame(
          Cell_Line = cell_line_cols,
          log2FC = as.numeric(log2fc_values[1, ])
        )
        
        # Define spot size function
        spot.size.med <- function(x) { abs(x) / 3 }
        
        spot.size.s <- function(x) {0.1 + (2 * abs(x))}
        spot.colour.function <- function(x) {
          colours <- rep("white", length(x))
          colours[sign(x) == -1] <- default.colours(2, palette.type = "dotmap")[1]
          colours[sign(x) == 1] <- default.colours(2, palette.type = "dotmap")[2]
          return(colours)
        }
        key.sizes <- seq(-2,2,1)
        
        # Convert to matrix format required by create.dotmap
        dotmap_matrix <- matrix(plot_data$log2FC, nrow = 1,
                                dimnames = list(selected_circRNA_ID, plot_data$Cell_Line))
        
        # Render dotplot 
        output$li_dotplot <- renderPlot({
          create.dotmap(
            x = dotmap_matrix,
            main = '',
            spot.size.function = spot.size.s,
            main.cex = 2,
            xaxis.cex = 1.5,
            yaxis.cex = 2,
            xaxis.tck = 0,
            yaxis.tck = 0,
            xlab.label = 'Cell Line',
            ylab.label = '',
            xlab.cex = 2,
            ylab.cex = 2,
            xaxis.lab = colnames(dotmap_matrix),
            yaxis.lab = rownames(dotmap_matrix),
            left.padding = 1,
            right.padding = 1,
            bottom.padding = 15,
            top.padding = 15,
            pch = 21,
            pch.border.col = 'transparent',
            spot.colour.function = spot.colour.function,
            key = list(
              space = 'right',
              points = list(
                cex = spot.size.s(seq(-3, 3, by = 1)),
                col = spot.colour.function(seq(-3, 3, by = 1)),
                pch = 19,
                height = 0.8 
              ),
              text = list(
                lab = as.character(seq(-3, 3, by = 1)),
                cex = 1.8,
                adj = 1
              ),
              title = 'log\u2082FC',
              cex = 1.5,
              padding.text = 8, 
              background = 'white'
            ),
            key.top = 2, 
            add.grid = TRUE,
            col.lwd = 1,
            style = 'Nature',
            width = 12,
            height = 15,
            col.colour = 'grey',
            row.colour = 'grey'
          )
        })
        
      } else {
        output$li_dotplot <- renderPlot(NULL)
      }
    }
  })
  
  # Home Image -------------------------
  
  output$home_img1 <- renderImage({
    list(src = "www/Picture1.png",
         width = 600, style = "display: block; margin-left: auto; margin-right: auto;")
  }, deleteFile = FALSE)
  
  output$home_img2 <- renderImage({
    list(src = "www/Picture2.png",
         width = 600, style = "display: block; margin-left: auto; margin-right: auto;")
  }, deleteFile = FALSE)
  
  # Clinical Dataset Selection and Plotting -------------------------
  
  # Update and Render Clinical Data Table
  observe({
    req(input$dataset)
    
    if (!is.null(input$circRNA_clinical) && input$circRNA_clinical != "") {
      clinical_query$data <- names_exp()[names_exp()$gene == input$circRNA_clinical,]
      
      # Define columns to show based on selected dataset
      if (input$dataset == "Arul et al.") {
        clinical_columns2show <- c("ENT", "gene", "index")
      } else if (input$dataset == "Canadian Prostate Cancer Genome") {
        clinical_columns2show <- c("ENT", "gene", "numE", "lengthE", "index", "id")
      } else if (input$dataset == "In-house Breast Cohort") {
        clinical_columns2show <- c("ENT", "gene", "numE", "lengthE", "index", "id")
      }
      
      # Check if 'clinical_query$data' is valid and contains necessary columns
      if (!is.null(clinical_query$data) && nrow(clinical_query$data) > 0 && all(clinical_columns2show %in% colnames(clinical_query$data))) {
        output$express_table <- DT::renderDataTable({
          DT::datatable(clinical_query$data[clinical_columns2show],
                        selection = 'single',
                        options = list(dom = 't', scrollX = TRUE),
                        rownames = FALSE)
        })
      } else {
        output$express_table <- DT::renderDataTable(NULL)
      }
    } else {
      # Clear the table if 'circRNA_clinical' is not selected
      output$express_table <- DT::renderDataTable(NULL)
    }
  })
  
  # Clinical plot
  observe({
    req(input$dataset)
    
    if (!is.null(input$circRNA_clinical) && input$circRNA_clinical != "" &&
        !is.null(input$express_table_rows_selected)) {
      
      clinical_row <- clinical_query$data[input$express_table_rows_selected,]
      
      if (!is.null(clinical_row) && nrow(clinical_row) > 0) {
        clinical_selected_circID <- clinical_row[["circID"]]
        clinical_to_plot <- ClinicalDataset()[ClinicalDataset()$circID == clinical_selected_circID, ]
        
        # Initialize plot_width with a default value
        plot_width <- 600  # Default width in pixels
        
        # Calculate plot_width based on the selected dataset
        if (input$dataset == "Arul et al.") {
          
          # Determine the number of cancer types
          num_categories <- length(unique(clinical_to_plot$cancerType))
          
          # Calculate plot width
          base_width <- 400
          width_per_category <- 100
          calculated_width <- base_width + (num_categories * width_per_category)
          plot_width <- max(min(calculated_width, 1500), 600)
          
        } else if (input$dataset == "Canadian Prostate Cancer Genome") {
          
          # Fixed plot width for single category
          plot_width <- 600
          
        } else if (input$dataset == "In-house Breast Cohort") {
          
          # Determine the number of sample types
          num_categories <- length(unique(clinical_to_plot$Sample_Type))
          
          # Calculate plot width
          base_width <- 400
          width_per_category <- 100
          calculated_width <- base_width + (num_categories * width_per_category)
          plot_width <- max(min(calculated_width, 1500), 600)
          
        }
        
        # Now, render the plot with the calculated plot_width
        output$clinical_plot <- renderPlot({
          
          if (input$dataset == "Arul et al.") {
            
            plot <- ggplot(clinical_to_plot, aes(x = cancerType, y = as.numeric(as.character(circRNA_ncpm)), fill = cancerType)) +
              geom_boxplot() +
              geom_point(shape=16, size = 2, alpha = 0.6) +
              scale_y_log10(name="Normalized circRNA counts") + 
              labs(title="CircRNA normalized count per million of reads across cancer types",
                   x = "Cancer type",
                   y =  expression(log[10]("Normalized circRNA counts")),
                   fill = "Cancer type") +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 60, hjust=1),
                    legend.title = element_text(size = 14),
                    legend.text = element_text(size = 12),
                    plot.title = element_text(size = 16),
                    axis.title.y = element_text(size = 14),
                    axis.text = element_text(size = 12),
                    axis.title.x = element_text(size = 14),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())
            
            print(plot)
            
          } else if (input$dataset == "Canadian Prostate Cancer Genome") {
            
            # Reshape the data for plotting
            circRNA_data_long <- clinical_to_plot %>%
              select(starts_with("CPCG")) %>%
              pivot_longer(cols = everything(), names_to = "Patient", values_to = "Expression")
            
            # Use x = "" to create a single boxplot
            plot <- ggplot(circRNA_data_long, aes(x = "", y = Expression)) +
              geom_boxplot(outlier.shape = NA, fill = "lightpink") +
              geom_point(shape=16, size = 2, alpha = 0.6) +
              labs(title = "Normalized circRNA Expression Across Prostate Cancer Patients",
                   x = "Patients", y = "Normalized circRNA counts") +
              theme_bw() +
              theme(axis.text.x = element_text(hjust=0.5),
                    legend.title = element_text(size = 14),
                    legend.text = element_text(size = 12),
                    plot.title = element_text(size = 16),
                    axis.title.y = element_text(size = 14),
                    axis.text = element_text(size = 12),
                    axis.title.x = element_text(size = 14),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank())
            
            print(plot)
            
          } else if (input$dataset == "In-house Breast Cohort") {
            
            clinical_to_plot$Sample_Type <- factor(clinical_to_plot$Sample_Type, 
                                                   levels = c("Normal", "Her2 positive", "Luminal A", "Luminal B", "Triple negative"))
            
            plot <- ggplot(clinical_to_plot, aes(x = Sample_Type, y = as.numeric(as.character(circRNA_rpkm)), fill = Sample_Type)) +
              geom_boxplot() +
              geom_point(shape=16, size = 2, alpha = 0.6) +
              labs(title="Normalized circRNA Expression Across Breast Cancer Patients",
                   x = "Breast Cancer Subtype",
                   y = "Normalized circRNA counts",
                   fill = "Breast Cancer Subtype") +
              theme_bw() +
              theme(axis.text.x = element_text(hjust=0.5),
                    legend.title = element_text(size = 14),
                    legend.text = element_text(size = 12),
                    plot.title = element_text(size = 16),
                    axis.title.y = element_text(size = 14),
                    axis.text = element_text(size = 12),
                    axis.title.x = element_text(size = 14),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank())
            
            print(plot)
            
          }
          
        }, width = plot_width, height = 600)  # Use the calculated plot_width here
        
      } else {
        # Clear the plot if no valid data
        output$clinical_plot <- renderPlot(NULL)
      }
    } else {
      # Clear the plot if inputs are missing
      output$clinical_plot <- renderPlot(NULL)
    }
  })
  
} # End of server 

# Run the Shiny App
shinyApp(ui = ui, server = server)
