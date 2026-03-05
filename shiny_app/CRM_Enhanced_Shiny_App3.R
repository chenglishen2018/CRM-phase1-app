library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(gridExtra)
library(scales)

# Generate CRM skeleton priors
generate_crm_skeleton <- function(min_prob, max_prob, n_doses) {
  skeleton <- seq(min_prob, max_prob, length.out = n_doses)
  return(skeleton)
}

# Simplified CRM analysis
crm_simplified <- function(skeleton, target_dlt, toxicity, level) {
  n_doses <- length(skeleton)
  
  # Calculate posterior using simple Bayesian update
  posterior <- numeric(n_doses)
  for (i in 1:n_doses) {
    dose_patients <- sum(level == i)
    dose_dlts <- sum(toxicity[level == i])
    if (dose_patients > 0) {
      posterior[i] <- (dose_dlts + 0.5) / (dose_patients + 1)
    } else {
      posterior[i] <- skeleton[i]
    }
  }
  
  # Bound posteriors
  posterior <- pmax(pmin(posterior, 0.999), 0.001)
  
  # Find MTD (closest to target)
  mtd <- which.min(abs(posterior - target_dlt))
  
  return(list(
    mtd = mtd,
    ptox = posterior,
    n_patients = length(toxicity),
    n_dlts = sum(toxicity)
  ))
}

# Run CRM analysis
run_crm_analysis <- function(skeleton, target_dlt, toxicity, level) {
  if (length(toxicity) == 0 | length(level) == 0) {
    return(list(mtd = NA, ptox = skeleton, error = "No data"))
  }
  
  toxicity <- as.numeric(toxicity)
  level <- as.numeric(level)
  
  tryCatch({
    if (requireNamespace("dfcrm", quietly = TRUE)) {
      result <- dfcrm::crm(prior = skeleton, target = target_dlt, tox = toxicity, level = level)
      return(list(
        mtd = result$mtd,
        ptox = result$ptox,
        n_patients = length(toxicity),
        n_dlts = sum(toxicity)
      ))
    } else {
      return(crm_simplified(skeleton, target_dlt, toxicity, level))
    }
  }, error = function(e) {
    return(crm_simplified(skeleton, target_dlt, toxicity, level))
  })
}

# Simulate CRM trials with progress tracking
simulate_crm_trial <- function(skeleton, target_dlt, n_doses, n_sims = 100, max_patients = 40, seed = 123, progress_callback = NULL) {
  set.seed(seed)
  
  sim_results <- data.frame(
    simulation = integer(),
    final_mtd = integer(),
    n_patients = integer(),
    n_dlts = integer(),
    stringsAsFactors = FALSE
  )
  
  true_prob <- seq(0.01, 0.90, length.out = n_doses)
  correct_mtd <- which.min(abs(true_prob - target_dlt))
  
  for (sim in 1:n_sims) {
    # Call progress callback if provided
    if (!is.null(progress_callback)) {
      progress_callback(sim, n_sims)
    }
    
    toxicity <- integer()
    level <- integer()
    current_dose <- 1
    n_patients_sim <- 0
    cohort_size <- 3
    
    while ((n_patients_sim + cohort_size) <= max_patients && current_dose > 0 && current_dose <= n_doses) {
      for (p in 1:cohort_size) {
        dlt <- rbinom(1, 1, true_prob[current_dose])
        toxicity <- c(toxicity, dlt)
        level <- c(level, current_dose)
        n_patients_sim <- n_patients_sim + 1
      }
      
      result <- run_crm_analysis(skeleton, target_dlt, toxicity, level)
      if (is.na(result$mtd)) break
      
      current_dose <- result$mtd
    }
    
    sim_results <- rbind(sim_results, data.frame(
      simulation = sim,
      final_mtd = current_dose,
      n_patients = n_patients_sim,
      n_dlts = sum(toxicity),
      stringsAsFactors = FALSE
    ))
  }
  
  mtd_accuracy <- sum(sim_results$final_mtd == correct_mtd) / n_sims
  
  return(list(
    results = sim_results,
    accuracy = mtd_accuracy,
    mean_patients = mean(sim_results$n_patients),
    mean_dlts = mean(sim_results$n_dlts),
    correct_mtd = correct_mtd
  ))
}

# ============================================================================
# UI
# ============================================================================

ui <- dashboardPage(
  dashboardHeader(title = "CRM Enhanced - Phase I Dose-Escalation"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("CRM Analysis", tabName = "analysis", icon = icon("flask")),
      menuItem("CRM Simulation", tabName = "simulation", icon = icon("chart-bar")),
      menuItem("Cohort History", tabName = "history", icon = icon("list")),
      menuItem("Trial Summary", tabName = "summary", icon = icon("file")),
      menuItem("Help", tabName = "help", icon = icon("question"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # TAB 1: CRM ANALYSIS
      tabItem(tabName = "analysis",
        h2("CRM Analysis"),
        
        fluidRow(
          box(title = "Step 1: Generate or Enter Skeleton",
              status = "primary", solidHeader = TRUE, width = 6,
              
              h5("Option A: Auto-Generate"),
              numericInput("min_prob", "Min Probability:", 0.05, 0.01, 0.5, 0.01),
              numericInput("max_prob", "Max Probability:", 0.40, 0.01, 0.9, 0.01),
              numericInput("n_doses", "Number of Doses:", 5, 3, 10),
              actionButton("gen_skeleton", "Generate Skeleton", class = "btn-success"),
              
              hr(),
              
              h5("Option B: Enter Manually"),
              p(em("Comma-separated, e.g: 0.05,0.12,0.25,0.40,0.55"),
                style = "font-size: 11px; color: #666;"),
              textAreaInput("manual_skeleton", label = NULL, value = "", rows = 2, placeholder = "Enter skeleton priors"),
              actionButton("set_manual_skeleton", "Use Manual Skeleton", class = "btn-info")
          ),
          
          box(title = "Current Skeleton & Target",
              status = "info", solidHeader = TRUE, width = 6,
              tableOutput("skeleton_table"),
              br(),
              numericInput("target_dlt", "Target DLT Rate:", 0.25, 0.1, 0.5, 0.05)
          )
        ),
        
        fluidRow(
          box(title = "Step 2: Escalation Strategy",
              status = "primary", solidHeader = TRUE, width = 6,
              
              selectInput("escalation_strategy",
                         "Choose Strategy:",
                         choices = c(
                           "Sequential (Cap +1, NO SKIP)" = "sequential",
                           "Unrestricted (Skip allowed)" = "unrestricted",
                           "Conservative (±1 only)" = "conservative"
                         ),
                         selected = "sequential"),
              
              uiOutput("strategy_explanation")
          ),
          
          box(title = "Step 3: Enter Patient Data",
              status = "success", solidHeader = TRUE, width = 6,
              
              p(strong("Doses (comma-separated):")),
              p(em("Example: 1,1,1,2,2,2,3,3,3"), style = "font-size: 11px; color: #666;"),
              textAreaInput("doses_input", label = NULL, value = "1,1,1,2,2,2,2,3,3", rows = 2),
              
              p(strong("DLT Outcomes (0=no, 1=yes):")),
              textAreaInput("outcomes_input", label = NULL, value = "0,0,0,0,0,1,0,1,1", rows = 2),
              
              actionButton("run_crm", "Run CRM Analysis", class = "btn-lg btn-success")
          )
        ),
        
        fluidRow(
          box(title = "Recommendation",
              status = "warning", solidHeader = TRUE, width = 6,
              
              h3(textOutput("recommendation_text"), style = "color: #d9534f; text-align: center; margin: 15px 0;"),
              
              p(strong("Raw CRM Recommendation:"), textOutput("raw_recommendation")),
              p(strong("After Strategy Adjustment:"), textOutput("adjusted_recommendation")),
              
              verbatimTextOutput("strategy_note")
          ),
          
          box(title = "Posterior Probabilities",
              status = "info", solidHeader = TRUE, width = 6,
              tableOutput("posterior_table")
          )
        ),
        
        fluidRow(
          box(title = "Posterior Probability Plot",
              status = "info", solidHeader = TRUE, width = 12,
              plotOutput("posterior_plot", height = 400)
          )
        )
      ),
      
      # TAB 2: CRM SIMULATION
      tabItem(tabName = "simulation",
        h2("CRM Simulation - Design Planning"),
        
        fluidRow(
          box(title = "Simulation Parameters",
              status = "primary", solidHeader = TRUE, width = 6,
              numericInput("sim_target", "Target DLT:", 0.25, 0.1, 0.5, 0.05),
              numericInput("sim_n_doses", "Number of Doses:", 5, 3, 10),
              numericInput("sim_n_sims", "Number of Simulations:", 100, 10, 1000, 50),
              numericInput("sim_max_pts", "Max Patients per Trial:", 40, 15, 100, 5),
              actionButton("run_sim", "Run Simulations", class = "btn-lg btn-info")
          ),
          
          box(title = "Operating Characteristics",
              status = "success", solidHeader = TRUE, width = 6,
              verbatimTextOutput("sim_summary")
          )
        ),
        
        fluidRow(
          box(title = "MTD Distribution",
              status = "info", solidHeader = TRUE, width = 6,
              plotOutput("mtd_dist_plot", height = 350)
          ),
          
          box(title = "Enrollment Distribution",
              status = "info", solidHeader = TRUE, width = 6,
              plotOutput("enrollment_dist_plot", height = 350)
          )
        )
      ),
      
      # TAB 3: COHORT HISTORY
      tabItem(tabName = "history",
        h2("Cohort Analysis History"),
        
        fluidRow(
          box(title = "All Analyses",
              status = "primary", solidHeader = TRUE, width = 12,
              p(strong("Track your trial progress automatically")),
              p(em("GREEN = Convergence detected, you can STOP the trial"),
                style = "color: green; font-weight: bold;"),
              DTOutput("history_table"),
              br(),
              uiOutput("convergence_alert"),
              br(),
              downloadButton("download_history", "Download History as CSV")
          )
        )
      ),
      
      # TAB 4: TRIAL SUMMARY
      tabItem(tabName = "summary",
        h2("Trial Summary Report"),
        
        fluidRow(
          box(title = "Generate Summary",
              status = "success", solidHeader = TRUE, width = 12,
              actionButton("gen_summary", "Generate Trial Summary Report", class = "btn-lg btn-success"),
              br(), br(),
              verbatimTextOutput("summary_report"),
              br(),
              downloadButton("download_summary", "Download Report as Text File")
          )
        )
      ),
      
      # TAB 5: HELP
      tabItem(tabName = "help",
        h2("Help & Documentation"),
        
        box(title = "How to Use This App",
            status = "info", solidHeader = TRUE, width = 12,
            h4("Quick Start:"),
            tags$ol(
              tags$li("Go to CRM Analysis tab"),
              tags$li("Enter skeleton parameters and click Generate Skeleton"),
              tags$li("Paste your patient doses and DLT outcomes"),
              tags$li("Click Run CRM Analysis"),
              tags$li("View recommended next dose"),
              tags$li("Repeat for each cohort"),
              tags$li("When Cohort History shows same dose twice = CONVERGENCE = STOP trial"),
              tags$li("Go to Trial Summary tab and generate final report")
            ),
            
            h4("Key Concepts:"),
            p(strong("MTD:"), "Maximum Tolerated Dose - the dose closest to your target DLT rate"),
            p(strong("Convergence:"), "When CRM recommends the SAME dose in two consecutive analyses - this means your trial is complete"),
            p(strong("Posterior:"), "Updated belief about DLT probability at each dose based on observed data"),
            
            h4("Data Format:"),
            p(strong("Doses:"), "1,1,1,2,2,2,3,3,3 (comma-separated, 3 patients at dose 1, 3 at dose 2, etc.)"),
            p(strong("Outcomes:"), "0,0,0,0,0,1,1,0,0 (0=no DLT, 1=DLT, must match doses length)")
        )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  skeleton_val <- reactiveVal(NULL)
  analysis_result <- reactiveVal(NULL)
  sim_result <- reactiveVal(NULL)
  raw_recommendation <- reactiveVal(NULL)
  
  cohort_history <- reactiveVal(data.frame(
    cohort = integer(),
    date = character(),
    n_patients = integer(),
    n_dlts = integer(),
    recommended = integer(),
    posterior = numeric(),
    stringsAsFactors = FALSE
  ))
  
  # Apply escalation strategy constraint
  apply_escalation_strategy <- function(crm_mtd, current_dose, strategy) {
    if (strategy == "sequential") {
      # Cap escalation at +1, allow unlimited de-escalation
      if (crm_mtd > current_dose + 1) {
        return(current_dose + 1)
      } else {
        return(crm_mtd)
      }
    } else if (strategy == "unrestricted") {
      # No restrictions
      return(crm_mtd)
    } else if (strategy == "conservative") {
      # Can only move by 1 in any direction
      if (crm_mtd > current_dose + 1) {
        return(current_dose + 1)
      } else if (crm_mtd < current_dose - 1) {
        return(current_dose - 1)
      } else {
        return(crm_mtd)
      }
    }
  }
  
  # Generate skeleton
  observeEvent(input$gen_skeleton, {
    skeleton <- generate_crm_skeleton(input$min_prob, input$max_prob, input$n_doses)
    skeleton_val(skeleton)
    showNotification("Skeleton generated!", type = "message")
  })
  
  # Set manual skeleton
  observeEvent(input$set_manual_skeleton, {
    if (input$manual_skeleton == "") {
      showNotification("Enter skeleton values!", type = "error")
      return()
    }
    
    skeleton <- as.numeric(strsplit(gsub(" ", "", input$manual_skeleton), ",")[[1]])
    
    if (any(is.na(skeleton)) || length(skeleton) < 2) {
      showNotification("Invalid skeleton format!", type = "error")
      return()
    }
    
    skeleton_val(skeleton)
    showNotification(paste("Manual skeleton set with", length(skeleton), "doses!"), type = "message")
  })
  
  # Strategy explanation
  output$strategy_explanation <- renderUI({
    strategy <- input$escalation_strategy
    
    if (strategy == "sequential") {
      HTML("<div style='background-color: #d4edda; padding: 10px; border-radius: 5px;'>
            <strong style='color: #155724;'>Sequential (RECOMMENDED)</strong><br/>
            <em style='font-size: 11px; color: #155724;'>Never escalate more than +1 dose<br/>Allows de-escalation<br/>Standard practice</em>
            </div>")
    } else if (strategy == "unrestricted") {
      HTML("<div style='background-color: #fff3cd; padding: 10px; border-radius: 5px;'>
            <strong style='color: #856404;'>Unrestricted</strong><br/>
            <em style='font-size: 11px; color: #856404;'>Can skip dose levels<br/>Needs justification<br/>Higher risk</em>
            </div>")
    } else {
      HTML("<div style='background-color: #cce5ff; padding: 10px; border-radius: 5px;'>
            <strong style='color: #004085;'>Conservative</strong><br/>
            <em style='font-size: 11px; color: #004085;'>Restrict ±1 in any direction<br/>Maximum safety<br/>Slower escalation</em>
            </div>")
    }
  })
  
  # Display skeleton
  output$skeleton_table <- renderTable({
    if (is.null(skeleton_val())) return(NULL)
    data.frame(
      Dose = 1:length(skeleton_val()),
      Prior_Probability = round(skeleton_val(), 4)
    )
  })
  
  # Run CRM analysis
  observeEvent(input$run_crm, {
    if (is.null(skeleton_val())) {
      showNotification("Generate or enter skeleton first!", type = "error")
      return()
    }
    
    doses <- as.numeric(strsplit(gsub(" ", "", input$doses_input), ",")[[1]])
    outcomes <- as.numeric(strsplit(gsub(" ", "", input$outcomes_input), ",")[[1]])
    
    if (length(doses) != length(outcomes)) {
      showNotification("Doses and outcomes must have same length!", type = "error")
      return()
    }
    
    result <- run_crm_analysis(skeleton_val(), input$target_dlt, outcomes, doses)
    raw_recommendation(result$mtd)
    analysis_result(result)
    
    # Update cohort history
    history <- cohort_history()
    new_row <- data.frame(
      cohort = nrow(history) + 1,
      date = format(Sys.Date(), "%Y-%m-%d"),
      n_patients = result$n_patients,
      n_dlts = result$n_dlts,
      recommended = result$mtd,
      posterior = round(result$ptox[result$mtd], 4),
      stringsAsFactors = FALSE
    )
    cohort_history(rbind(history, new_row))
    
    showNotification("CRM analysis complete!", type = "message")
  })
  
  output$recommendation_text <- renderText({
    if (is.null(analysis_result())) return("Run CRM Analysis to see recommendation")
    
    strategy <- input$escalation_strategy
    raw_mtd <- analysis_result()$mtd
    current_dose <- 1  # Assuming starting at dose 1
    adjusted_mtd <- apply_escalation_strategy(raw_mtd, current_dose, strategy)
    
    paste("RECOMMENDED NEXT DOSE:", adjusted_mtd)
  })
  
  output$raw_recommendation <- renderText({
    if (is.null(analysis_result())) return("N/A")
    paste("Dose", analysis_result()$mtd)
  })
  
  output$adjusted_recommendation <- renderText({
    if (is.null(analysis_result())) return("N/A")
    
    strategy <- input$escalation_strategy
    raw_mtd <- analysis_result()$mtd
    current_dose <- 1
    adjusted_mtd <- apply_escalation_strategy(raw_mtd, current_dose, strategy)
    
    if (adjusted_mtd != raw_mtd) {
      paste("Dose", adjusted_mtd, "(capped from", raw_mtd, ")")
    } else {
      paste("Dose", adjusted_mtd, "(no adjustment)")
    }
  })
  
  output$strategy_note <- renderPrint({
    if (is.null(analysis_result())) {
      cat("Escalation strategy will be applied after analysis\n")
      return()
    }
    
    strategy <- input$escalation_strategy
    raw_mtd <- analysis_result()$mtd
    current_dose <- 1
    adjusted_mtd <- apply_escalation_strategy(raw_mtd, current_dose, strategy)
    
    cat("Strategy Applied:\n")
    cat("────────────────────────────────\n")
    cat(sprintf("CRM Recommendation: Dose %d\n", raw_mtd))
    cat(sprintf("Strategy: %s\n", 
               switch(strategy,
                     "sequential" = "Sequential (Cap +1)",
                     "unrestricted" = "Unrestricted",
                     "conservative" = "Conservative (±1)")))
    cat(sprintf("Adjusted: Dose %d\n", adjusted_mtd))
    
    if (adjusted_mtd != raw_mtd) {
      cat(sprintf("\nNote: Dose %d capped at Dose %d per %s strategy\n",
                 raw_mtd, adjusted_mtd, strategy))
    }
  })
  
  # Run CRM analysis
  observeEvent(input$run_crm, {
    if (is.null(skeleton_val())) {
      showNotification("Generate or enter skeleton first!", type = "error")
      return()
    }
    
    doses <- as.numeric(strsplit(gsub(" ", "", input$doses_input), ",")[[1]])
    outcomes <- as.numeric(strsplit(gsub(" ", "", input$outcomes_input), ",")[[1]])
    
    if (length(doses) != length(outcomes)) {
      showNotification("Doses and outcomes must have same length!", type = "error")
      return()
    }
    
    result <- run_crm_analysis(skeleton_val(), input$target_dlt, outcomes, doses)
    analysis_result(result)
    
    # Update cohort history
    history <- cohort_history()
    new_row <- data.frame(
      cohort = nrow(history) + 1,
      date = format(Sys.Date(), "%Y-%m-%d"),
      n_patients = result$n_patients,
      n_dlts = result$n_dlts,
      recommended = result$mtd,
      posterior = round(result$ptox[result$mtd], 4),
      stringsAsFactors = FALSE
    )
    cohort_history(rbind(history, new_row))
    
    showNotification("CRM analysis complete!", type = "message")
  })
  
  output$recommendation_text <- renderText({
    if (is.null(analysis_result())) return("Run CRM Analysis to see recommendation")
    paste("RECOMMENDED NEXT DOSE:", analysis_result()$mtd)
  })
  
  output$posterior_table <- renderTable({
    if (is.null(analysis_result())) return(NULL)
    data.frame(
      Dose = 1:length(analysis_result()$ptox),
      Posterior = round(analysis_result()$ptox, 4),
      Distance_to_Target = round(abs(analysis_result()$ptox - input$target_dlt), 4),
      Recommended = ifelse(1:length(analysis_result()$ptox) == analysis_result()$mtd, "YES", "")
    )
  })
  
  output$posterior_plot <- renderPlot({
    if (is.null(analysis_result())) return(NULL)
    
    df <- data.frame(
      Dose = factor(1:length(analysis_result()$ptox)),
      Probability = analysis_result()$ptox,
      IsRecommended = 1:length(analysis_result()$ptox) == analysis_result()$mtd
    )
    
    ggplot(df, aes(x = Dose, y = Probability, fill = IsRecommended)) +
      geom_col(alpha = 0.8) +
      geom_hline(yintercept = input$target_dlt, linetype = "dashed", color = "red", size = 1) +
      scale_fill_manual(values = c("FALSE" = "skyblue", "TRUE" = "lightgreen")) +
      labs(title = "Posterior DLT Probabilities", x = "Dose", y = "Probability", fill = "Recommended") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Run simulations
  # Run simulations with progress bar
  observeEvent(input$run_sim, {
    withProgress(message = 'Running Simulations', value = 0, {
      skeleton <- generate_crm_skeleton(input$min_prob, input$max_prob, input$sim_n_doses)
      
      n_sims <- input$sim_n_sims
      
      # Create progress callback function
      progress_update <- function(current, total) {
        incProgress(1/total, detail = paste("Simulation", current, "of", total))
      }
      
      result <- simulate_crm_trial(skeleton, input$sim_target, input$sim_n_doses, 
                                   n_sims, input$sim_max_pts,
                                   progress_callback = progress_update)
      sim_result(result)
    })
    showNotification("Simulations complete!", type = "message")
  })
  
  output$sim_summary <- renderPrint({
    if (is.null(sim_result())) {
      cat("Run simulations to see results\n")
      return()
    }
    
    result <- sim_result()
    cat("OPERATING CHARACTERISTICS:\n")
    cat("========================\n")
    cat(sprintf("Number of Simulations: %d\n", nrow(result$results)))
    cat(sprintf("MTD Selection Accuracy: %.1f%%\n", result$accuracy * 100))
    cat(sprintf("Mean Patients per Trial: %.1f\n", result$mean_patients))
    cat(sprintf("Mean DLTs per Trial: %.1f\n", result$mean_dlts))
    cat(sprintf("Correct MTD: Dose %d\n", result$correct_mtd))
  })
  
  output$mtd_dist_plot <- renderPlot({
    if (is.null(sim_result())) return(NULL)
    
    df <- sim_result()$results
    
    # Count frequency for each dose
    freq_data <- as.data.frame(table(df$final_mtd))
    colnames(freq_data) <- c("dose", "count")
    freq_data$dose <- as.numeric(as.character(freq_data$dose))
    
    ggplot(freq_data, aes(x = factor(dose), y = count)) +
      geom_col(fill = "steelblue", alpha = 0.8) +
      geom_text(aes(label = count), vjust = -0.5, size = 5, fontface = "bold") +
      labs(title = "Final MTD Distribution from Simulations", 
           x = "Dose", 
           y = "Frequency (Number of Trials)",
           subtitle = paste("Total Simulations:", nrow(df))) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 14),
            axis.text = element_text(size = 12))
  })
  
  output$enrollment_dist_plot <- renderPlot({
    if (is.null(sim_result())) return(NULL)
    
    df <- sim_result()$results
    ggplot(df, aes(x = n_patients)) +
      geom_histogram(fill = "darkgreen", alpha = 0.8, bins = 15) +
      geom_vline(aes(xintercept = mean(n_patients)), linetype = "dashed", color = "red", size = 1) +
      labs(title = "Patient Enrollment Distribution", x = "Number of Patients", y = "Frequency") +
      theme_minimal()
  })
  
  # Cohort history
  output$history_table <- renderDT({
    history <- cohort_history()
    if (nrow(history) == 0) {
      return(datatable(data.frame(Message = "No analyses yet")))
    }
    datatable(history, options = list(pageLength = 20))
  })
  
  output$convergence_alert <- renderUI({
    history <- cohort_history()
    if (nrow(history) < 2) {
      return(HTML("<p style='color: #666;'>Need 2+ analyses to detect convergence...</p>"))
    }
    
    prev <- history$recommended[nrow(history) - 1]
    curr <- history$recommended[nrow(history)]
    
    if (prev == curr) {
      return(HTML(sprintf(
        "<div style='background-color: #d4edda; padding: 15px; border: 2px solid #28a745; border-radius: 5px;'>
          <h4 style='color: #155724;'>✓ CONVERGENCE ACHIEVED!</h4>
          <p style='color: #155724;'>Dose %d recommended twice. You can STOP the trial!</p>
        </div>", curr
      )))
    } else {
      return(HTML(sprintf(
        "<div style='background-color: #fff3cd; padding: 15px; border: 2px solid #ffc107; border-radius: 5px;'>
          <p style='color: #856404;'>No convergence yet. Last: Dose %d, Current: Dose %d</p>
        </div>", prev, curr
      )))
    }
  })
  
  output$download_history <- downloadHandler(
    filename = function() paste("cohort_history_", Sys.Date(), ".csv", sep=""),
    content = function(file) write.csv(cohort_history(), file, row.names = FALSE)
  )
  
  # Trial summary
  observeEvent(input$gen_summary, {
    if (nrow(cohort_history()) == 0) {
      showNotification("No trial data yet!", type = "error")
      return()
    }
  })
  
  output$summary_report <- renderPrint({
    history <- cohort_history()
    if (nrow(history) == 0) {
      cat("No trial data available\n")
      return()
    }
    
    total_patients <- max(history$n_patients)
    total_dlts <- max(history$n_dlts)
    final_mtd <- history$recommended[nrow(history)]
    
    is_converged <- nrow(history) >= 2 && history$recommended[nrow(history)] == history$recommended[nrow(history)-1]
    
    cat("╔════════════════════════════════════════╗\n")
    cat("║    PHASE I TRIAL SUMMARY REPORT        ║\n")
    cat("╚════════════════════════════════════════╝\n\n")
    
    cat("ENROLLMENT SUMMARY:\n")
    cat("────────────────────────────────────────\n")
    cat(sprintf("Total Patients:  %d\n", total_patients))
    cat(sprintf("Total DLTs:      %d\n", total_dlts))
    cat(sprintf("DLT Rate:        %.1f%%\n\n", 100 * total_dlts / total_patients))
    
    cat("FINAL MTD:\n")
    cat("────────────────────────────────────────\n")
    cat(sprintf("Recommended Dose: %d\n", final_mtd))
    cat(sprintf("Convergence:      %s\n\n", ifelse(is_converged, "YES ✓", "NO")))
    
    cat("COHORT DETAILS:\n")
    cat("────────────────────────────────────────\n")
    cat("Cohort | Patients | DLTs | MTD\n")
    for (i in 1:nrow(history)) {
      cat(sprintf("%6d | %8d | %4d | %3d\n", history$cohort[i], history$n_patients[i], 
                  history$n_dlts[i], history$recommended[i]))
    }
    
    cat("\nReport Generated: ", format(Sys.Date(), "%Y-%m-%d"), "\n")
  })
  
  output$download_summary <- downloadHandler(
    filename = function() paste("trial_summary_", Sys.Date(), ".txt", sep=""),
    content = function(file) {
      sink(file)
      
      history <- cohort_history()
      total_patients <- max(history$n_patients)
      total_dlts <- max(history$n_dlts)
      final_mtd <- history$recommended[nrow(history)]
      is_converged <- nrow(history) >= 2 && history$recommended[nrow(history)] == history$recommended[nrow(history)-1]
      
      cat("PHASE I TRIAL SUMMARY REPORT\n")
      cat("============================\n\n")
      cat(sprintf("Total Patients: %d\n", total_patients))
      cat(sprintf("Total DLTs: %d\n", total_dlts))
      cat(sprintf("Final MTD: Dose %d\n", final_mtd))
      cat(sprintf("Convergence: %s\n\n", ifelse(is_converged, "YES", "NO")))
      
      cat("Cohort Details:\n")
      for (i in 1:nrow(history)) {
        cat(sprintf("Cohort %d: %d patients, %d DLTs, Dose %d\n", 
                   history$cohort[i], history$n_patients[i], history$n_dlts[i], history$recommended[i]))
      }
      
      cat("\nReport Generated: ", format(Sys.Date(), "%Y-%m-%d"), "\n")
      
      sink()
    }
  )
}

# Run the app
shinyApp(ui, server)
