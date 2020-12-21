
library(shiny)
source("mod_executeButton.R")

# Global setup
plan(multisession, workers = 2) # evaluate futures in separate R sessions
# Sys.setenv(R_FUTURE_PLAN="multisession") # or set the environment variable

# define the analysis/query outside of any reactive context so it can be tested with testthat
# Usually in a separate file
run_analysis <- function(database){
    Sys.sleep(4) # pretend this operation takes a long time
    results <- tibble::tibble(database = database, 
                              covariate = paste0("Age_", seq(50, 80, 10), "_", seq(60, 90, 10)), 
                              odds_ratio = round(exp((1:4)/20 + rnorm(4, 0, .1)), 5))
    
    return(results)
}


ui <- fluidPage(
    titlePanel("Run an analysis on multiple databases"),
    executeButton("run1", "Execute on Database 1"), tags$br(),
    executeButton("run2", "Execute on Database 2"), tags$br(),
    executeButton("run3", "Execute on Database 3"), tags$br(),
    tags$br(),
    DT::dataTableOutput("table")
)

server <- function(input, output, session) {
    result <- reactiveVal()
    
    executeButtonServer("run1", result = result, executeFunction = run_analysis, database = "Database 1")
    executeButtonServer("run2", result = result, executeFunction = run_analysis, database = "Database 2")
    executeButtonServer("run3", result = result, executeFunction = run_analysis, database = "Database 3")
    
    output$table <- DT::renderDataTable(DT::datatable(result()))
}

shinyApp(ui = ui, server = server)