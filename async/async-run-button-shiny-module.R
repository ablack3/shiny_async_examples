library(shiny)
library(future)
library(promises)

cssFile <- tempfile(fileext = "css")
writeLines(con = cssFile, text = "
    .glyphicon.spinning {
        animation: spin 1s infinite linear;
        -webkit-animation: spin2 1s infinite linear;
    }
    
    @keyframes spin {
        from { transform: scale(1) rotate(0deg); }
        to { transform: scale(1) rotate(360deg); }
    }
    
    @-webkit-keyframes spin2 {
        from { -webkit-transform: rotate(0deg); }
        to { -webkit-transform: rotate(360deg); }
    }
")

executeButton <- function(id, label, icon = shiny::icon("play-circle", lib = "glyphicon"), ...) {
  if(!grepl("glyphicon", icon$attribs$class)) stop("icon must be a glyphicon.")
  tagList(
    shinyjs::useShinyjs(),
    includeCSS(cssFile),
    actionButton(NS(id, "button"), label = label, icon = icon, ...))
}

executeButtonServer <- function(id, executeFunction, result = NULL, ...) {
  moduleServer(id, function(input, output, session) {
    
    selector <- paste0("#", NS(id, "button"), " i")
    
    observeEvent(input$button, {
      shinyjs::disable("button")
      shinyjs::addClass(class = "glyphicon-refresh spinning", selector = selector)
      future(executeFunction(...), seed = T) %...>% 
        {if(!is.null(result)) result(.)} %...T>% 
        {
          shinyjs::removeClass(class = "glyphicon-refresh spinning", selector = selector)
          # shinyjs::addClass(class = "glyphicon-play-circle", selector = selector) 
          shinyjs::enable("button")
        }  
      
      return(NULL)
    })
    }
  )
}

################################################ # # # # # # # # # # # # # # # # # # # # #
# Usage of module in a shiny app (app.R file) # # # # # # # # # # # # # # # # # # # # # #
################################################ # # # # # # # # # # # # # # # # # # # #

# Global setup
plan(multisession, workers = 2) # evaluate futures in separate R sessions
# Sys.setenv(R_FUTURE_PLAN="multisession") # or set the environment variable

# define the analysis/query outside of any reactive context so it can be tested with testthat
# Usually in a separate file
run_analysis <- function(database){
  print(paste("executing analysis on", database))
  Sys.sleep(4) # pretend this operation takes a long time
  results <- tibble::tibble(covariate = paste0("Age_", seq(50, 80, 10), "_", seq(60, 90, 10)), odds_ratio = round(exp((1:4)/20 + rnorm(4, 0, .1)), 5))
  print(paste("execution on", database, "complete"))
  print(results)
  return(NULL) # function called for its side effect
}


ui <- fluidPage(
  titlePanel("Run an analysis on multiple databases"),
  executeButton("run1", "Execute on database 1"), tags$br(),
  executeButton("run2", "Execute on database 2"), tags$br(),
  executeButton("run3", "Execute on database 3"), tags$br()
)

server <- function(input, output, session) {
  executeButtonServer("run1", executeFunction = run_analysis, database = "database1")
  executeButtonServer("run2", executeFunction = run_analysis, database = "database2")
  executeButtonServer("run3", executeFunction = run_analysis, database = "database3")
}

shinyApp(ui = ui, server = server)