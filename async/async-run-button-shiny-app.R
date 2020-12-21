# A minimal app that demonstrates an async computation
library(shiny)
library(future)
library(promises)
library(DT)

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

# evaluate futures in separate R sessions
plan(multisession, workers = 2)
# Sys.setenv(R_FUTURE_PLAN="multisession") # or set the environment variable

# define the analysis/query outside of any reactive context so it can be tested with testthat
run_analysis <- function(computation_time = 6){
    Sys.sleep(computation_time) # pretend this operation takes a long time
    results <- data.frame(covariate = paste0("Age_", seq(50, 80, 10), "_", seq(60, 90, 10)), odds_ratio = round(exp((1:4)/20 + rnorm(4, 0, .1)), 5))
    return(results)
}

# test async
# run_analysis() %>% print() # blocks R process
# future(run_analysis()) %...>% print() # does not block R

ui <- fluidPage(
    tags$head(tags$style(HTML(css))),
    shinyjs::useShinyjs(),
    titlePanel("Run some long computation asynchronously"),
    sliderInput("computation_time", "Set computation time in seconds", 0, 10, 2),
    fluidRow(actionButton("run", "Run Analysis", icon = icon("play-circle", lib = "glyphicon")), actionButton("popup", "Show popup")),
    wellPanel(tags$em("Log"), verbatimTextOutput("log")),
    dataTableOutput("table")
)

server <- function(input, output, session) {
    rv <- reactiveValues(text = NULL)
    results <- reactiveVal()
    
    observeEvent(input$popup, {
        showModal(modalDialog("A popup", easyClose = T))
    })
    
    observeEvent(input$run, {
        rv$text <- paste(rv$text, Sys.time(), " - Starting analysis - ", input$run, "\n")
        seconds <- input$computation_time
        shinyjs::disable("run")
        shinyjs::addClass(class = "glyphicon-refresh spinning", selector = "#run i")
        future(run_analysis(seconds), seed = T) %...>% 
            results() %...T>% 
            {
                shinyjs::removeClass(class = "glyphicon-refresh spinning", selector = "#run i")
                shinyjs::addClass(class = "glyphicon-play-circle", selector = "#run i") 
                shinyjs::enable("run")
            }  
        
        return(NULL)
    })
    
    output$log <- renderPrint(cat(rv$text))
    output$table <- renderDataTable({
        results() %>% 
            DT::datatable()
    })
}

shinyApp(ui = ui, server = server)
