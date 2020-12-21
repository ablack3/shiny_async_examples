library(shiny)

ui <- fluidPage(
    shinyjs::useShinyjs(),
    includeCSS("styles.css"),
    actionButton("button", "Run", icon = icon("home", lib = "glyphicon"))
)

server <- function(input, output) {
    # shinyjs::addClass("button2", "load1")
    # shinyjs::removeClass(class = "glyphicon-home", selector = "#button i")
    shinyjs::addClass(class = "glyphicon-refresh spinning", selector = "#button i")
}

shinyApp(ui = ui, server = server)



# shinyjs::addClass()

# shinycssloaders::withSpinner("a")
