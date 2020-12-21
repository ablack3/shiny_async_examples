require(shiny)
require(future)
require(promises)

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
