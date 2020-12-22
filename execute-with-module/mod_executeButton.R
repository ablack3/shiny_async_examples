require(shiny)
require(promises) # for %...>% 

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

executeButtonServer <- function(id, f) {
  moduleServer(id, function(input, output, session) {
    lhs <- rlang::f_lhs(f)
    env <- rlang::f_env(f)
    # print(ls(envir = rlang::f_env(f)))
    # print(class(eval(lhs, envir = env)))
    # print(lhs)
    if(!(is.null(lhs) || "function" %in% class(eval(lhs, env)))) {
      rlang::abort("The left hand side of the formula must be blank or a function",
                   class(eval(lhs, env)))
    }
    
    func <- rlang::as_function(rlang::new_formula(NULL, rlang::f_rhs(f), env))
    selector <- paste0("#", NS(id, "button"), " i")
    
    observeEvent(input$button, {
      shinyjs::disable("button")
      shinyjs::addClass(class = "glyphicon-refresh spinning", selector = selector)
      future::future(func(), seed = T) %...>% 
        # If the left hand side of the formula was supplied, 
        # pass the result of the RHS to the LHS
        {if(!is.null(lhs)) eval(lhs, env)(.)} %...T>% 
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
