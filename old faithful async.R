# https://stackoverflow.com/questions/52864035/asynchronous-shiny-programming-setting-up-a-basic-example

# async does not fix blocking within session.
# https://github.com/rstudio/promises/issues/23
# https://stackoverflow.com/questions/50165443/async-process-blocking-r-shiny-app


library(future)
library(promises)
library(shiny)
plan(multisession, workers = 2)

ui <- fluidPage(
  titlePanel("Old Faithful Geyser Data"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("slow_bins",
                  "Number of slow bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      sliderInput('fast_bins',
                  'fast bins',
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    mainPanel(
      plotOutput("slow_dist_plot"),
      plotOutput("fast_dist_plot")
    )
  )
)

server <- function(input, output) {
  
  bins = reactive({
    future({
      print("I'm slow")
      Sys.sleep(10)
      faithful[, 2]
    }) %...>% 
      {seq(min(.), max(.), length.out = input$slow_bins + 1)}
  })
  
  output$slow_dist_plot <- renderPlot({
    bins() %...>%
      {hist(faithful[, 2], breaks = ., col = 'darkgray', border = 'white')}
    
  })
  
  output$fast_dist_plot = renderPlot({
    print("I'm fast")
    x    <-faithful[, 2] 
    bins = seq(min(x), max(x), length.out = input$fast_bins + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
}

shinyApp(ui = ui, server = server)

