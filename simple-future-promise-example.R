n <- 5

get_data <- function(){
  Sys.sleep(5)
  head(mtcars, 5)
}
Sys.setenv(R_FUTURE_PLAN="multisession")
library(future)
library(promises)
# plan(multisession)

future(get_data()) %...>% 
  print()

# this works