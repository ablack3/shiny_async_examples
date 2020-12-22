
#' Execute a function and possibly save the result as a side effect
#'
#' @param f a one or two sided formula formula. The right side of the formula
#'  should be function call you want to run. The left side is optional but if supplied
#'  should be the name of the reactive value to save the result to.
#'
#' @return NULL
#' @export
#'
#' @examples
#' execute(~cat(paste(letters[1:4], collapse = "\n"))
#' execute(let~cat(paste(letters[1:4], collapse = "\n"))
execute <- function(f){
  # extract the LHS from the formula
  # browser()
  lhs <- rlang::f_lhs(f)
  func <- rlang::as_function(rlang::new_formula(NULL, rlang::f_rhs(f)))
  if(!is.null(lhs)){
    # call f and pass the result to the function on the LHS
    eval(lhs)(func())
  } else {
    # just call the function
    func()
  }
  invisible(NULL)
}


execute(cat~paste(letters[1:4], collapse = "\n"))
execute(print~paste(letters[1:4], collapse = "\n"))
execute(~paste(letters[1:4], collapse = "\n"))

rlang::new_formula()

f <- ~print("h")
rlang::f_lhs()


# a function that caches its value
a <- (function(){
  cache <- NULL
  function(val){
    if(missing(val)) {
      return(cache)
    } else {
      cache <<- val
    }
  }
})()

a()
a(3)
a()

missing()

f <- a ~ paste(letters, sep = "-")
res <- rlang::f_lhs(f)
rlang::f_lhs(f) <- NULL
f <- rlang::as_function(f)
r <- f(...)
if(!is.null(res)) eval(res)(r)


lhs

eval(lhs)(5)
eval(lhs)()

rlang::f_lhs(~a)


(function(...){
  rnorm(5, ...)
})(n = 10)
