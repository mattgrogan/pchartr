#' is.nelson_rule
#'
#' Is this a Nelson Rule object?
#' 
#' 
#' @param x object of class /code{nelson_rule}
#' 
#' @export
is.nelson_rule <- function(x) {
  # Check for class name
  if (!inherits(x, 'nelson_rule')) return(FALSE)
  
  # Check that members are available
  if (is.null(x$rule) | is.null(x$descr)) return(FALSE)
  
  return(TRUE)
}