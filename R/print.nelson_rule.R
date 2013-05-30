#' print.nelson_rule
#'
#' Prints summary of Nelson rule object
#' 
#' @param x object of class /code{nelson_rule}
#' @param ... further arguments passed to or from other methods
#' 
#' @S3method print nelson_rule
#' @method print nelson_rule
#' @export
print.nelson_rule <- function(x, ...) {
  cat(paste0("\nNelson Rule ", x$rule, ': ', x$descr, "\n"))
  cat(paste0("\nViolated: ", x$violated))
  if (x$violated) {
    tmp <- paste(x$which, collapse=", ")
    cat(paste0("\nWhich: ", tmp))
  }
  cat(paste0("\nLength of data (x): ", length(x$x)))
  cat("\n")
  invisible(x)
}