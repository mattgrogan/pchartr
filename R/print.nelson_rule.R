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
  cat(paste0("\nTested against dataset of length ", length(x$x)))

  if (x$violated) {
    cat(paste0("\nFirst detected at point ", x$first))
    tmp <- paste(x$which, collapse=", ")
    cat(paste0("\nWhich: ", tmp))
  }

  cat("\n")
  invisible(x)
}