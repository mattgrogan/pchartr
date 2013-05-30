#' plot.nelson_rule
#'
#' Creates a control chart with violation points highlighted in red.
#' 
#' @param x object of class /code{nelson_rule}
#' @param ... arguments to be passed to methods, such as graphical parameters.
#' 
#' @return An object of type 
#' @S3method plot nelson_rule
#' @method plot nelson_rule
#' @export
plot.nelson_rule <- function(x, ...) {
  # TODO: Add proper labels for x-axis
  # TODO: Figure out how to return the plot properly
  
  xlabels <- 1:length(x$x)
  
  # Set up plot and add ucl, lcl, and mean lines
  nr.plot <- plot(x=xlabels, y=x$x, type="o", pch=19, ...)
  lines(x$ucl, lty=3, col="red")
  lines(x$lcl, lty=3, col="red")
  lines(x$mean, lty=3, col="black")
  
  # Add red points for violators
  points(x=x$which, y=x$x[x$which], col="red", pch=19)
  
  # Return plot
  return(nr.plot)
}