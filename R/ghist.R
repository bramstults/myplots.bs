#' Create a quick histogram plot in ggplot.
#'
#' This will graph a variable provided as a vector in a ggplot-style histogram with the
#' x-axis labeled after the input 'variable' and the y-axis labeled "Frequency".
#'
#' @param var This is the vector to be plotted.
#' @param binwidth Default NULL, provides an opportunity to specify binwidth.
#' @param outline_color This is the color of any contour of the histogram bars. Default is 'grey15'
#' @param title The user may provide a title
#' @param fill_color This is the color of the histogram bar fill. Default is 'slategrey'
#' 
#' 
#'
#' @return This function returns a ggplot histogram object.
#'
#' @examples
#' ## Create a histogram of variable t.
#' t <- rnorm(100)
#' names(t) <- c('t-variable')
#' ghist(t, title="Graph of t")
#'
#' @import
#'   ggplot2
#'   magrittr
#'
#' @export

ghist <- function(var, binwidth = NULL, title = NULL, outline_color = "grey15", 
                   fill_color = "slategrey") {
  
  if (!is.data.frame(var)) {
    var <- data.frame(var)
  }
  
  p <- ggplot(data=var, aes(x = var)) +
    geom_histogram(binwidth = binwidth,
                   color = outline_color,
                   fill = fill_color) +
    theme(plot.title = element_text(size = 12, face = 'bold')) +
    labs(x = names(var), y = 'Frequency', face = 'bold')
  
  # error handling and setting title
  if (is.null(title)) {
    p <- p + ggtitle(paste('Graph of', names(var)))  # Use default title
  } else {
    p <- p + ggtitle(title)  # Use user-provided title
  }
  return(p)
}

#   
#   # Error handling: check for valid input of named vector
#   if (title == NULL) {
#     stop("A title should be given.")
#     }
#   
#     ggplot(aes(x = var)) +
#     geom_histogram(binwidth = binwidth,
#                    color = outline_color,
#                    fill = fill_color
#                   ) +
#     ggtitle(title) +
#     (ifelse(is.null(title), ggtitle(paste('Graph of', names(var))), ggtitle(title))) +
#     theme(plot.title = element_text(size = 12, face = 'bold'))+
#     labs(x = names(var), y = 'Frequency', face = 'bold')
# }
