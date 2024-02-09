#' Create a quick boxplot in ggplot.
#'
#' This will graph a given numeric vector in a ggplot-style box plot with the
#' y-axis labeled after the input 'vector'.  OR this will graph side-by-side
#' box plots of a numeric vector grouped by a given categorical vector.
#' The user may indicate 'vertical' (default) or 'horizontal' plotting.  
#' The 'scales' package is required to implement viridis_c() over the number of 
#' unique categories in 'group'.
#'
#' @param numeric Variable for which to create the box plot.
#' @param group (optional) Categorical variable by which to group the box plots.
#' @param fill  A color for the numeric box plot (default is 'NULL').
#' @param orientation Allows user to indicate vertical or horizontal plotting.
#'
#' @return This function returns a ggplot box plot object.
#'
#' @examples
#' ## Create a box plot of 'var' over four categories in 'group'.
#' var <- rnorm(100)
#' group <- factor(rep(c("Yes", "No", "Maybe", "Probably"), each = 25))
#' gbox(var, group, orient = 'horizontal', fill = 'orange')
#'
#' @import
#'   ggplot2
#'   magrittr
#'
#' @export

gbox <- function(var, group = NULL, fill = NULL, orient = 'vertical') {
  
  # error handling
  if (!is.numeric(var)) {
    stop("Variable 'var' must be a numeric vector.")
  }
  if (!is.null(group) && !is.factor(group) && !is.character(group)) {
    stop("Variable 'group', if provided, must be a factor or character vector 
         of the same length as 'var'.")
  }
  if (!is.null(fill) && !is.character(fill)) {
    stop("Variable 'fill', if provided, must be a character vector 
         of valid colors.")
  }
  if (orient != 'vertical' && orient != 'horizontal') {
    stop("Variable 'orientation' must be either 'vertical' or 'horizontal'.")
  }
  # plotting:
  # check for single numeric vector
  if (is.null(group)) {
    # single box plot
    ggplot(data.frame(var), aes(x = "", y = var)) +
      geom_boxplot(fill = fill) +
      ggtitle(paste("Range of ", names(var)))+
      labs(x = NULL, y = names(var))+
      if (orient == 'horizontal') {
        coord_flip() }
  } else {
    # side-by-side box plots
    # color palette based on number of groups

    require(scales)
    
    n_groups <- length(unique(group))
    base_colors <- c('yellow2',"orange", "orange2", "orange3", "orange4",
                     "sienna4", "sienna", "sienna3", "sienna2", "sienna1",'orangered')
    create_palette <- function(n_groups) {
      palette <- base_colors[1:n_groups]
      return(palette)
    }
    palette <- create_palette(n_groups)
    
    ggplot(data.frame(var, group), aes(x = group, y = var, fill = group)) +
      geom_boxplot(fill = palette) +
      ggtitle(paste("Range of ", names(var))) +
      labs(x = names(group), y = names(var)) +
      if (orient == 'horizontal') {
        coord_flip()
      }
  }
}


