% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ghist.R
\name{ghist}
\alias{ghist}
\title{Create a quick histogram plot in ggplot.}
\usage{
ghist(
  var,
  binwidth = NULL,
  title = NULL,
  outline_color = "grey15",
  fill_color = "slategrey"
)
}
\arguments{
\item{var}{This is the vector to be plotted.}

\item{binwidth}{Default NULL, provides an opportunity to specify binwidth.}

\item{title}{The user may provide a title}

\item{outline_color}{This is the color of any contour of the histogram bars. Default is 'grey15'}

\item{fill_color}{This is the color of the histogram bar fill. Default is 'slategrey'}
}
\value{
This function returns a ggplot histogram object.
}
\description{
This will graph a variable provided as a vector in a ggplot-style histogram with the
x-axis labeled after the input 'variable' and the y-axis labeled "Frequency".
}
\examples{
## Create a histogram of variable t.
t <- rnorm(100)
names(t) <- c('t-variable')
ghist(t, title="Graph of t")

}
