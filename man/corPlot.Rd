\name{corPlot}
\alias{corPlot}
\title{ Plot of similarity matrix based on correlation }
\description{
  Plot of similarity matrix. This function is a slight modification of function
  \code{plot.cor} of package \code{"sma"}.
}
\usage{
corPlot(x, new = FALSE, col = grey(0:50/50), minCor, labels = FALSE, 
          labcols = "black", title = "", protocol = FALSE, cex.axis = 0.8, 
          cex.axis.bar = 1, ...)
}
\arguments{
  \item{x}{ data or correlation matrix, respectively }
  \item{new}{ If \code{new=FALSE}, \code{x} must already be a correlation matrix. 
              If \code{new=TRUE}, the correlation matrix for the columns of 
              \code{x} is computed and displayed in the image. }
  \item{col}{ colors palette for image }
  \item{minCor}{numeric value in [-1,1], used to adjust \code{col}}
  \item{labels}{ vector of character strings to be placed at the tickpoints,
          labels for the columns of \code{x}. }
  \item{labcols}{ colors to be used for the labels of the columns of \code{x}.
                  \code{labcols} can have either length 1, in which case all 
                  the labels are displayed using the same color, or the same 
                  length as \code{labels}, in which case a color is specified 
                  for the label of each column of \code{x}. }
  \item{title}{ character string, overall title for the plot. }
  \item{protocol}{ logical, display color bar without numbers }
  \item{cex.axis}{ The magnification to be used for axis annotation relative to the 
                         current setting of 'cex'; cf. \code{\link[graphics]{par}}.}
  \item{cex.axis.bar}{ The magnification to be used for axis annotation of the color 
                               bar relative to the current setting of 'cex'; cf. 
                               \code{\link[graphics]{par}}.}
  \item{\dots}{ graphical parameters may also be supplied as arguments to the
          function (see \code{\link[graphics]{par}}). For comparison purposes, 
          it is good to set \code{zlim=c(-1,1)}. }
}
\details{
  This functions generates the so called similarity matrix (based on correlation) 
  for a microarray experiment. 
  
  If \code{min(x)}, respectively \code{min(cor(x))} is smaller than \code{minCor}, 
  the colors in \code{col} are adjusted such that the minimum correlation value 
  which is color coded is equal to \code{minCor}.
}
%\value{
%}
\references{ 
  Sandrine Dudoit, Yee Hwa (Jean) Yang, Benjamin Milo Bolstad and with 
  contributions from Natalie Thorne, Ingrid Lönnstedt and Jessica Mar.
  sma: Statistical Microarray Analysis.\cr
  http://www.stat.berkeley.edu/users/terry/zarray/Html/smacode.html
}
\author{ Dr. Matthias Kohl (SIRS-Lab GmbH) \email{kohl@sirs-lab.com}}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link[sma]{plot.cor}}}
\examples{

}
\keyword{univar}
