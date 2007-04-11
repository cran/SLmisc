\name{corDist}
\alias{corDist}
\title{ Correlation Distance Matrix Computation }
\description{
  The function computes and returns the correlation and absolute correlation distance 
  matrix computed by using the specified distance measure to compute the distances 
  between the rows of a data matrix.
}
\usage{
corDist(x, method = "pearson", diag = FALSE, upper = FALSE, abs = FALSE, 
          use = "pairwise.complete.obs")
}
\arguments{
  \item{x}{ a numeric matrix or data frame }
  \item{method}{ the correlation distance measure to be used. This must be one of
          \code{"pearson"}, \code{"spearman"}, \code{"kandall"} or \code{"cosine"}. 
          Any unambiguous substring can be given. }
  \item{diag}{ logical value indicating whether the diagonal of the distance
          matrix should be printed by 'print.dist'. }
  \item{upper}{ logical value indicating whether the upper triangle of the
          distance matrix should be printed by 'print.dist'. }
  \item{abs}{ logical, compute absolute correlation distances }
  \item{use}{ character, correponds to argument \code{use} of function 
          \code{\link[stats]{cor}} }
}
\details{
  The function computes the Pearson, Spearman, Kendall or Cosine sample correlation
  and absolute correlation; confer Section 12.2.2 of Gentleman et al (2005). For more
  details about the arguments we refer to functions \code{\link[stats]{dist}} and 
  \code{\link[stats]{cor}}.
}
\value{
  'corDist' returns an object of class \code{"dist"}; cf. \code{\link[stats]{dist}}.
}
\references{ 
  Gentleman R. Ding B., Dudoit S. and Ibrahim J. (2005) Distance Measures in DNA 
  Microarray Data Analysis. In: Gentleman R., Carey V.J., Huber W., Irizarry R.A. and 
  Dudoit S. (editors) Bioinformatics and Computational Biology Solutions Using R and
  Bioconductor. Springer.
}
\author{ Dr. Matthias Kohl (SIRS-Lab GmbH) \email{kohl@sirs-lab.com}}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link[sma]{plot.cor}}}
\examples{
## only a dummy example
M <- cor(matrix(rnorm(1000), ncol = 20))
D <- corDist(M)
}
\keyword{multivariate}
