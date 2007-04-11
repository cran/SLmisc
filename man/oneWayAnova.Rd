\name{oneWayAnova}
\alias{oneWayAnova}
\docType{data}
\title{ A function for Analysis of Variance }
\description{
  This function is a slight modification of function \code{\link[genefilter]{Anova}} of
  package \code{"genefilter"}.
}
\usage{
oneWayAnova(cov, na.rm = TRUE)}
\arguments{
  \item{cov}{ The covariate. It must have length equal to the number of
          columns of the array that the result of \code{oneWayAnova} will be 
          applied to. }
  \item{na.rm}{ a logical value indicating whether 'NA' values should be
          stripped before the computation proceeds. }
}
\details{
  The function returned by \code{oneWayAnova} uses \code{\link[stats]{lm}} to fit 
  a linear model of the form \code{lm(x ~ cov)}, where \code{x} is the set of gene
  expressions. The F statistic for an overall effect is computed and the corresponding
  p-value is returned.

  The function \code{\link[genefilter]{Anova}} instead compares the computed 
  p-value to a prespecified p-value and returns \code{TRUE}, if the computed p-value
  is smaller than the prespecified one.
}
\value{
  \code{oneWayAnova} returns a function with bindings for \code{cov} that will
  perform a one-way ANOVA.
     
  The covariate can be continuous, in which case the test is for a linear effect 
  for the covariate.
}
\references{
  R. Gentleman, V. Carey, W. Huber and F. Hahne (2006). 
  genefilter: methods for filtering genes from microarray experiments. 
  R package version 1.13.7.
}
\author{ Dr. Matthias Kohl (SIRS-Lab GmbH) \email{kohl@sirs-lab.com}}
%\note{ ~~further notes~~ }
\seealso{ \code{\link[SGAx]{gap}}, \code{\link{kmeansGap}} }
\examples{
set.seed(123)
af <- oneWayAnova(c(rep(1,5),rep(2,5)))
af(rnorm(10))
}
\keyword{htest}
\keyword{models}
