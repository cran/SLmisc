\name{gapStat}
\alias{gapStat}
\title{ Gap statistic for estimating the number of data clusters }
\description{
  Calculates a goodness of clustering measure based on the average dispersion 
  compared to a reference distribution.
}
\usage{
gapStat(data = swiss, class = g, M = 500)
}
\arguments{
  \item{data}{ matrix or data.frame, data}
  \item{class}{ a vector describing the cluster memberships of the rows of \code{data} }
  \item{M}{ integer, number of Monte Carlo samples }
}
\details{
  This function is based on the function \code{\link[SAGx]{gap}} of package \code{"SAGx"}.
}
\value{
  vector with components \code{"gap statistic"} and \code{"SE of simulation"}.
}
\references{
  T. Hastie, R. Tibshirani and G. Walther (2001). Estimating the number of data 
  clusters via the Gap statistic. J.R. Statist. Soc. B, 63, pp. 411--423.

  Tibshirani, R., Walther, G. and Hastie, T. (2000). Estimating the number of clusters 
  in a dataset via the Gap statistic. Technical Report. Stanford. 
  
  Per Broberg (2006). SAGx: Statistical Analysis of the GeneChip. 
  R package version 1.9.7. 
  http://home.swipnet.se/pibroberg/expression\_hemsida1.html
}
\author{ Dr. Matthias Kohl (SIRS-Lab GmbH) \email{kohl@sirs-lab.com}}
%\note{ ~~further notes~~ }
\seealso{ \code{\link[SGAx]{gap}}, \code{\link{kmeansGap}} }
\examples{
x <- rbind(matrix(rnorm(150, sd = 0.1), ncol= 3),
              matrix(rnorm(150, mean = 1, sd = 0.1), ncol = 3),
              matrix(rnorm(150, mean = 2, sd = 0.1), ncol = 3),
              matrix(rnorm(150, mean = 3, sd = 0.1), ncol = 3))

gap.stat <- matrix(NA, ncol = 2, nrow = 9)
for(i in 2:10){
  cl <- kmeans(x, i)
  gap.stat[i-1, ] <- gapStat(x, cl$clust, M = 100)
}

## choose cluster size to be the smallest value such that the following 
## is positive
(res <- gap.stat[1:8,1] - gap.stat[2:9,1] + gap.stat[2:9,2])
c(2:9)[res >= 0] 
}
\keyword{cluster}
