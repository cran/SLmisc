\name{kmeansGap}
\alias{kmeansGap}
\alias{plot.clusterGap}
\title{ k-means Clustering via gap statistic }
\description{
  Perform k-means clustering on a data matrix where the number of clusters k is 
  chosen via the gap statistic.
}
\usage{
kmeansGap(x, iter.max = 10, nstart = 1, 
                algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), 
                k.max = 20, M = 100, hclust = FALSE)
\method{plot}{clusterGap}(x, ...)
}
\arguments{
  \item{x}{ A numeric matrix of data, or an object that can be coerced to
          such a matrix (such as a numeric vector or a data frame with
          all numeric columns). In case of \code{plot.clusterGap} the argument
          \code{x} is an object of class \code{clusterGap}. }
  \item{iter.max}{ The maximum number of iterations allowed in function 
                         \code{\link[stats]{kmeans}}. }
  \item{nstart}{ If \code{centers} is a number (i.e., \code{hclust = FALSE}), 
                      how many random sets should be chosen? }
  \item{algorithm}{ character, may be abbreviated. }
  \item{k.max}{ integer, maximum number of clusters. }
  \item{M}{ integer, number of Monte Carlo samples. }
  \item{hclust}{ logical, use hclust with method \code{"average"} to determine
                       initial cluster centers. }
  \item{...}{optional arguments to \code{plot.clusterGap} - not yet implemented}
}
\details{
  For details on k-means clustering see \code{\link[stats]{kmeans}}. The function
  proceeds computing k-means clustering and the corresponding gap statistic 
  \eqn{Gap(k)} for increasing number of clusters until
  \eqn{Gap(k) - Gap(k+1) + s(k+1) >= 0} where \eqn{s(k+1)} is SE of simulation 
  or \code{k.max} is reached, respectively.
}
\value{
  An object of class \code{"clusterGap"} which consist of the gap statistic as well
  as the clustering result.
}
\references{ 
  Forgy, E. W. (1965) Cluster analysis of multivariate data: 
  efficiency vs interpretability of classifications. Biometrics 21, 768-769.

  Hartigan, J. A. and Wong, M. A. (1979). A K-means clustering algorithm. 
  Applied Statistics 28, 100-108.

  Lloyd, S. P. (1957, 1982)  Least squares quantization in PCM. Technical Note, 
  Bell Laboratories.  Published in 1982 in IEEE Transactions on Information 
  Theory 28, 128-137.

  MacQueen, J. (1967)  Some methods for classification and analysis of multivariate
  observations. In Proceedings of the Fifth Berkeley Symposium on  Mathematical 
  Statistics and  Probability, eds L. M. Le Cam & J. Neyman, 1, pp. 281-297. 
  Berkeley, CA: University of California Press.

  T. Hastie, R. Tibshirani and G. Walther (2001). Estimating the number of data 
  clusters via the Gap statistic. J.R. Statist. Soc. B, 63, pp. 411--423.

  Tishirani, R., Walther, G. and Hastie, T. (2000) Estimating the number of clusters in 
  a dataset via the Gap statistic. Technical Report. Stanford.
}
\author{ Dr. Matthias Kohl (SIRS-Lab GmbH) \email{kohl@sirs-lab.com}}
%\note{ ~~further notes~~ }
\seealso{ \code{\link[SGAx]{gap}}, \code{\link{kmeansGap}} }
\examples{
x <- rbind(matrix(rnorm(150, sd = 0.1), ncol= 3),
              matrix(rnorm(150, mean = 1, sd = 0.1), ncol = 3),
              matrix(rnorm(150, mean = 2, sd = 0.1), ncol = 3),
              matrix(rnorm(150, mean = 3, sd = 0.1), ncol = 3))

res <- kmeansGap(x = x, nstart = 10)
plot(res)
}
\keyword{cluster}
