corDist <- function (x, method = "pearson", diag = FALSE, upper = FALSE, 
                           abs = FALSE, use = "pairwise.complete.obs"){
    if (!is.na(pmatch(method, "pearson")))
        method <- "pearson"

    METHODS <- c("pearson", "kendall", "spearman", "cosine")
    method <- pmatch(method, METHODS)

    if (is.na(method))
        stop("invalid distance method")

    if (method == -1)
        stop("ambiguous distance method")

    N <- nrow(x <- as.matrix(x))

    if (!is.matrix(x))
        stop("'x' must be a matrix")

    if(method != 4){
      if(!abs)
        d <- 1 - cor(t(x), use = use, method = METHODS[method])
      else
        d <- 1 - abs(cor(t(x), use = use, method = METHODS[method]))
    }else{
      na.rm <- use == "pairwise.complete.obs"
      if (na.rm) {
        M <- rowSums(!is.na(x))
        M2 <- (!is.na(x)) %*% t(!is.na(x))
        x[is.na(x)] <- 0
        M <- sqrt(M %*% t(M))/M2
      }
      y <- rowSums(x^2)
      if(!abs)
        d <- 1 - M*tcrossprod(x)/sqrt(tcrossprod(y))
      else
        d <- 1 - abs(M*tcrossprod(x)/sqrt(tcrossprod(y)))
    }
    d <- d[lower.tri(d)]
    attr(d, "Size") <- N
    attr(d, "Labels") <- dimnames(x)[[1]]
    attr(d, "Diag") <- diag
    attr(d, "Upper") <- upper
    attr(d, "method") <- METHODS[method]
    attr(d, "call") <- match.call()
    class(d) <- "dist"

    return(d)
}
