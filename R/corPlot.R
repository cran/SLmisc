## Modifikation of function plot.cor of package "sma"
corPlot <- function (x, new = FALSE, col = grey(0:50/50), minCor = 0.5, 
                           labels = FALSE, labcols = "black", title = "", protocol = FALSE,       
                           cex.axis = 0.8, cex.axis.bar = 1, ...){
    n <- ncol(x)
    corr <- x

    if(new) corr <- cor(x, use = "pairwise.complete.obs")
    if(minCor < -1 | minCor > 1)
      stop("'minCor' has to be in [-1, 1]")

    layout(matrix(c(1, 2), 1, 2), width = c(10, 3))
    if(min(corr) >= minCor){
      col.nr <- trunc((round(min(corr), 2) - minCor)/(1-minCor)*length(col))
      minCorInd <- FALSE
    }else{ 
      col.nr <- 1
      corr[corr < minCor] <- minCor
      minCorInd <- TRUE
    }
    image(1:n, 1:n, corr[, n:1], col = col[col.nr:length(col)], axes = FALSE, 
          xlab = "sample index", ylab = "", ...)

    if (length(labcols) == 1) {
        axis(2, at = n:1, labels = labels, las = 2, cex.axis = cex.axis,
            col.axis = labcols)
        axis(1, at = 1:n, labels = 1:n, las = 1, cex.axis = cex.axis,
            col.axis = labcols)
    }

    if (length(labcols) == n) {
        cols <- unique(labcols)
        for (i in 1:length(cols)) {
            which <- (1:n)[labcols == cols[i]]
            axis(2, at = (n:1)[which], labels = labels[which],
                las = 2, cex.axis = cex.axis, col.axis = cols[i])
            axis(1, at = which, labels = (1:n)[which], las = 2,
                cex.axis = cex.axis, col.axis = cols[i])
        }
    }
    title(title)
    box()

    x.bar <- seq(min(minCor, min(corr, na.rm = TRUE)), max(corr, na.rm = TRUE), length = length(col))
    x.small <- seq(x.bar[1], x.bar[length(x.bar)], length = 10)
    par(mar = c(5.1, 1, 4.1, 5))
    if(protocol){
      image(1, x.bar, matrix(x.bar, 1, length(x.bar)), axes = FALSE, xlab = "", ylab = "", col = col, ...)
      box()
      axis(4, at = c(x.small[3], x.small[8]), labels = c("<-- dissimilar", "similar -->"), las = 0, cex.axis = cex.axis.bar)
    }else{
      image(1, x.bar, matrix(x.bar, 1, length(x.bar)), axes = FALSE, xlab = "", ylab = "", col = col, ...)
      box()
      x.small <- seq(x.bar[1], x.bar[length(x.bar)], length = 10)
      if(minCorInd)
        Labels <- c(signif(rev(x.small[2:10]), 2), paste("<=", signif(x.small[1], 2), sep = ""))
      else
        Labels <- signif(rev(x.small), 2)      
      axis(4, at = rev(x.small), labels = Labels, las = 1, cex.axis = cex.axis.bar)
    }
    
    layout(1)
    par(mar = c(5, 4, 4, 2) + 0.1)

    invisible()
}

