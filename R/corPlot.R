## Modifikation of function plot.cor of package "sma"
corPlot <- function (x, new = FALSE, col = grey(0:50/50), labels = FALSE, 
                     labcols = "black", title = "", protocol = FALSE, ...){
    n <- ncol(x)
    corr <- x

    if(new) corr <- cor.na(x)

    if(protocol)
      layout(matrix(c(1, 2), 1, 2), width = c(10, 3))
    else
      layout(matrix(c(1, 2), 1, 2), width = c(10, 2))
    if(min(corr) > 0.5) 
      col.nr <- trunc((round(min(corr), 2) - 0.5)/0.5*length(col))
    else 
      col.nr <- 1
    image(1:n, 1:n, corr[, n:1], col = col[col.nr:length(col)], axes = FALSE, 
          xlab = "sample index", ylab = "", ...)

    if (length(labcols) == 1) {
        axis(2, at = n:1, labels = labels, las = 2, cex.axis = 0.8,
            col.axis = labcols)
        axis(1, at = 1:n, labels = 1:n, las = 1, cex.axis = 0.8,
            col.axis = labcols)
    }

    if (length(labcols) == n) {
        cols <- unique(labcols)
        for (i in 1:length(cols)) {
            which <- (1:n)[labcols == cols[i]]
            axis(2, at = (n:1)[which], labels = labels[which],
                las = 2, cex.axis = 0.8, col.axis = cols[i])
            axis(1, at = which, labels = labels[which], las = 2,
                cex.axis = 0.8, col.axis = cols[i])
        }
    }
    title(title)
    box()

    x.bar <- seq(min(0.5, min(corr, na.rm = TRUE)), max(corr, na.rm = TRUE), length = length(col))
    if(protocol){
      par(mar = c(5.1, 1, 4.1, 5))
      image(1, x.bar, matrix(x.bar, 1, length(x.bar)), axes = FALSE, xlab = "", ylab = "", col = col, ...)
      box()
      x.small <- seq(x.bar[1], x.bar[length(x.bar)], length = 10)
      axis(4, at = c(x.small[2], x.small[9]), labels = c("dissimilar", "similar "), las = 2)
    }else{
      par(mar = c(5.1, 1, 4.1, 3))
      maColorBar(unique(round(x.bar, 2)), horizontal = FALSE, col = col, main = "")
    }
    
    layout(1)
    par(mar = c(5, 4, 4, 2) + 0.1)

    invisible()
}

