## Modification of function Anova in package genefilter
oneWayAnova <- function(cov, na.rm = TRUE){
    function(x) {
        if (na.rm) {
            drop <- is.na(x)
            x <- x[!drop]
            cov <- cov[!drop]
        }
        m1 <- lm(x ~ cov)
        m2 <- lm(x ~ 1)
        av <- anova(m2, m1)
        return(av[["Pr(>F)"]][2])
    }
}
