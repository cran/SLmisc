################################################################################
## Funktion zur Mittelung über die Replikationen innerhalb
## der Arrays unter Berücksichtigung der Qualität der Spots (flags)
################################################################################
## x: matrix or data.frame with expression data
## flags: matrix or data.frame with spot flags
## use.flags: should flags be respected and in which way
##            NULL: flags are not used; minimum flag value of replicated spots
##                  is returned
##            'max': only spots with maximum flag value are used
##            'median': only spots with flag values larger or equal to median are used
##            'mean': only spots with flag values larger or equal to mean are used
## ndups: integer, number of replicates on chip. The number of rows of 'x' must 
##        be divisible by 'ndups'
## spacing: the spacing between the rows of 'x' corresponding to replicated spots, 
##          'spacing=1' for consecutive spots; cf. function unwrapdups in 
##          package 'limma'
repMeans <- function(x, flags, use.flags = NULL, ndups, spacing){
  if(any(dim(x) != dim(flags)))
    stop("wrong dimensions of 'x' and 'flags'")
  if(!isTRUE(all.equal(trunc(nrow(x)/ndups), nrow(x)/ndups)))
    stop("'nrow(x)' not divisible by 'ndups'")

  anz <- ncol(x)
  Exprs <- matrix(NA, nrow = nrow(x)/ndups, ncol = anz)
  Flags <- matrix(NA, nrow = nrow(x)/ndups, ncol = anz)
  for(i in 1:anz){
    exprs.tmp <- unwrapdups(M = x[,i], ndups = ndups, spacing = spacing)
    flags.tmp <- unwrapdups(M = flags[,i], ndups = ndups, spacing = spacing)

    if(!is.null(use.flags)){
      if(use.flags == "max"){
        max.flags <- apply(flags.tmp, 1, max)
        max.flags <- matrix(rep(max.flags, ndups), ncol = ndups)
        is.max <- (flags.tmp == max.flags)
        exprs.tmp[!is.max] <- NA
        Exprs[,i] <- rowMeans(exprs.tmp, na.rm = TRUE)
        Flags[,i] <- max.flags[,1]
      }
      if(use.flags == "median"){
        median.flags <- apply(flags.tmp, 1, median)
        median.flags <- matrix(rep(median.flags, ndups), ncol = ndups)
        is.median <- (flags.tmp >= median.flags)
        exprs.tmp[!is.median] <- NA
        Exprs[,i] <- rowMeans(exprs.tmp, na.rm = TRUE)
        Flags[,i] <- median.flags[,1]
      }    
      if(use.flags == "mean"){
        mean.flags <- apply(flags.tmp, 1, mean)
        mean.flags <- matrix(rep(mean.flags, ndups), ncol = ndups)
        is.mean <- (flags.tmp >= mean.flags)
        exprs.tmp[!is.mean] <- NA
        Exprs[,i] <- rowMeans(exprs.tmp, na.rm = TRUE)
        Flags[,i] <- mean.flags[,1]
      }    
      if(!use.flags %in% c("max", "median", "mean"))
        stop("wrong 'use.flags' specified")
    }else{
      Exprs[,i] <- rowMeans(exprs.tmp, na.rm = TRUE)
      Flags[,i] <- apply(flags.tmp, 1, min)
    }
  }

  return(list(exprs = Exprs, flags = Flags))
}
