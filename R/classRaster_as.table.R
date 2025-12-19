'as.table.ursaRaster' <- 'ursa_table' <- function(x,...) {
  # print("as.table.ursaRaster")
   ct <- ursa_colortable(x)
   seqct <- seq(length(ct))-1L
  # x[x==names(ct)[1]] <- NA
   isCT <- .is.colortable(x)
   multi <- length(x)>1
   if (devel <- isTRUE(getOption("ursaDevel")))
      print(c(multi=multi,isCT=isCT))
   if (!isCT) {
      res <- table(x$value,...)
     # ## try mget(names(match.call())[2])
     # names(dimnames(res)) <- as.character(match.call())[2]
      names(dimnames(res)) <- NULL
      return(res)
   }
   if (T) { ## 20251012 proposed
      if (inherits(x$value,c("ursaSymbol")))
         x <- reclass(x)
      res <- t(apply(x$value,2,function(y) {
         ta <- table(y)
         if (length(ta)==length(ct))
            return(ta)
         ind <- match(as.numeric(names(ta)),seqct)
         freq <- rep(0L,length(ct))
        # names(freq) <- names(ct)
         freq[ind] <- ta
        # freq <- as.table(freq)
         freq
      }))
      if (ncol(res)!=length(ct))
         return(res)
      dimnames(res) <- list(names(x),names(ct))
      if (!multi)
         res <- res[1L,,drop=TRUE]
      return(res)
     # ind <- match(as.numeric(colnames(res)),seqct)
     # if (any(is.na(ind))) {
     #    return(res)
     # }
     # res2 <- array(0,dim=c(length(x),length(ct)),dimnames=list(names(x),names(ct)))
     # res2[,ind] <- res
     # if (!multi)
     #    res2 <- res2[1L,,drop=TRUE]
     # return(res2)
   }
   res <- table(x$value,...)
   ind <- match(as.numeric(names(res)),seq(length(ct))-1)
   if (any(is.na(ind))) {
      return(res)
   }
   freq <- rep(0L,length(ct))
   freq[ind] <- res
   res <- as.table(freq)
   dimnames(res) <- list(names(ct))
   res
}
#'table' <- function(...) UseMethod("table")
#'table.NULL' <- function(...) NULL
#'table.default' <- function(x,...) base::table(x,...)
#'table.ursaRaster' <- function(x,...) as.table.ursaRaster(x,...)
