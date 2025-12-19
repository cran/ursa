'.chunk' <- function(nr,size=96,selected=NULL,name="") {
   if (indexed <- !is.null(selected))
      nr <- length(selected)
   nl <- ceiling(nr/size)
   res <- vector("list",nl)
   i1 <- 1
  # if (is.null(selected))
  #    seqi <- seq(nr)
  # else
  #    seqi <- selected
   for (i in seq(nr)) {
      i2 <- i1+size-1
      if (i2>nr)
         i2 <- nr
      res[[i]] <- if (indexed) selected[i1:i2] else i1:i2
      i1 <- i2+1
      if (i1>nr)
         break
   }
   if (nchar(name))
      names(res) <- sprintf(sprintf("%s%%0%dd",name,nchar(length(1:i))),1:i)
   res
}
'chunk_line' <- function(obj,mem=100,mul=1,selected=NULL) {
   if (!is.ursa(obj))
      return(NULL)
   y <- obj$con
   if (any(is.na(with(y,c(samples,lines,bands))))) {
      nb <- nband(obj)
      nc <- obj$grid$columns
      nr <- obj$grid$rows
   }
   else {
      nb <- y$bands
      nr <- y$lines
      nc <- y$samples
   }
   a <- as.integer(ceiling(1e6*mem*mul/(8*nc*nb)))
   return(.chunk(nr,a,selected,"line"))
}
'chunk_band' <- function(obj,mem=100,mul=1,selected=NULL) {
   if (!is.ursa(obj))
      return(NULL)
   y <- obj$con
   if (anyNA(with(y,c(samples,lines,bands)))) {
      nb <- nband(obj)
      nc <- obj$grid$columns
      nr <- obj$grid$rows
   }
   else {
      nb <- if (!is.na(y$posZ[1])) length(y$posZ) else y$bands
      nr <- y$lines
      nc <- y$samples
   }
   a <- as.integer(ceiling(1e6*mem*mul/(8*nc*nr)))
  # print(data.frame(nb=nb,a=a,row.names="band"))
   return(.chunk(nb,a,selected,"band"))
}
'chunk_expand' <- function(ind,size=3) {
   nr <- session_grid()$rows
   s1 <- ceiling(size)
   if (!(s1%%2))
      s1 <- s1+1
   s2 <- as.integer(floor(s1/2))
   sq <- seq(s2)
   r2 <- c(min(ind)-rev(sq),ind,max(ind)+sq)
   src <- r2[r2>=1 & r2<=nr]
   dst <- na.omit(match(ind,src))
   list(src=src,dst=dst)
}
