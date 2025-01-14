'.grid.skeleton' <- function()
{
   g1 <- list(columns=NA_integer_,rows=NA_integer_,resx=NA_real_,resy=NA_real_
             ,minx=NA_real_,maxx=NA_real_,miny=NA_real_,maxy=NA_real_
             ,seqx=numeric(0),seqy=numeric(0)
             ,crs="",retina=NA)
   class(g1$crs) <- c("ursaCRS","character")[1]
   class(g1) <- "ursaGrid"
   g1
}
'.is.grid' <- function(obj) inherits(obj,"ursaGrid")
'print.ursaGrid' <- function(x,...) {
   class(x) <- paste0(".",class(x))
   if (!length(x$seqx))
      x$seqx <- NULL
   if (!length(x$seqy))
      x$seqy <- NULL
   if (is.na(x$retina))
      x$retina <- NULL
  # x$crs <- .crsBeauty(x$crs)
   str(x,formatNum=function(x) format(x,scientific=FALSE),...)
}
'str.ursaGrid' <- function(object,...) {
  # args <- list(...)
  # print(str(args))
  # q()
   class(object) <- paste0(".",class(object))
   if (FALSE) {
      y <- list('colunms rows'=c(object$columns,object$rows)
               ,'resx resy'=c(object$resx,object$resy)
               ,'minx miny maxx maxy'=c(object$minx,object$miny,object$maxx,object$maxy)
               ,'crs'=c(object$crs))
      class(y) <- class(object)
      return(y)
   }
   if (!length(object$seqx))
      object$seqx <- NULL
   if (!length(object$seqy))
      object$seqy <- NULL
   if ((!is.null(object$retina))&&(is.na(object$retina)))
      object$retina <- NULL
  # object$crs <- .crsBeauty(object$crs)
   str(object,...)#,formatNum=function(x) format(x,scientific=FALSE),...)
  # do.call("str",lx,...)#,formatNum=function(x) format(x,scientific=FALSE),...)
}
'dim.ursaGrid' <- function(x) structure(c(x$rows,x$columns),names=c("lines","samples"))
'.grid.equal' <- function(g1,g2) {
   (g1$columns==g2$columns)&&
   (g1$rows==g2$rows)&&
   (.is.eq(g1$minx,g2$minx))&&
   (.is.eq(g1$maxx,g2$maxx))&&
   (.is.eq(g1$miny,g2$miny))&&
   (.is.eq(g1$maxy,g2$maxy))&&
   1L
}
'as.data.frame.ursaGrid' <- function(x,row.names=NULL,optional=FALSE,...) {
   expand.grid(x=seq(x,"x"),y=seq(x,"y"),KEEP.OUT.ATTRS=FALSE)
}
'.identicalGrid' <- function(src,dst) {
   if ((!.is.grid(src))||(!.is.grid(dst)))
      return(NULL)
   cond1 <- .identicalCRS(src$crs,dst$crs)
   src$crs <- character()
   dst$crs <- character()
  # cond2 <- identical(src,dst)
   cond2 <- .grid.equal(src,dst)
   cond1 & cond2
}
