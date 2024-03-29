'compress' <- function(obj) {
   if (inherits(obj,c("ursaCategory","ursaNumeric"))) {
      if (length(attr(obj,"sparse")))
         return(obj)
      dimx <- dim(obj)
      ind <- .Cursa(C_makeField,as.numeric(obj),dim=dimx
                   ,res=integer(dimx[1]),NAOK=TRUE)$res
      ind <- ind[ind!=0L]
      if (!length(ind)) {
         attr(obj,"sparse") <- NULL ## neccessary?
         return(obj)
      }
      cl <- class(obj)
      obj <- obj[ind,,drop=FALSE]
      class(obj) <- cl
      attr(obj,"sparse") <- ind
      .gc()
      return(obj)
   }
   if (!is.ursa(obj))
      return(obj)
   dimx <- dim(obj$value)
   if (is.null(dimx))
      return(obj)
   dimx2 <- obj$dim
   if (!is.na(obj$con$posZ[1]))
      dimx2[2] <- length(obj$con$posZ)
   if (!identical(dimx2,dimx))
      return(obj)
   if (length(attr(obj$value,"sparse")))
      return(obj)
   if (TRUE) {
      ind <- .Cursa(C_makeField,as.numeric(obj$value),dim=dimx
                            ,res=integer(dimx[1]),NAOK=TRUE)$res
   }
   else
   {
      ind <- apply(obj$value,2,function(x) !all(is.na(x)))
      ind <- which(ind)
   }
   ind <- ind[ind!=0L]
   if (!length(ind))
   {
      attr(obj$value,"sparse") <- NULL ## neccessary?
      return(obj)
   }
   cl <- class(obj$value)
   obj$value <- obj$value[ind,,drop=FALSE]
   class(obj$value) <- cl
   attr(obj$value,"sparse") <- ind #if ((1)&&(n*2>obj$dim[1])) -which(ind2==0L) else ind
   obj$dim <- dim(obj$value)
   .gc()
   obj
}
'decompress' <- function(obj)
{
   if (inherits(obj,c("ursaCategory","ursaNumeric"))) {
      sparse <- attr(obj,"sparse")
      if (is.null(sparse)) ## no compression
         return(obj)
      g0 <- session_grid()
      columns <- g0$columns
      rows <- g0$rows
      if (all(na.omit(sparse)<0))
         sparse <- seq(columns*rows)[sparse]
      val <- array(NA,dim=c(columns*rows,dim(obj)[2]))
      cl <- class(obj)
      val[sparse,] <- obj[]
      obj <- val
      class(obj) <- cl
      attr(obj,"sparse") <- NULL ## neccessary?
      .gc()
      return(obj)
   }
   if (!is.ursa(obj))
      return(obj)
   sparse <- attr(obj$value,"sparse")
   if (is.null(sparse)) ## no compression
      return(obj)
   columns <- obj$grid$columns
   rows <- obj$grid$rows
   if (all(na.omit(sparse)<0))
      sparse <- seq(columns*rows)[sparse]
   val <- array(NA,dim=c(columns*rows,obj$dim[2]))
   cl <- class(obj$value)
   val[sparse,] <- obj$value[]
   obj$value <- val
   class(obj$value) <- cl
   attr(obj$value,"sparse") <- NULL ## neccessary?
   obj$dim <- dim(val)
   .gc()
   obj
}
'.is.sparse' <- function(obj) length(attr(ursa_value(obj),"sparse"))>0
'.is.sparse.deprecated' <- function(obj) {
   if (is.ursa(obj))
      return(length(attr(ursa_value(obj),"sparse"))>0)
   if (inherits(obj,c("ursaNumeric","ursaCategory")))
      return(length(attr(obj,"sparse"))>0)
   NULL
}
