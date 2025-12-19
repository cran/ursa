'mtfrm.ursaRaster' <- function(x) {
   ct <- ursa(x,"category")
   apply(ursa_value(x),2,function(v) ct[v+1L])
}
'ursa_match' <- function(obj,value) {
   if (is.character(value))
      value <- match(value,ursa(obj,"category"))-1L
   ind <- apply(ursa_value(obj),2,function(v) v %in% value)
   if (FALSE) ## drops posZ
      return(ursa_new(ind,bandname=names(obj))>0)
   obj[] <- as.integer(ind)
   obj>0
}
