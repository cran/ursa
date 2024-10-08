'.fasterize' <- function(obj,by=NULL,fun="last") {
   for (pkg in c("sf","raster","fasterize")[3]) 
      if (!requireNamespace(pkg,quietly=.isPackageInUse(),partial=FALSE))
         stop(paste("Package",sQuote(pkg),"is required for this operation"))
  # if (is.character(obj))
   obj <- spatialize(obj,engine="sf")
  # print(spatial_crs(obj))
   if (is.null(by)) {
      by <- spatial_fields(obj)
      if (!length(by)) {
        # spatial_data(obj) <- data.frame(mask=seq(spatial_count(obj)))
         spatial_data(obj) <- data.frame(mask=0)
         by <- NULL
      }
   }
  # if (!("OGRFID" %in% spatial_fields(obj)))
  #    obj$OGRFID <- seq(spatial_count(obj))
  # print(spatial_data(obj))
  # q()
  # if (inherits(by,"try-error"))
  #    by <- NULL
   byVal <- obj[,by,drop=TRUE]
   isCategory <- is.character(by) && is.character(byVal)
   if (isCategory) {
      ##~ a <- fasterize::fasterize(sf=obj,raster=as.Raster(),by=by,fun=fun)
      ##~ str(a)
      ##~ print(object.size(a))
      ##~ print(a)
      ##~ q()
      a <- ursa(fasterize::fasterize(sf=obj
                                    ,raster=as.Raster()
                                    ,by=by,fun=fun))
      if (length(names(a))==length(unique(byVal)))
         names(a) <- unique(byVal)
      else if (identical(gsub("(\\s|\\.)","",names(a))
                   ,gsub("(\\s|\\.)","",unique(byVal))))
         names(a) <- unique(byVal)
      a2 <- sum(a*seq(a),cover=0)
      res <- reclass(a2,src=seq(a),dst=names(a))
      if (!is.null(by))
         names(res) <- by
   }
   else {
      isList <- length(by)>1
      if (isList) {
         bname <- by
         if (!("OGRFID" %in% spatial_fields(obj)))
            obj$OGRFID <- seq(spatial_count(obj))
         by <- "OGRFID"
      }
      if (devel <- FALSE) {
         a9 <- fasterize::fasterize(sf=obj,raster=as.Raster()
                    ,field=by,by=NULL,fun=fun) ## in memory 
         print(object.size(a9))
         print(object.size(ursa(a9)))
         q()
      }
      res <- ursa(fasterize::fasterize(sf=obj,raster=as.Raster()
                                      ,field=by,by=NULL,fun=fun))
     # res <- fasterize::fasterize(sf=obj,raster=as.Raster(),field=by,by=NULL,fun=fun)
      if (isList) {
         src <- as.integer(names(ursa(res,"table")))
         res <- lapply(bname,function(x) {
            a <- reclass(res,src=src,dst=obj[src,x,drop=TRUE])
            names(a) <- x
            a
         })
         names(res) <- bname
      }
      else if (!is.null(by))
         names(res) <- by
   }
   res
}
