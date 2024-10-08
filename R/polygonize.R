'polygonize' <- function(obj,fname,engine=c("native","sf")
                        ,verbose=NA,...) {
  # class(obj)
   missing(obj) ## keep`session_grid` from reset in `a <- polygonize(envi_read())`
   engList <- as.character(as.list(match.fun("polygonize"))[["engine"]])[-1]
   if ((isNamespaceLoaded("sp"))||(nchar(system.file(package="sp"))>0))
      engList <- c(engList,"sp")
   engine <- match.arg(engine[1],engList)
   if (engine=="sp") {
      isSF <- FALSE
      isSP <- TRUE
   }
   else if (engine=="sf") {
      isSF <- requireNamespace("sf",quietly=.isPackageInUse())
      isSP <- !isSF
   }
   else {
      loaded <- loadedNamespaces() # .loaded()
      if ("sf" %in% loaded)
         isSF <- TRUE
      else if (("sp" %in% loaded)||("rgdal" %in% loaded))
         isSF <- FALSE
      else
         isSF <- requireNamespace("sf",quietly=.isPackageInUse())
      isSP <- !isSF
   }
   if ((!is.na(verbose))&&(verbose))
      print(c(isSP=isSP,isSF=isSF))
   if (is.na(verbose))
      verbose <- isSP
   g0 <- session_grid()
   if ((!missing(obj))&&(is.numeric(obj))&&(length(obj)==4)&&
       ((!anyNA(match(names(obj),c("minx","maxx","miny","maxy"))))||
        (!anyNA(match(names(obj),c("xmin","xmax","ymin","ymax")))))) {
      crs <- if (is.null(attr(obj,"crs"))) session_crs() else attr(obj,"crs")
      obj <- regrid(bbox=unname(obj),dim=c(1,1),crs=crs)
   }
   onlyGeometry <- missing(obj) || .is.grid(obj)
   isList <- !onlyGeometry && .is.ursa_stack(obj)
   if ((!isList)&&(!((onlyGeometry)||(is.ursa(obj))||(is.data.frame(obj))))) ## propose?
  # if ((!isList)&&(!(is.ursa(obj))))
      return(NULL)
  # requireNamespace("sp",quietly=.isPackageInUse())
  # requireNamespace("methods",quietly=.isPackageInUse())
  # obj$grid$seqy <- nuimeric()
   g1 <- if (onlyGeometry) {
      if (missing(obj))
         session_grid()
      else 
         ursa(obj,"grid")
   }
   else if (isList)
      ursa_grid(obj[[1]])
   else
      ursa_grid(obj)
   if (is.null(g1))
      g1 <- g0
   prj <- ursa_proj(g1)
   if (onlyGeometry)
      b <- as.data.frame(g1)
   else if (isList) {
      for (i in seq_along(obj)) {
         b0 <- as.data.frame(obj[[i]],na.rm=FALSE)
         b <- if (i==1) b0 else cbind(b,b0[,3:ncol(b0),drop=FALSE])
      }
      ind <- which(apply(b[,3:ncol(b),drop=FALSE],1,function(x) all(is.na(x))))
      if (length(ind))
         b <- b[-ind,]
   }
   else if (is.ursa(obj)) {
      if (length(ind <- attr(obj$value,"sparse"))) {
         val <- data.frame(unclass(ursa_value(obj)),check.names=FALSE)
         colnames(val) <- names(obj)
         b <- cbind(data.frame(t(coord_cr(obj,ind))),val)
         attr(b,"crs") <- ursa_crs(obj)
      }
      else {
         b <- as.data.frame(obj)
      }
   }
   else
      b <- obj
  # b <- b[sample(seq(nrow(b)),1e3),]
   n <- nrow(b)
   sa <- vector("list",n)
   if (!length(g1$seqx))
      dx <- g1$resx/2
   else {
      dx <- diff(g1$seqx)
      dx <- (c(head(dx,1),dx)+c(dx,tail(dx,1)))/2
      ind <- match(b$x,g1$seqx) ## or .near?
      if (anyNA(ind))
         stop("TODO: near matching (x-axis)")
      dx <- dx[ind]/2
   }
   if (!length(g1$seqy))
      dy <- g1$resy/2
   else {
      dy <- diff(g1$seqy)
      dy <- (c(head(dy,1),dy)+c(dy,tail(dy,1)))/2
      ind <- match(b$y,g1$seqy) ## or .near?
      if (anyNA(ind))
         stop("TODO: near matching (y-axis)")
      dy <- dy[ind]/2
   }
   if (isSF) {
      if (verbose)
         cat("1 of 3: create polygons from points...")
      xy <- cbind(b$x-dx,b$y-dy,b$x-dx,b$y+dy,b$x+dx,b$y+dy,b$x+dx,b$y-dy
                 ,b$x-dx,b$y-dy)
      if (!onlyGeometry) {
         b <- b[,3:ncol(b),drop=FALSE]
         for (i in seq(ncol(b))) {
            if ((!is.factor(b[,i]))&&(.is.integer(na.omit(b[,i]))))
               b[,i] <- as.integer(b[,i])
         }
      }
      sa <- apply(xy,1,function(x) {
         res <- list(matrix(x,ncol=2,byrow=TRUE))
         class(res) <- c("XY","POLYGON","sfg")
        ## check for new versions of 'sf':
        # res2 <- sf::st_polygon(res);print(identical(res,res2));q()
         res
      })
      if (verbose)
         cat(" done!\n2 of 3: create geometry...")
      sa <- sf::st_sfc(sa)
      if (verbose)
         cat(" done!\n")
      if (!onlyGeometry) {
         if (verbose)
            cat("3 of 3: assign data to geometry...")
         p4s <- if (nchar(g1$crs)) g1$crs else sf::NA_crs_
         sa <- sf::st_sf(b,coords=sa,crs=p4s)
         if (verbose)
            cat(" done!\n")
      }
      else {
         if (verbose)
            cat("3 of 3: assign data to geometry... skipped!\n")
         sf::st_crs(sa) <- .p4s2epsg(g1$crs)
        # spatial_crs(sa,verbose=TRUE) <- g1$crs ## alt
      }
      if ((TRUE)&&(!onlyGeometry)) { ## ++ 20171128
         colnames(sa)[head(seq(ncol(sa)),-1)] <- colnames(b)
      }
   }
   else if (isSP) {
      if (verbose) {
         pb <- ursaProgressBar(min=0,max=n,tail=TRUE)
         i <- -1
         repeat({
            n1 <- n*(10^i)
            if (n1<100)
               break
            i <- i-1
         })
         n1 <- 10^(-i-1)
      }
      for (i in seq(n))
      {
         x <- b$x[i]+c(-dx,-dx,+dx,+dx,-dx)
         y <- b$y[i]+c(-dy,+dy,+dy,-dy,-dy)
         sa[[i]] <- sp::Polygons(list(sp::Polygon(cbind(x,y))),i)
         if ((verbose)&&(i%%n1==0))
            setUrsaProgressBar(pb,i)
      }
      if (verbose)
         close(pb)
      sa <- sp::SpatialPolygons(sa,proj4string=sp::CRS(prj,doCheckCRSArgs=FALSE))
      if (!onlyGeometry) {
         b <- b[,3:ncol(b),drop=FALSE]
         for (i in seq(ncol(b)))
            if ((!is.factor(b[,i]))&&(.is.integer(na.omit(b[,i]))))
               b[,i] <- as.integer(b[,i])
         sa <- sp::SpatialPolygonsDataFrame(sa,data=b,match.ID=FALSE)
      }
   }
   if (!missing(fname)) {
     # return(.shp.write(sa,fname,...))
      return(spatial_write(sa,fname,verbose=verbose,...))
   }
   session_grid(g0)
   sa
}
'.vectorize' <- function(obj,fname,opt="",engine=c("sf","sp"),verbose=FALSE) {
   stopifnot('\'gdal_polygonize.py\' not found'=!nchar(Sys.which("gdal_polygonize.py"))==0)
   engine <- match.arg(engine)
   if (engine=="sp") {
      isSF <- FALSE
      isSP <- TRUE
   }
   else if (engine=="sf") {
      isSF <- requireNamespace("sf",quietly=.isPackageInUse())
      isSP <- !isSF
   }
   else {
      loaded <- loadedNamespaces() # .loaded()
      if ("sf" %in% loaded)
         isSF <- TRUE
      else if (("sp" %in% loaded)||("rgdal" %in% loaded))
         isSF <- FALSE
      else
         isSF <- requireNamespace("sf",quietly=.isPackageInUse())
      isSP <- !isSF
   }
   internal <- missing(fname)
   if (internal) {
      bname <- .maketmp() # tempfile()
      fname <- paste0(bname,".shp")
   }
   Fout <- .maketmp()
   write_envi(obj,paste0(Fout,"."))
   cmd <- paste("python",Sys.which("gdal_polygonize.py")
               ,opt," -f \"ESRI Shapefile\"",Fout,fname)
   if (verbose)
      print(cmd)
   system(cmd)
   envi_remove(Fout)
   if (!internal)
      return(0L)
   ret <- spatialize(fname,engine=engine)
   list1 <- dir(path=dirname(bname),pattern=paste0("^",basename(bname)),full.names=TRUE)
   file.remove(list1)
   ret
}
