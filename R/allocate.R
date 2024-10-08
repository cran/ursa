# for interpoaltion: packages 'interp', 'MBA'. 'akima' is under ACM license
# D:\ongoing\CloudMailRu\pdf\R\akima_ACM--alternatives_GPL.pdf 
'allocate' <- function(vec,coords=c("x","y"),nodata=NA,attr=".+",fun=c("mean","sum","n")
                      ,cellsize=NA,resetGrid=FALSE,verbose=FALSE)
{
   class(vec) ## initizize evaluation 'vec' before
  # vec <- list(x=vec$x,y=vec$y,conc=vec$conc,speed=vec$speed)
   fun <- match.arg(fun)
   kind <- switch(fun,mean=1L,sum=2L,n=4L,0L)
   onlyGrid <- FALSE
   if (resetGrid)
      session_grid(NULL)
   if (is.character(vec)) {
     # vec <- .shp.read(vec)
      vec <- spatialize(vec,engine="sp")
   }
   if (inherits(vec,c("SpatialPointsDataFrame","SpatialPixelsDataFrame"))) {
      requireNamespace("sp",quietly=.isPackageInUse())
      z <- methods::slot(vec,"data")
      lname <- colnames(z)
      opW <- options(warn=ifelse(.isPackageInUse(),-1,1))
      proj4 <- sp::proj4string(vec)
      options(opW)
      vec <- as.data.frame(sp::coordinates(vec),stringsAsFactors=FALSE)
      colnames(vec) <- c("x","y")
   }
   else if ((inherits(vec,c("sf")))&&
      (.grep("^sfc_.+$",class(vec[[attr(vec,"sf_column")]]),value=TRUE))=="sfc_POINT") {
      proj4 <- sf::st_crs(vec)$proj4string
      z <- vec
      sf::st_geometry(z) <- NULL
      lname <- colnames(z)
      vec <- as.data.frame(sf::st_coordinates(vec))
      colnames(vec) <- c("x","y")
   }
   else if ((inherits(vec,c("sfc")))&&(inherits(vec,"sfc_POINT"))) {
      spatial_data(vec) <- data.frame(value=1L)
      z <- spatial_data(vec)
      proj4 <- spatial_crs(vec)
      lname <- spatial_colnames(vec)
      vec <- data.frame(spatial_coordinates(vec))
      colnames(vec) <- c("x","y")
   }
   else {
      proj4 <- attr(vec,"proj")
      if ((!is.data.frame(vec))&&(is.list(vec)))
         vec <- as.data.frame(do.call("cbind",lapply(vec,I)))
      if (!is.data.frame(vec))
         return(NULL)
      mname <- colnames(vec)
      indX <- .grep("^coords.x1$",mname)
      if (!length(indX))
         indX <- .grep("^x$",mname)
      if (!length(indX))
         indX <- .grep("^x1$",mname)
      if (!length(indX))
         indX <- .grep("^lon",mname)
      if (!length(indX))
         indX <- .grep("^east",mname)
      indY <- .grep("^coords.x2$",mname)
      if (!length(indY))
         indY <- .grep("^y$",mname)
      if (!length(indY))
         indY <- .grep("^x2$",mname)
      if (!length(indY))
         indY <- .grep("^lat",mname)
      if (!length(indY))
         indY <- .grep("^north",mname)
      if ((!length(indX))&&(!length(indY))) {
         indX <- .grep("^000x1$",mname)
         indY <- .grep("^000x2$",mname)
      }
      if ((!length(indX))&&(!length(indY))) {
         indX <- .grep(paste0("^",coords[1],"$"),mname)
         indY <- .grep(paste0("^",coords[2],"$"),mname)
      }
      ind <- c(indX[1],indY[1])
      if ((any(is.na(ind)))||(length(ind)!=2))
         stop("unable to detect 'x' and 'y' coordinates")
      lname <- .grep(attr,mname[-ind],value=TRUE)
      if (!length(lname)) {
        # lname <- "<location>"
        # z <- data.frame(z=rep(1,nrow(vec)))
         z <- data.frame(z=numeric())
         onlyGrid <- TRUE
      }
      else {
        # z <- vec[,-ind,drop=FALSE] ## -- 20160505
        # z <- subset(vec,select=lname) ## ++ 20150505
         if (inherits(vec,"data.table"))
            z <- vec[,lname,with=FALSE] ## try vec[,-ind,drop=FALSE][,lname,with=FALSE]
         else {
            z <- vec[,-ind,drop=FALSE][,lname,drop=FALSE] ## modified 20170128
            if (inherits(z,"data.frame"))
               z <- as.data.frame(lapply(z,c))
         }
      }
      if (inherits(vec,"data.table"))
         vec <- vec[,ind,with=FALSE]
      else
         vec <- vec[,ind,drop=FALSE]
      colnames(vec) <- c("x","y")
      ind <- .grep("^(lon|lat)",mname)
      if ((length(ind)==2)&&(is.null(proj4)))
         proj4 <- paste(.crsWGS84())
   }
   if (is.null(proj4))
      proj4 <- ""
   ind <- which(unname(sapply(z,function(x)
                      any(!(class(x) %in% c("integer","numeric","logical"))))))
  # ind <- which(sapply(z,class) %in% c("factor","character","POSIXt"))
   isCategory <- length(ind)>0
   isStack <- isCategory & ncol(z)>1
   vname <- vector("list",ncol(z))
   a <- lapply(z,levels)
   names(vname) <- colnames(z)
   if (isCategory) {
      for (i in ind) {
         val <- factor(z[,i])
         vname[[i]] <- levels(val)
         z[,i] <- as.numeric(val)-1
      }
   }
   if (!.is.grid((getOption("ursaSessionGrid")))) {
      x <- sort(unique(vec$x))
      y <- sort(unique(vec$y))
      minx <- min(x)
      maxx <- max(x)
      miny <- min(y)
      maxy <- max(y)
      difx <- diff(x)
      dify <- diff(y)
      resx <- unique(difx)
      resy <- unique(dify)
      if ((length(resx)!=1)||(length(resy)!=1))
      {
        # print(resx,digits=16)
        # print(resy,digits=16)
         if (!is.na(cellsize))
            resx <- resy <- cellsize
         else {
           # if (sd(diff(x))/mean(difx)<1)
            mx <- mean(resx)
            my <- mean(resy)
            if (FALSE)
               NULL
            if ((any(abs(resx-mx)<1e-11))&&(any(abs(resy-my)<1e-11))) {
               resx <- mx
               resy <- my
            }
            else if ((sd(difx)/mean(difx)<0.01)&&(sd(dify)/mean(dify)<0.01)) {
               resx <- max(resx) # min(resx) mx
               resy <- max(resy) # min(resy) mx
            }
            else {
               isU <- FALSE
               n <- sqrt(2*nrow(z))
               if (FALSE) ## coarse
                  res <- min(c(v1=(maxx-minx)/n,v2=(maxy-miny)/n))
               else if (FALSE) ## extra quality
                  res <- median(c(difx,dify))
               else {
                  if (inherits(vec,"data.table"))
                     vec <- as.data.frame(vec)
                  d <- .dist2(vec,vec,verbose=FALSE)$dist
                  if (verbose) {
                     print(range(d),digits=16)
                     print(summary(d))
                  }
                  res <- unique(d)
                  if (!(isU <- length(res)==1L)) {
                    # res <- min(d)
                     res <- median(d)
                     if ((res==mean(d))||(res==min(d)))
                        isU <- TRUE
                     else if (max(res)/min(res)-1<.Machine$double.eps) {
                        isU <- TRUE
                     }
                  }
               }
               if (!isU) {
                  p <- pretty(res)
                  res <- p[which.min(abs(res-p))]
               }
               resx <- resy <- res
               if (verbose)
                  print(c(x=resx,y=resy))
            }
         }
      }
      minx <- min(x)-resx/2
      miny <- min(y)-resy/2
      maxx <- max(x)+resx/2
      maxy <- max(y)+resy/2
     # g0 <- regrid(minx=minx,miny=miny,maxx=maxx,maxy=maxy,resx=resx,resy=resy
     #             ,columns=(maxx-minx)/resx,rows=(maxy-miny)/resy,crs=proj4)
      g0 <- regrid(minx=minx,miny=miny,maxx=maxx,maxy=maxy,resx=resx,resy=resy
                  ,crs=proj4,verbose=verbose)
     # g0 <- regrid(setbound=)
      session_grid(g0)
   }
   if (onlyGrid)
      return(session_grid())
   res <- ursa_new(bandname=lname) ## --20240217, remove 'ignorevalue=nodata'
   z <- as.matrix(z)
   if (is.na(nodata))
      nodata <- .optimal.nodata(z)
   z[is.na(z)] <- nodata
   g1 <- res$grid
   nb <- length(lname)
   dimx <- dim(res$value)
   dimy <- with(res$grid,c(columns,rows,nb))
   if (verbose)
      .elapsedTime(paste0(fun,":start"))
   res$value <- .Cursa(C_rasterize,dat=numeric(prod(dimy)),dim=as.integer(dimy)
                  ,bbox=as.numeric(with(g1,c(minx,miny,maxx,maxy)))
                  ,x=as.numeric(vec$x),y=as.numeric(vec$y),value=as.numeric(z)
                  ,nodata=as.numeric(nodata)
                  ,len=nrow(vec),kind=kind,NAOK=FALSE)$dat
   if (abs(nodata)<1)
      res$value[abs(res$value-nodata)<1e-27] <- NA
   else
      res$value[abs(res$value/nodata-1)<1e-6] <- NA
   dim(res$value) <- dimx
   class(res$value) <- "ursaNumeric" #ifelse(isCategory,"ursaCategory","ursaNumeric")
   if (isStack)
      res <- ursa_stack(res)
   if (isCategory) {
      for (i in ind) {
         ct <- rep(NA,length(vname[[i]]))
         names(ct) <- vname[[i]]
         class(ct) <- "ursaColorTable"
         if (isStack) {
            res[[i]] <- as.integer(round(res[[i]]))
            res[[i]]$colortable <- ct
         }
         else {
            res[i] <- as.integer(round(res[i]))
            res$colortable <- ct
         }
      }
   }
   if (verbose)
      .elapsedTime(paste0(fun,":finish"))
   res
}
