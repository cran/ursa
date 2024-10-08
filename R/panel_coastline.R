## http://www.soest.hawaii.edu/pwessel/gshhg/ 2.3.7 2017-06-15
## http://gis-lab.info/forum/viewtopic.php?t=22977
## http://thematicmapping.org/downloads/world_borders.php



##~ '.panel_coastline' <- function(
                               ##~ coastline=TRUE,col="grey60",fill="transparent"
                              ##~ ,detail=NA,density=NA,angle=NA,land=FALSE
                              ##~ ,lwd=1,lty=1L,...) {
   ##~ NULL
##~ }
'compose_coastline' <- function(...) {
   arglist <- list(...)
   kwd <- "coast(line)*"
   coastline <- .getPrm(arglist,name=paste0("(",kwd,"|decor)$")
                       ,class=list("integer","logical"),default=TRUE)
   if (any(!coastline)) {
      res <- list(coast_xy=NULL)
      class(res) <- "ursaCoastLine"
      return(res)
   }
   res <- .getPrm(arglist,name=kwd,class="ursaCoastLine",default=NULL)
   if (!is.null(res)) {
      return(res)
   }
   res <- getOption("ursaPngCoastLine")
   if (!is.null(res))
      return(res)
   bg <- sum(c(col2rgb(getOption("ursaPngBackground")))*c(0.30,0.59,0.11))
   defcol <- ifelse(bg<128,"#FFFFFF3F","#0000003F") # grey60
   col <- .getPrm(arglist,name="(col|line)",kwd=kwd,default=defcol) 
   fill <- .getPrm(arglist,name="fill",kwd=kwd,default=c("#FFFF003F","transparent")[2])
   detail <- .getPrm(arglist,name="detail",kwd=kwd,default=NA_character_)
   density <- .getPrm(arglist,name="density",kwd=kwd,default=NA_real_)
   angle <- .getPrm(arglist,name="angle",kwd=kwd,default=NA_real_)
   land <- .getPrm(arglist,name="land",kwd=kwd,default=FALSE)
   lwd <- .getPrm(arglist,name="lwd",kwd=kwd,default=0.5)
   lty <- .getPrm(arglist,name="lty",kwd=kwd,default=1L)
   fail180 <- .getPrm(arglist,name="fail180",kwd=kwd,default=NA)
   obj <- .getPrm(arglist,name=paste0("(^$|",kwd,"$)") ## name="^$"
                 ,class=list("character","matrix","SpatialPolygonsDataFrame","sf")#[-3]
                 ,default=NULL)
   verbose <- .getPrm(arglist,name="verbose",kwd=kwd,default=FALSE)
   if (is.integer(coastline)) {
      panel <- coastline
     # coastline <- TRUE
   }
   else
      panel <- 0L
   invisible(.compose_coastline(obj=obj,panel=panel,col=col,fill=fill,detail=detail
                     ,density=density,angle=angle
                     ,land=land,lwd=lwd,lty=lty,fail180=fail180,verbose=verbose))
}
'.compose_coastline' <- function(obj=NULL,panel=0,col=NA,fill="transparent"
                                ,detail=NA,density=NA,angle=NA,land=FALSE
                                ,lwd=0.5,lty=1,fail180=NA,verbose=FALSE) {
  # forceProj4string <- isTRUE(getOption("ursaTransformProj4sring"))
   if (verbose)
      str(list(obj=obj,panel=panel,col=col,fill=fill,detail=detail
              ,density=density,angle=angle
              ,land=land,lwd=lwd,lty=lty,fail180=fail180
              ,forceProj4string=isTRUE(getOption("ursaSFProjectProj4string"))))
   g1 <- .panel_grid() # session_grid()
   if (is.null(g1))
      g1 <- .compose_grid()
   if (!is.null(obj)) {
      isPoly <- inherits(obj,c("sf","SpatialPolygonsDataFrame"))
      if ((is.matrix(obj))&&(ncol(obj)==2))
         coast_xy <- obj
      else if ((is.character(obj))||(isPoly)) {
         if ((isPoly)||(.lgrep("\\.shp",obj))) {
            if (isPoly)
               a <- obj
            else {
              # a <- .shp.read(obj)
               a <- spatialize(obj)#,engine="sp")
            }
            if (.isSP(a)) {
               a <- lapply(methods::slot(a,grep("(polygons|lines)"
                                 ,methods::slotNames(a),value=TRUE)),function(x) {
                  y <- lapply(methods::slot(x,grep("(Polygons|Lines)"
                                 ,methods::slotNames(x),value=TRUE)),function(y) {
                     do.call("rbind",lapply(list(sp::coordinates(y),cbind(NA,NA))
                                           ,matrix,ncol=2))
                  })
                  do.call("rbind",lapply(y,matrix,ncol=2))
               })
               coast_xy <- head(do.call("rbind",lapply(a,matrix,ncol=2)),-1)
               rm(a)
            }
            else if (.isSF(a)) {
               if (.lgrep("^multi",spatial_shape(a))) {
                  a <- do.call("rbind",lapply(spatial_coordinates(a),function(x) {
                     do.call("rbind",lapply(unlist(x,recursive=FALSE),rbind,cbind(NA,NA)))
                  }))
               }
               else {
                  a <- do.call("rbind",lapply(spatial_coordinates(a),function(x) {
                     do.call("rbind",lapply(x,rbind,cbind(NA,NA)))
                  }))
               }
               coast_xy <- head(a,-1)
               rm(a)
            }
         }
         else if (.lgrep("\\.rds$",obj)) {
            stop("COASLINE GRID AGAIN")
           # g1 <- session_grid() ## earlier
            coast_xy <- readRDS(obj)
            if (.isCRS(g1$crs)) {
              # b <- attributes(coast_xy)
               coast_xy <- .project(coast_xy,g1$crs)
              # attributes(coast_xy) <- b
            }
         }
         else
            coast_xy <- NULL
      }
      if (!is.null(coast_xy))
      {
         shadow <- unname(col2rgb(fill,alpha=TRUE)[4,1])
         options(ursaPngShadow=ifelse(shadow %in% c(0,255),"",fill))
         res <- list(coast_xy=coast_xy,grid=g1,detail=detail,panel=panel
                    ,col=col,fill=fill,shadow=shadow,land=land
                    ,density=density,angle=angle,lwd=lwd,lty=lty)
         class(res) <- "ursaCoastLine"
         options(ursaPngCoastLine=res)
         return(res)
      }
   }
   projClass <- .crsProj(g1$crs)
   isLongLat <- "longlat" %in% projClass
   isMerc <- "merc" %in% projClass
   isCea <- "cea" %in% projClass
   isUTM <- "utm" %in% projClass
   proj <- g1$crs
   if ((is.list(proj))&&("input" %in% names(proj)))
      proj <- proj[["input"]]
   if (length(proj)>1)
      proj <- proj[nchar(proj)==max(nchar(proj))]
   if (!.isCRS(proj)) {
      return(NULL)
   }
   isDetail <- !is.na(detail)
   if (is.na(detail))
      detail <- "l"
   if (!(detail %in% c("l","i","h","f",NA)))
      message(paste("coastline detail:",detail))
   if ((FALSE)&&(.lgrep("proj=(merc|longlat)",proj))&&((g1$maxx<=20037508))) {
      if (detail %in% c("f"))
         detail <- "h180"
      else
         detail <- paste0(detail,"180")
   }
   if (FALSE)
   {
      g2 <- with(g1,my.expand.grid(x=seq(minx,maxx,length=16)
                                  ,y=seq(miny,maxy,length=16)))
      ll <- .project(with(g2,cbind(x,y)),g1$crs,inv=TRUE)
      if (all(ll[,2]<65))
         return(NULL)
   }
   fpath <- getOption("ursaRequisite")
  # if (!nchar(fpath))
  #    fpath <- system.file("requisite",package="ursa")
   if (!is.na(detail)) {
      fname <- file.path(fpath,paste0("coast-",detail,".rds"))
      if (!file.exists(fname)) {
         detail <- "l"
         fname <- file.path(fpath,paste0("coast-",detail,".rds"))
         if (!file.exists(fname))
            fname <- system.file("requisite/coast-l.rds",package="ursa")
      }
      xy <- readRDS(fname)
   }
   else {
      stop("Detalization level is unknown")
     # xy <- readRDS("C:/platt/shapefile/auxiliary/thematicmapping.org/countries.rds")
   }
   coast_xy <- cbind(lon=xy[,1],lat=xy[,2])
   ind <- .grep("^\\d+$",proj)
   if (length(ind))
   {
      proj4 <- "" ## was NA
      try(proj4 <- get(paste0("epsg",proj[ind])))
      if (!nchar(proj4))
      {
         if (.rgdal_loadedNamespaces())
            proj4 <- .rgdal_CRSargs(sp::CRS(sprintf("EPSG:%s",proj[ind])))
         else
            proj4 <- sf::st_crs(proj[ind])$proj4string
      }
   }
   else {
      proj4 <- proj # paste(proj,collapse=" ")
      if (!.crsForceWKT()) ## proj4sting is faster, if operated by 'sf'
         proj4 <- .proj4string(proj4)
   }
  # print(data.frame(isLongLat=isLongLat,isMerc=isMerc))
   if (FALSE) { ## deprecate
      if ((!isLongLat)&&(!isMerc)) {
         lat0 <- .gsub("^.*\\+lat_[012]=(\\S+)\\s.*$","\\1",proj4)
         lat0 <- .crsLat0(proj4)
         if (lat0==proj4) {
           # lat0 <- .gsub("^.*\\+lat_ts=(\\S+)\\s.*$","\\1",proj4)
            if (lat0==proj4) {
               epsg <- .gsub("^.+init=epsg:(\\d+).*$","\\1",proj4)
               if ((epsg==proj4))
                  lat0 <- NA
               else {
                  epsg <- as.integer(epsg)
                  if (epsg %in% c(3411,3413,3408,3571:3576,6931,6973))
                     lat0 <- 90
                  else if (epsg %in% c(3409,6932,6974,3412,3976))
                     lat0 <- -90
                  else
                     lat0 <- NA
               }
            }
            else
               lat0 <- as.numeric(lat0)
         }
         else
            lat0 <- as.numeric(lat0)
      }
      else
         lat0 <- NA
   }
   else {
      lat0 <- .crsLat0(proj4)
   }
   ant_xy <- NULL
   ind <- attr(xy,"antarctic")
   if (!is.null(ind))
      indS <- which(abs(coast_xy[ind,2])<(85.0-1e-3))
   if (!isLongLat) {
     # indNA <- which(is.na(coast_xy[,1]) | is.na(coast_xy[,2]))
     # qs::qsave(coast_xy,"C:/tmp/interim.qs");q()
      coast_xy <- .project(coast_xy,proj4,verbose=!.isPackageInUse())
      isInf <- any(is.infinite(coast_xy))
      if (isInf) {
         shadow <- unname(col2rgb(fill,alpha=TRUE)[4,1]) # if (shadow!=0)
        # coast_xy[indNA,] <- NA
         indI <- which(is.infinite(coast_xy[,1]) | is.infinite(coast_xy[,2]))
         coast_xy[indI,] <- NA
         if (dev <- TRUE) {
            ind <- NULL ## drop Antarctic
           # coast_xy <- coast_xy[500:600,]
            ind1 <- as.integer(!is.na(coast_xy[,1]) & !is.na(coast_xy[,2]))
           # print(c(ind=ind))
            ind2 <- cumsum(ind1)
           # print(ind2)
            ind3 <- which(duplicated(ind2))
           # print(ind3)
            ind4 <- diff(ind3)
           # print(ind4)
            ind5 <- which(ind4==1)
           # print(ind5)
            ind6 <- ind3[ind5]
            coast_xy <- coast_xy[-ind6,]
            if (dev2 <- TRUE) {
               if (anyNA(coast_xy[nrow(coast_xy),]))
                  coast_xy <- head(coast_xy[,1:2],-1)
               ind7 <- which(is.na(coast_xy[,1]) | is.na(coast_xy[,2]))
               ind7a <- c(1,ind7+1L)
               ind7b <- c(ind7-1L,nrow(coast_xy))
               ind8 <- NULL
               for (i in sample(seq_along(ind7a))) {
                  ind9 <- ind7a[i]:ind7b[i]
                  xy <- coast_xy[ind9,,drop=FALSE]
                  if (nrow(xy)==1)
                     ind8 <- c(ind8,c(ind9,tail(ind9,1)+1L))
                 # print(all(abs(head(xy,1)-tail(xy,1))<1e-11))
               }
               if (length(ind8))
                 coast_xy <- coast_xy[-ind8,]
            }
           # print(coast_xy)
           ## for detail=="l": [7720,] [8897,]
           # q()
         }
         else {
            res <- list()
            class(res) <- "ursaCoastLine"
            options(ursaPngCoastLine=res)
            return(res)
         }
      }
   }
   if (!is.null(ind)) {
      if ((is.na(lat0))||(lat0<=0)) {
         ant_xy <- coast_xy[ind,]
         if (length(indS))
            ant_xy <- ant_xy[indS,]
      }
      ind <- c(head(ind,1)-1,ind)
      coast_xy <- coast_xy[-ind,]
   }
   isMerc <- isMerc | isCea
   if (is.na(fail180))
      fail180 <- (isMerc || isLongLat)
   if ((fail180)||(isLongLat || isMerc)) {
      if (!isLongLat) {
         lon0 <- .crsLon0(proj4)
         B <- mean(abs(.project(rbind(cbind(lon0-180+1e-9,-45),cbind(lon0+180-1e-9,+45))
                               ,proj4)[,1]))
      }
      else
         B <- 180
      if (isMerc) {
        # B <- .crsSemiMajor(proj4)*pi
        # B <- 7720000
         '.shift' <- function(seg) {
           # if (all(seg[,2]>0)) ## debug Chukotka vs 
           #    return(NULL)
            j <- which(abs(diff(seg[,1]))>B)
            if (!length(j))
               return(NULL)
           # plot(seg[,1],seg[,2],type="l")
           # abline(v=c(-B,B),lty=2)
            center <- sign(mean(seg[,1]))
            j1 <- c(1,j+1)
            j2 <- c(j,nrow(seg))
            if (center<0)
               k <- which(seg[j1,1]>0.9*B)
            else
               k <- which(seg[j1,1]<=(-0.9*B))
           # da <- data.frame(j1=j1,j2=j2,center=center,s=0,seg=seg[j1,1])
           # da2 <- apply(da,1,function(x) range(seg[x["j1"]:x["j2"],1]))
           # da$s <- -sign(da$seg)
           # da$min <- da2[1,]
           # da$max <- da2[2,]
           # print(da)
           # print(da[k,])
            if (TRUE) { ## added 20180207
               nr <- length(j1)
               if ((1 %in% k)&&(!(nr %in% k)))
                  k <- c(k,nr)
               else if ((nr %in% k)&&(!(1 %in% k)))
                  k <- c(1,k)
            }
            j1k <- j1[k]
            j2k <- j2[k]
           # print(data.frame(j1=j1,j2=j2,center=center,seg=seg[j1,1]))
            if (center<0) { 
               for (m in seq_along(j1k))
                  seg[j1k[m]:j2k[m],1] <- seg[j1k[m]:j2k[m],1]-2*B
            }
            else {
               for (m in seq_along(j1k))
                  seg[j1k[m]:j2k[m],1] <- seg[j1k[m]:j2k[m],1]+2*B
            }
           # for (m in c(1))
           #    seg[j1[m]:j2[m],1] <- seg[j1[m]:j2[m],1]-2*B
           # da <- data.frame(j1=j1,j2=j2,center=center,seg=seg[j1,1])
           # da2 <- apply(da,1,function(x) range(seg[x["j1"]:x["j2"],1]))
           # da$min2 <- da2[1,]
           # da$max2 <- da2[2,]
           # print(da)
           # print(summary(seg))
           # plot(seg[,1],seg[,2],type="l")
           # abline(v=c(-B,B),lty=2)
            seg
         }
         if ((TRUE)||(g1$minx<(-B))||(g1$maxx>(+B))) {
            ind <- which(is.na(coast_xy[,1]))
            ind1 <- c(1,ind+1)
            ind2 <- c(ind-1,nrow(coast_xy))
            for (i in seq(length(ind1))) {
               if (ind1[i]>ind2[i])
                  next
              # if (nrow(coast_xy[ind1[i]:ind2[i],])<1e3) ## debug
              #    next
               seg <- .shift(coast_xy[ind1[i]:ind2[i],])
               if (is.null(seg))
                  next
              # str(seg)
              # if (nrow(seg)<1e3)
              #    next
              # message("-----")
              # str(seg)
              # print(summary(coast_xy[ind1[i]:ind2[i],]))
              # print(summary(seg))
              # message("=====")
              # if (nrow(seg)<1e3)
              #    next
              # print(c(i=i,ind1=ind1[i],ind2=ind2[i]))
               coast_xy[ind1[i]:ind2[i],] <- seg
            }
            if (!is.null(ant_xy)) {
               seg <- .shift(ant_xy)
               if (!is.null(seg))
                  ant_xy <- seg
            }
         }
      }
      cond1 <- g1$minx<(-B*(169.2/180)) ## east boarder of Eurasia
      cond2 <- g1$maxx>(+B)
      if (cond1) {
         if (verbose)
            print("expand to the West")
         opp1 <- coast_xy
         opp1[,1] <- opp1[,1]-2*B
         if (!is.null(ant_xy)) {
            if (FALSE) {
               opp1a1 <- opp1a2 <- ant_xy[-nrow(ant_xy),]
               opp1a1[,1] <- opp1a1[,1]-4*B
               opp1a2[,1] <- opp1a2[,1]-2*B
               opp1a <- rbind(opp1a1,c(NA,NA),opp1a2)
               rm(opp1a1,opp1a2)
            }
            else {
               opp1a <- ant_xy[-1,]
               opp1a[,1] <- opp1a[,1]-2*B
            }
         }
      }
      if (cond2) {
         if (verbose)
            print("expand the East")
         opp2 <- coast_xy
         opp2[,1] <- opp2[,1]+2*B
         if (!is.null(ant_xy)) {
            opp2a <- ant_xy[-1,]
            opp2a[,1] <- opp2a[,1]+2*B
         }
      }
      if (cond1) {
         coast_xy <- rbind(coast_xy,c(NA,NA),opp1)
         if (!is.null(ant_xy))
            ant_xy <- rbind(opp1a,ant_xy)
      }
      if (cond2) {
         coast_xy <- rbind(coast_xy,c(NA,NA),opp2)
         if (!is.null(ant_xy))
            ant_xy <- rbind(ant_xy,opp2a)
      }
   }
   if (any(is.na(coast_xy[1,])))
      coast_xy <- coast_xy[-1,]
   n <- nrow(coast_xy)
   if (any(is.na(coast_xy[n,])))
      coast_xy <- coast_xy[-n,]
   if (!is.null(ant_xy)) {
      if ((isLongLat)||(isMerc)) {
         ant1 <- ant_xy[1,]
         ant2 <- ant_xy[nrow(ant_xy),]
         ant1[2] <- g1$miny-g1$resy
         ant2[2] <- ant1[2]
         ant_xy <- rbind(ant1,ant_xy,ant2,ant1)
         rownames(ant_xy) <- NULL
         if (FALSE) { ## non-reproducible code for non-author 
            plot(0,0,xlim=range(c(coast_xy[,1],ant_xy[,1]),na.rm=TRUE),
                    ,ylim=range(c(coast_xy[,2],ant_xy[,2]),na.rm=TRUE),type="n")
            polypath(coast_xy[,1],coast_xy[,2],col="red")
            polypath(ant_xy[,1],ant_xy[,2],col="green")
            stop("")
         }
      }
      coast_xy <- rbind(coast_xy[,1:2],c(NA,NA),ant_xy[,1:2])
   }
   if (!.isPackageInUse())
      print(c(detail=detail))
   if (!isDetail) {
      inside <- with(g1,coast_xy[,1]>=minx & coast_xy[,1]<=maxx &
                            coast_xy[,2]>=miny & coast_xy[,2]<=maxy)
      inside <- any(na.omit(unique(inside)))
      if (inside) {
         area <- with(g1,max(maxx-minx,maxy-miny))
        # if (grepl("\\+units=km",g1$crs))
        #    area <- area*1e3
         if (isLongLat)
            area <- area*111
         else
            area <- area/ifelse(grepl("\\+units=km",g1$crs),1,1000)
         if (area<=(-10))
            return(NULL)
         else if (area<=100)
            detail <- "f"
         else if (area<=500)
            detail <- "h"
         else if (area<=3000)
            detail <- "i"
         else
            detail <- "l"
         if (detail!="l") {
            fname <- file.path(fpath,paste0("coast-",detail,".rds"))
            if (file.exists(fname)) {
               if (FALSE) {
                  arglist <- list(...)
                  ind <- .grep("detail",names(arglist))
                  if (length(ind))
                     arglist[[ind]] <- detail
                  else
                     arglist$detail <- detail
                  return(do.call("compose_coastline",arglist))
               }
               else {
                  arglist <- as.list(match.call()) ## try mget(names(match.call())[-1])
                  arglist$detail <- detail
                  return(do.call(as.character(arglist[[1]]),arglist[-1]))
               }
            }
         }
      }
   }
  # ind <- which(coast_xy[,2]<(-68))
  # print(summary(coast_xy[ind,1]))
   shadow <- unname(col2rgb(fill,alpha=TRUE)[4,1])
   options(ursaPngShadow=ifelse(shadow %in% c(0,255),"",fill))
   res <- list(coast_xy=coast_xy,grid=g1,detail=detail,panel=panel,col=col,fill=fill
              ,shadow=shadow,land=land,density=density,angle=angle,lwd=lwd,lty=lty)
   class(res) <- "ursaCoastLine"
   options(ursaPngCoastLine=res)
   res
}
'panel_coastline' <- function(...) {
   if (.skipPlot(TRUE))
      return(NULL)
   arglist <- list(...)
   kwd <- "coast(line)*"
   isWeb <- getOption("ursaPngWebCartography")
   if (!is.logical(isWeb))
      isWeb <- FALSE
   coastline <- .getPrm(arglist,name=paste0("^(",kwd,"|decor)$")
                       ,class=list("integer","logical","ursaCoastLine")[1:2]
                       ,default=TRUE,verbose=FALSE)
   ##~ decor <- .getPrm(arglist,name="^decor$"
                   ##~ ,class=list("integer","logical","ursaCoastLine")[1:2]
                   ##~ ,default=TRUE,verbose=FALSE)
   ##~ coastline <- .getPrm(arglist,name=paste0("^",kwd,"$")
                       ##~ ,class=list("integer","logical","ursaCoastLine")[1:2]
                       ##~ ,default=,verbose=FALSE)
   ##~ str(arglist)
   ##~ print(c(coast=coastline,web=isWeb))
   ##~ q()
   ##~ if (inherits(coastline,"ursaCoastLine")) {
      ##~ obj <- coastline
      ##~ coastline <- TRUE
      ##~ isFound <- TRUE
   ##~ }
   ##~ else
      ##~ isFound <- FALSE
   if (!coastline)
      return(NULL)
   g1 <- .panel_grid() ## session_grid()
  # if (!isFound)
   obj <- .getPrm(arglist,class="ursaCoastLine",default=NULL)
   figure <- getOption("ursaPngFigure")
   if ((!is.logical(coastline))&&(figure!=coastline))
      return(NULL)
   if (is.null(obj)) {
      obj <- getOption("ursaPngCoastLine")
      if (!is.null(obj)&&(any(obj$panel))&&(!(figure %in% obj$panel)))
         return(NULL)
      if ((is.null(obj))||(!identical(g1,obj$grid))) {
         options(ursaPngCoastLine=NULL)
         obj <- compose_coastline(...)
      }
      else {
         detail <- .getPrm(arglist,name="detail",kwd=kwd,default=obj$detail)
         if (!identical(detail,obj$detail)) {
            options(ursaPngCoastLine=NULL)
            obj <- compose_coastline(...)
         }
      }
   }
   if ((any(obj$panel))&&(!(figure %in% obj$panel)))
      return(invisible(NULL))
   if (is.null(obj$coast_xy)) {
      return(invisible(NULL))
   }
   if (!FALSE) {
      obj$col <- .getPrm(arglist,name="col",kwd=kwd,default=obj$col)
      obj$fill <- .getPrm(arglist,name="fill",kwd=kwd,default=obj$fill)
      obj$density <- .getPrm(arglist,name="density",kwd=kwd,default=obj$density)
      obj$angle <- .getPrm(arglist,name="angle",kwd=kwd,default=obj$angle)
      obj$land <- .getPrm(arglist,name="land",kwd=kwd,default=obj$land)
      obj$lwd <- .getPrm(arglist,name="lwd",kwd=kwd,default=obj$lwd)
      obj$lty <- .getPrm(arglist,name="lty",kwd=kwd,default=obj$lty)
   }
   verbose <- .getPrm(arglist,name="verbose",kwd=kwd,default=FALSE)
   .panel_coastline(obj,verbose=verbose)
}
'.panel_coastline' <- function(obj,verbose=FALSE) {
   with(obj,{
      shadow <- unname(col2rgb(fill,alpha=TRUE)[4,1])
      if (verbose)
         str(list(col=col,fill=fill,shadow=shadow,detail=detail
                 ,density=density,angle=angle,land=land,lwd=lwd,lty=lty))
      if ((TRUE)&&(shadow==0)|| ## 20171214 changed 'shadow!=255'
          ((!is.na(angle[1]))&&(!is.na(density[1])))) ## more quick
      {
        # op <- par(usr=par()$usr-c(0,125000,0,125000))
        # print(par()$usr)
         if ((is.na(angle[1]))||(is.na(density[1]))) {
            if (inherits(coast_xy,"SpatialPolygonsDataFrame"))
               plot(coast_xy,border=col,col=fill,lwd=lwd,add=TRUE)
            else {
               polygon(coast_xy[,1],coast_xy[,2],border=col,col=fill,lwd=lwd)
            }
         }
         else {
            for (an in angle) {
              # print(str(list(angle=an,border=col,col=fill,density=density,lwd=lwd)))
               polygon(coast_xy[,1],coast_xy[,2],border=col,col=fill
                      ,density=density,angle=an,lwd=lwd)
            }
         }
        # par(op)
      }
      else
      {
         if (inherits(coast_xy,"SpatialPolygonsDataFrame"))
            plot(coast_xy,border=col,col=fill,lwd=lwd
                ,usePolypath=TRUE,rule=c("winding","evenodd")[2],add=TRUE)
         else if (!all(is.na(c(coast_xy)))) {
            if (land)
            {
               g1 <- session_grid()
               x <- with(g1,c(minx,minx,maxx,maxx,minx)+c(-1,-1,1,1,-1)*resx)
               y <- with(g1,c(miny,maxy,maxy,miny,miny)+c(-1,1,1,-1,-1)*resy)
               coast_xy <- rbind(cbind(x,y),c(NA,NA),coast_xy)
            }
           ## ?polypath: Hatched shading (as implemented for polygon()) is not (currently) supported.
           ## if semi-opacity|trasparency them 'polygon' else fill is transparent
            ret <- try(polypath(coast_xy[,1],coast_xy[,2],border=col,col=fill
                    ,rule=c("winding","evenodd")[2],lwd=lwd)) ##,density=15??
            if (inherits(ret,"try-error"))
               cat("Plotting coastline...",ret)
         }
      }
   })
   invisible(NULL)
}

'update_coastline' <- function(merge=TRUE) {
   missedSF <- !requireNamespace("sf",quietly=TRUE) 
   if (missedSF)
      stop("Suggested package 'sf' is required for this operation")
  # missedLW <- !requireNamespace("lwgeom",quietly=TRUE)
  # if (missedLW)
  #    stop("Suggested package 'lwgeom' is required for this operation")
   dpath <- getOption("ursaRequisite")
   ftemp <- tempfile(tmpdir=dpath)
   opW <- options(warn=-1)
   res <- .try(writeLines("1",ftemp))
   options(opW)
   if (res)
      file.remove(ftemp)
   else
      stop("Unable to update 'requisite' directory for package")
   verbose <- !.isPackageInUse()
   if (!verbose)
      merge <- TRUE
   if (FALSE) {
      toUnloadMethods <- !("methods" %in% .loaded())
      .require("methods",quietly=!verbose)
   }
   else
      toUnloadMethods <- FALSE
  # src <- "http://data.openstreetmapdata.com/simplified-land-polygons-complete-3857.zip" ## (depredated)
   src <- "https://osmdata.openstreetmap.de/download/simplified-land-polygons-complete-3857.zip"
   dst <- .ursaCacheDownload(src,mode="wb",quiet=FALSE)
   list1 <- unzip(dst,exdir=tempdir())
   a <- sf::st_read(list1[.grep("\\.shp$",basename(list1))],quiet=TRUE)
   file.remove(list1)
   if (verbose)
      n <- nrow(a)
   aG <- sf::st_geometry(a)
  # sf::st_geometry(a) <- NULL
  # a <- sf::st_sf(data.frame(FID=as.integer(a$FID)),geometry=aG)
   spatial_data(a) <- data.frame(FID=as.integer(a$FID))
   .elapsedTime("reading - done")
   FID <- a$FID
   prj1 <- spatial_crs(a)
   lon0 <- 120
  # prj1 <- spatial_proj4(a) ## commented this 'x'-line after 'x-2' line assign 
   prj2 <- .gsub("(^.+)(\\s\\+lon_0=)(\\d+(\\.\\d+)*)(\\s.+$)"
                ,paste0("\\1\\2",lon0,"\\5"),prj1)
   if (merge) {
      aG <- sf::st_transform(aG,crs=4326)
      cross180 <- NULL
      if (verbose)
         pb <- ursaProgressBar(min=0,max=n,tail=TRUE)
      for (i in sample(seq_along(FID))) {
         fid <- FID[i]
         bG <- lapply(aG[[i]],function(x) which(abs(x[,1])>180-1e-11))
         if (verbose)
            setUrsaProgressBar(pb)
         if (!sum(sapply(bG,length)))
            next
         cross180 <- c(cross180,fid)
      }
      if (verbose)
         close(pb)
      a180 <- subset(a,FID %in% cross180)
      sf::st_agr(a180) <- "constant"
     # spatial_write(a180,"a180.sqlite")
      c180 <- sf::st_transform(sf::st_centroid(a180),crs=4326)
      g180 <- do.call("rbind",lapply(lapply(sf::st_geometry(c180),unclass)
                                    ,matrix,ncol=2))
      sf::st_geometry(c180) <- NULL
      c180 <- data.frame(c180,lon=g180[,1],lat=g180[,2],area=spatial_area(a180)*1e-6)
      if (verbose) {
         print(c180)
         print(nrow(c180)) 
      }
      p1 <- c180$FID[c180$lat>52.0 & c180$lat<67.0] # Chukotka
      p2 <- c180$FID[c180$lat>71.0 & c180$lat<71.5] # Wrangel
      p3 <- c180$FID[c180$lat>(-17.0) & c180$lat<(-16.7)] # Taveuni (south)
      p4 <- c180$FID[c180$lat>(-16.55) & c180$lat<(-16.45)] # Rabi (center)
      p5 <- c180$FID[c180$lat>(-16.65) & c180$lat<(-16.55) |
                     c180$lat>(-16.3) & c180$lat<(-16.0)] # Labasa (top MANUAL)
      pair <- list(Chukotka=p1,Wrangel=p2,Taveuni_S=p3,Rabi_M=p4,Labasa_N=p5)
      pname <- names(pair)
      if (verbose) {
         print(pair)
         if (FALSE) {
            a180 <- subset(a180,FID %in% c180$FID[abs(c180$lat)<=20])
            if (FALSE)
            spatial_write(sf::st_transform(a180,prj2),"cross180.sqlite")
         }
      }
      if ((!.isPackageInUse())&&(devel <- F)) {
         a5 <- subset(a,FID %in% pair[[2]])
         message("writing `wrangel_fail.sqlite`")
         spatial_write(spatial_transform(a5,3571),"wrangel_fail.sqlite")
         q()
         ##~ a5 <- spatial_union(a5)
         ##~ str(a5)
         ##~ glance(a5,resetProj=TRUE)
         ##~ q()
      }
      a180 <- vector("list",length(pair))
      if (devel3 <- TRUE) {
        # pair <- pair[2]
         for (i in seq_along(pair)) {
            fid5 <- pair[[i]]
            a180[[i]] <- subset(a,FID %in% fid5)
            a0 <- spatialize(a180[[i]],resetProj=TRUE)
           # a0 <- spatialize(subset(a,FID %in% fid5),style="merc",lon0=180)
           # print(spatial_crs(a0))
           # spatial_write(a0,"wrangel_fail.sqlite")
           # str(a0)
           # print(spatial_area(a0))
            a1 <- spatial_union(a0)
            a1 <- sf::st_cast(a1,"MULTIPOLYGON")
           # str(a1)
           # cat("------------\n")
           # str(spatial_geometry(a1)[[1]])
            ng <- length(spatial_geometry(a1)[[1]])
           # str(ng)
           # cat("------------\n")
            if (ng>1)
               next
            spatial_data(a1) <- data.frame(FID=65000L+i)
            a180[[i]] <- sf::st_transform(a1,prj1)
           # print(spatial_area(a1))
           # print(spatial_count(a2))
           # glance(a2,resetGrid=TRUE,decor=FALSE)
         }
        # cat("===============\n")
      }
      for (i in seq_along(pair)) {
         if (spatial_count(a180[[i]])==1)
            next
         a0 <- a180[[i]]
        # fid5 <- pair[[i]]
        # a0 <- spatial_transform(subset(a,FID %in% fid5),3571)
         a0 <- spatial_transform(a0,3571)
         xy0 <- spatial_coordinates(a0)
         if (devel6 <- F) {
            if (i==4) {
               str(xy0)
               p <- unlist(xy0,recursive=FALSE)
               str(p)
               saveRDS(p,"tmp2.rds")
               q()
            }
         }
         xy1 <- head(xy0[[1]][[1]],-1)
         xy2 <- head(xy0[[2]][[1]],-1)
         d1 <- .dist2(xy1,xy2,verbose=FALSE,summarize=FALSE)
         order1 <- order(d1$dist,xy1[,1])[1:2]
         ind1 <- d1$ind[order1]
         d2 <- .dist2(xy2,xy1,verbose=FALSE,summarize=FALSE)
         order2 <- order(d2$dist,xy2[,1])[1:2]
         ind2 <- d2$ind[order2]
         if (ind2[1]>ind2[2]) { ## 10 9
           # print("ind2A")
            xy1 <- xy1[c(ind2[1]:nrow(xy1),1L:ind2[2]),]
         }
         else if (ind2[1]>1) { ## 9 10
           # print("ind2B")
            xy1 <- xy1[c(ind2[1]:1L,nrow(xy1):ind2[2]),]
         }
         else if (ind2[2]>2) {
           # print("ind2C")
         }
         else {
            xy1 <- xy1[c(tail(seq(nrow(xy1)),-1),head(seq(nrow(xy1)),1)),]
           # print("ind2D")
         }
         if (ind1[1]>ind1[2]) { ## 10 9
           # print("ind1A")
            xy2 <- xy2[c(ind1[1]:nrow(xy2),1L:ind1[2]),]
         }
         else if (ind1[1]>1) { ## 9 10
           # print("ind1B")
            xy2 <- xy2[c(ind1[1]:1L,nrow(xy2):ind1[2]),]
         }
         else if (ind1[2]>2) {
           # print("ind1C")
         }
         else {
            xy2 <- xy2[c(tail(seq(nrow(xy2)),-1),head(seq(nrow(xy2)),1)),]
           # print("ind1D")
         }
         ind3 <- .dist2(xy1[range(seq(nrow(xy1))),],xy2[range(seq(nrow(xy2))),]
                       ,verbose=FALSE,summarize=FALSE)$ind
         xy7 <- rbind(if (T) xy1 else head(xy1,-1)
                     ,if (ind3[1]>ind3[2]) xy2
                      else xy2[order(seq(nrow(xy2)),decreasing=TRUE),]
                     )
         xy7 <- xy7[c(which(!duplicated(xy7))),]
         xy7 <- xy7[c(seq(nrow(xy7)),1),]
         if (mean(xy1[,1])<0) {
            xy0[[1]][[1]] <- xy7
            xy0[2] <- NULL
         }
         else {
            xy0[[2]][[1]] <- xy7
            xy0[1] <- NULL
         }
         a7 <- sf::st_sfc(GOMER=sf::st_polygon(xy0[[1]]),crs=spatial_crs(a0))
         spatial_data(a7) <- data.frame(FID=65000L+i)
         a180[[i]] <- sf::st_transform(a7,prj1)
      }
      if (devel10 <- F) {
         a2 <- do.call(spatial_bind,a180)
         g1 <- session_grid(c(360,400))
         compose_open(length(pair),fix=TRUE)
         for (i in seq_along(pair)) {
            a2 <- spatialize(a180[[i]],resetProj=TRUE)
            g3 <- consistent_grid(spatial_grid(a2),ref=g1)
            session_grid(g3)
            panel_new("white")
            panel_plot(a2,lwd=1)
         }
         compose_close()
         q()
      }
      .elapsedTime("spliting - done")
      if (F) {
         a180 <- spatial_bind(a180)
         a180 <- spatialize(a180,style="merc",lon0=180)
         str(a180)
         spatial_write(a180,"wrangel_fail.sqlite")
         q()
      }
      a <- list(subset(a,!(FID %in% unlist(pair))))
      a <- do.call("rbind",c(a,a180))
      .elapsedTime("merging - done")
     # spatial_write(do.call(spatial_bind,a180),"tmp1.sqlite")
   }
   if (devel2 <- FALSE) {
      sf::sf_use_s2(TRUE)
      print("D")
      a3 <- sf::st_area(a)
      print("A")
      str(a3)
      str(attr(a3,"units"))
     # u <- attr(a3,"units")$numerator
      a1 <- sf::st_transform(a,crs=4326)
      print("B")
      a2 <- sf::st_area(a1)
      print("C")
      str(a2)
      q()
   }
   sf::sf_use_s2(FALSE) ## ++ 20220125 
   pArea <- sf::st_area(sf::st_transform(a,crs=4326))
  # pArea <- as.numeric(units::set_units(pArea,km^2))
   u <- attr(pArea,"units")$numerator
   m <- rep(1,length(u))
   for (i in seq_along(u)) {
      if (u[i]=="m")
         m[i] <- 1/1000
      else
         stop(u[i])
   }
   pArea <- as.numeric(pArea)*prod(m)
   .elapsedTime("area calculation - done")
   res <- sapply(c("l","i","h","f"),function(x) .update_coastline(a,pArea,merge,x))
   if ((toUnloadMethods)&&("package:methods" %in% search())) {
      detach("package:methods",unload=FALSE) 
   }
   res
}
'.update_coastline' <- function(a,pArea,merge,detail=c("l","i","h","f")) {
   detail <- match.arg(detail)
   verbose <- !.isPackageInUse()
   thL <- switch(detail,l=12,i=3.5,h=1,f=0,stop("unable set length"))
   thA <- (0.5*thL)^2
   if (thA>0) {
      ind <- which(pArea<thA)
      if (length(ind))
         a <- a[-ind,]
   }
   if (thL>0) {
      a <- sf::st_simplify(a,dTolerance=thL*1e3,preserveTopology=TRUE)
   }
   .elapsedTime(paste0(detail,": simplifying - done"))
   a <- sf::st_transform(a,crs=4326)
   g1 <- sf::st_geometry(a)
   k <- 0
   for (i in seq_along(g1)) {
      g2 <- g1[[i]]
      for (j in seq_along(g2)) {
         g3 <- g2[[j]]
         if (!is.list(g3))
            g3 <- list(g3)
         for (j in seq_along(g3)) {
            k <- k+1L
         }
      }
   }
   xy <- vector("list",k)
   k <- 0
   for (i in seq_along(g1)) {
      g2 <- g1[[i]]
      for (j in seq_along(g2)) {
         g3 <- g2[[j]]
         if (!is.list(g3))
            g3 <- list(g3)
         for (m in seq_along(g3)) {
            g4 <- g3[[m]]
            isATA <- try(as.integer(any(g4[,2]<(-84.9))))
            if (isATA) {
               a$FID[i] <- -abs(a$FID[i])
               if (verbose)
                  print(c('Antarctida FID'=a$FID[i]))  # Antarctida 43705
            }
            k <- k+1
            if (isATA) {
               indX <- which(abs(g4[,1])>180-1e-3)
               indY <- which(abs(g4[,2])>85-1e-3)
               ind2 <- na.omit(match(indY,indX))
               if (length(ind2)) {
                  ind2 <- ind2[1L]-1L
                  g4 <- head(g4,-1L)
                  g4 <- unname(rbind(tail(g4,-ind2),head(g4,ind2)))
                  g4 <- rbind(g4,head(g4,1L))
               }
            }
            else if (any(g4[,1]>(+175)) && any(g4[,1]<(-175))) {
               ind3 <- which(g4[,1]>=(-180) & g4[,1]<=(-120))
               g4[ind3,1] <- g4[ind3,1]+360
            }
            xy[[k]] <- rbind(cbind(g4,isATA),c(NA,NA,NA))
         }
      }
   }
   ind <- which(sapply(xy,function(x) is.null(x)))
   if (length(ind))
      xy <- xy[-ind]
   xy <- do.call("rbind",lapply(xy,I))
   xy <- xy[-nrow(xy),]
   indA <- which(xy[,3]>0)
   colnames(xy) <- c("lon","lat","spole")
   if (TRUE)
      xy <- xy[,1:2]
   if (length(indA))
      attr(xy,"antarctic") <- indA
   indP <- which(xy[,2]<(-84.99))
   if (length(indP))
      attr(xy,"south_pole") <- indP
   .elapsedTime(paste0(detail,": coercing - done"))
   if (merge)
      saveRDS(xy,file.path(getOption("ursaRequisite"),paste0("coast-",detail,".rds"))
             ,version=2) ## Such files are only readable in R >= 3.5.0.
   if (!.isPackageInUse())
      spatial_write(a,paste0(paste0("coast-",detail,ifelse(merge,"","180")),".sqlite"))
   0L
}
