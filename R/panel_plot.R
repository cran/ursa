## ?formals
'panel_plot' <- function(obj,...) {
   if (.skipPlot(TRUE))
      return(NULL)
   geoType <- ""
   isSP <- FALSE
   isSF <- FALSE
   arglist <- as.list(match.call()) ## try mget(names(match.call())[-1])
   isLang <- is.language(arglist[["obj"]])
   if (isLang)
      oname <- as.character(arglist["obj"])
   else
      oname <- "*undetermed*"
  # str(lapply(arglist,class))
   arglist2 <- list(...) ## remove dupe of 'add=TRUE'
   ct <- NULL
   if (inherits(obj,c("raster"))) {
      ret <- with(.panel_grid(),rasterImage(as.raster(obj),minx,miny,maxx,maxy,...))
   }
   else if (is.character(obj)) {
      if (.lgrep("\\.(shp(\\.zip)*|(geojson|sqlite|gpkg)(\\.(gz|bz2))*)$",obj)) {
         if (FALSE) { ## 20171216 deprecated
            op <- options(warn=0)
           # requireNamespace("rgdal",quietly=.isPackageInUse())
            a <- .shp.read(obj)
           # a <- spTransform(a,session_grid()$crs)
           # ret <- .panel_plot(a,add=TRUE,...)
            ret <- sp::plot(a,add=TRUE,...)
            options(op)
         }
         else {
            ret <- panel_plot(spatialize(obj),...) ## RECURSIVE
         }
      }
      else if ((length(obj)==1)&&(envi_exists(obj))) {
         ret <- panel_raster(read_envi(obj),...)
      }
      else if ((length(obj)==1)&&(file.exists(obj))) {
         ret <- panel_raster(read_gdal(obj),...)
      }
      else if ((getOption("ursaPngScale")==1)&&
             (.isMerc()>0)&&
             ((TRUE)||(.lgrep(obj,.tileService())))) {
         ret <- panel_raster(obj,...)
      }
      else
         return(invisible(NULL))
   }
   else if (is.ursa(obj)) {
      ct <- panel_raster(obj,...)
   }
   else if (is_spatial(obj)) {
      oprj <- spatial_crs(obj)
      sprj <- .panel_crs() # session_crs()
      if (!identical(.crsBeauty(oprj,extended=TRUE),.crsBeauty(sprj,extended=TRUE))) {
         if (F & !.isPackageInUse()) {
            cat("Please check comparison of WKT strings\n")
         }
         if ((is.list(oprj))&&("input" %in% names(oprj)))
            oprj <- oprj[["input"]]
         if ((is.list(sprj))&&("input" %in% names(sprj)))
            sprj <- sprj[["input"]]
         if (nchar(sprj)>2) {
            oprj2 <- .gsub("\\+wktext\\s","",oprj)
            sprj2 <- .gsub("\\+wktext\\s","",sprj)
            oprj2 <- .gsub("(^\\s|\\s$)","",oprj2)
            sprj2 <- .gsub("(^\\s|\\s$)","",sprj2)
            if (!identical(oprj2,sprj2)) {
               obj <- spatial_transform(obj,sprj)
            }
         }
      }
      geoType <- spatial_geotype(obj)
      onlyGeometry <- is.null(spatial_data(obj))
      if ((FALSE)&&(onlyGeometry)) {
        # ret <- plot(obj,add=TRUE,...)
         ret <- .tryE(do.call(".panel_plot",c(obj,add=TRUE)))
      }
      else if (TRUE) { ## is advanced condition required here?
        # ret <- plot(sf::st_geometry(obj),...)
        # opE <- options(show.error.messages=TRUE)
        # ret <- .tryE(.panel_plot(sf::st_geometry(obj),add=TRUE,...))
        # ret <- .tryE(.panel_plot(obj,add=TRUE,...))
        # message("===========")
        # str(arglist2)
        # str(.lgrep("^(plot\\.)*col(o(u)*r)*$",names(arglist2)))
        # message("===========")
         if ((TRUE)&&(.lgrep("^col(o(u)*r)*$",names(arglist2)))) {
           # stop("I")
            if (is.numeric(col)) {
              # stop("G")
               ct <- colorize(col)
               col <- ct$colortable[ct$index]
            }
            else if ((is.character(arglist2$col))&&(length(spatial_fields(obj)))) {
              # stop("H")
               if (!anyNA(match(arglist2$col,spatial_fields(obj)))) {
                 # stop("A")
                  ct <- colorize(spatial_data(obj,subset=col,drop=TRUE))
                  col <- ct$colortable[ct$index]
               }
               else if (length(spatial_fields(obj))==-1) {
                 # stop("B")
                  if (.is.colortable(arglist2$col)) {
                    # stop("B1")
                     val <- spatial_data(obj,drop=TRUE)
                     ind <- match(names(arglist2$col),val)
                     if ((!anyNA(ind))&&(all(diff(ind)==1))) {
                       # stop("B1a")
                        col <- unname(unclass(arglist2$col))
                        ct <- arglist2$col
                       # ct <- colorize(val,pal=arglist2$col)
                     }
                     else {
                       # stop("B1b")
                        if (length(val)==length(arglist2$col)) {
                           ct <- arglist2$col
                           col <- unname(unclass(arglist2$col))
                        }
                        else {
                           ct <- colorize(val,colortable=arglist2$col)
                           col <- ct$colortable[ct$index]
                        }
                     }
                  }
                  else {
                    # stop("B2")
                     col <- arglist2$col
                  }
               }
               else if (is.character(col)) {
                 # stop("C")
                  col <- arglist2$col
               }
               else if ((.is.colortable(arglist2$col))&&
                       (length(spatial_fields(obj))==1)) {
                 # stop("D")
                  ct <- arglist2$col
                  val <- spatial_data(obj)[,1,drop=TRUE]
                  if (length(ct)==length(val)) {
                    # stop("D1")
                     col <- unname(ct)
                  }
                  else {
                    # stop("D2")
                    # str(val)
                    # str(ct)
                    # print(table(val))
                     if (!FALSE) { ## -- 20230529
                        col <- colorize(val,colortable=ct)
                        col <- unname(col$colortable[col$index])
                     }
                     else { ## ++ 20230529
                        col <- palettize(val,colortable=ct)
                     }
                  }
                 # str(col)
                 # print(table(col))
               }
               else {
                  ##~ stop("E")
                  col <- NULL
               }
            }
            else {
              # stop("F")
               col <- arglist2$col
               if (length(spatial_fields(obj))==1) {
                  if (.is.colortable(arglist2$col)) {
                     ct <- ursa_colortable(arglist2$col)
                     ind <- match(obj[[1]],names(ct))
                     if (all(!anyNA(ind)))
                        col <- unname(ct)[ind]
                  }
               }
            }
            alpha <- .getPrm(arglist2,"alpha",default=1)
            if (alpha<1) {
               val <- t(grDevices::col2rgb(col,alpha=TRUE))
               col <- grDevices::rgb(val[,1],val[,2],val[,3],val[,4]*alpha,max=255)
            }
            if (!is.null(col))
               arglist2$col <- unclass(col)
           # str(arglist2)
         }
         else if (length(spatial_fields(obj))==1) {
           # stop("II")
            if ((!is.null(arglist2$pal))&&(inherits(arglist2$pal,"ursaColorTable"))) {
              # stop("II1")
               if (isTRUE(is.numeric(arglist2$alpha))) {
                  pname <- names(arglist2$pal)
                  alpha <- sprintf("%02X",.round(as.hexmode(gsub(".*(\\w{2}$)","\\1"
                            ,arglist2$pal))*arglist2$alpha))
                  arglist2$pal <- paste0(gsub("(\\w{2}$)","",arglist2$pal),alpha)
                  names(arglist2$pal) <- pname
               }
               ind <- match(obj[[1]],names(arglist2$pal))
               if (!anyNA(ind))
                  arglist2$col <- arglist2$pal[ind]
               else {
                  ct <- do.call("colorize",c(list(obj[[1]],arglist2)))
                  arglist2$col <- ct$colortable[ct$index]
               }
            }
            else if (is_ursa(attr(obj[[1]],"colortable"),"colortable")) {
              # stop("II2")
               arglist2$pal <- attr(obj[[1]],"colortable")
               ind <- match(obj[[1]],names(arglist2$pal))
               arglist2$col <- unclass(unname(arglist2$pal))[ind]
            }
            else {
              # stop("II3")
              # ct <- colorize(obj[[1]])
               ct <- do.call("colorize",c(list(obj[[1]]),arglist2))
               arglist2$pal <- ct$colortable ## ++ 20210520
               arglist2$col <- unclass(unname(ct$colortable))[ct$index] ## -- 20210520
            }
         }
         else {
            arglist2$col <- ifelse(is_spatial_lines(obj),"#0000005F"
                                  ,ifelse(T,ursa_colortable(colorize("",alpha=0.2))
                                           ,"transparent"))
         }
         if (.lgrep("point",geoType)) {
            if (F & !.isPackageInUse())
               message("'ursa'-dev: test 'scattermode' ('https://github.com/exaexa/scattermore') for points plot")
            if (!.lgrep("pch",names(arglist2))) {
               arglist2$pch <- 21
            }
            if (!.lgrep("bg",names(arglist2))) {
               arglist2$bg <- arglist2$col
               if ((.lgrep("pch",names(arglist2)))&&
                   (arglist2$pch %in% c(21,22,23,24,25))) { ## ?pch
                  op <- median(col2rgb(arglist2$bg,alpha=T)["alpha",])
                  if (!is.null(arglist2$border)) {
                     bg <- c(col2rgb(arglist2$border))
                     arglist2$col <- rgb(bg[1],bg[2],bg[3],op,maxColorValue=255)
                  }
                  else {
                     arglist2$col <- rgb(0,0,0,op,maxColorValue=255)
                    # arglist2$bg <- "transparent"
                  }
               }
            }
            if (!.lgrep("cex",names(arglist2))) {
               arglist2$cex <- 1.2
            }
            if (!.lgrep("lwd",names(arglist2))) {
               arglist2$lwd <- 0.5
            }
            if (!.lgrep("col",names(arglist2)))
               arglist2$col <- "black"
         }
         else if (.lgrep("lines",geoType)) {
            if (!.lgrep("lwd",names(arglist2))) {
               arglist2$lwd <- 1 ## 20180309 was 'lwd <- 3'
            }
            if (F & length(ind <- .grep("^border",names(arglist2)))) { ## -- 20220515
               arglist2$col <- arglist2[[ind]]
            }
         }
         if (.lgrep("polygon",geoType)) {
            if (!.lgrep("lwd",names(arglist2))) {
               arglist2$lwd <- 0.1
            }
            else {
               lwd <- arglist2$lwd
               if ((is.character(lwd))&&(.lgrep("(km|m)$",lwd))) {
                  if (.lgrep("km$",lwd))
                     mul <- 1e3
                  else if (.lgrep("m$",lwd))
                     mul <- 1
                  else
                     mul <- 1
                  lwd <- mul*as.numeric(gsub("\\D","",lwd))
                  print("LWD")
                  cell <- session_cellsize()
                  sc <- getOption("ursaPngScale")
                  dpi <- getOption("ursaPngDpi")/96
                  px <- cell/sc
                  res <- lwd/px/dpi
                 # print(data.frame(lwd=lwd,cell=cell,sc=sc,px=px,res=res))
                  arglist2$lwd <- res
               }
               else if ((is.numeric(lwd))&&(lwd<=0))
                  arglist2$lwd <- 1e-6
               else if ((is.na(lwd))||(is.null(lwd)))
                  arglist2$lwd <- 1e-6
            }
           # str(arglist2)
           # str(ct)
         }
        # str(c(obj=list(spatial_geometry(obj)),add=TRUE,arglist2))
         if (isFALSE(arglist2$border))
            arglist2$border <- NULL
         if ((TRUE)&&(is_spatial_lines(obj))&&("border" %in% names(arglist2))) {
            if (isTRUE(arglist2$border)) {
               gscale <- median(colSums(col2rgb(arglist2$col)*c(0.30,0.59,0.11)))
               arglist2$border <- ifelse(gscale>160,"#000000A0","#FFFFFFA0")
            }
            arglist3 <- list(lwd=1.25,col=arglist2$border)
            if ("lwd" %in% names(arglist2)) {
               if (length(arglist2$lwd)==1)
                  arglist3$lwd <- max(arglist2$lwd*1.3,arglist2$lwd+1.5)
               else if (length(arglist2$lwd)==2) {
                  arglist3$lwd <- max(arglist2$lwd)
                  arglist2$lwd <- min(arglist2$lwd)
               }
            }
            ret3 <- .tryE(do.call(".panel_plot"
                         ,c(obj=list(spatial_geometry(obj)),add=TRUE,arglist3)))
         }
         ret <- .tryE(do.call(".panel_plot"
                     ,c(obj=list(spatial_geometry(obj)),add=TRUE,arglist2)))
        # str(arglist2)
        # str(ret)
         arglist2$type <- spatial_geotype(obj)
        # options(opE)
      }
      isSF <- .isSF(obj)
      isSP <- .isSP(obj)
   }
   else if (F & is.list(obj)) {
      retname <- names(obj)[1]
      ret <- try(.panel_plot(obj[[1]],add=TRUE,...))
      str(ret)
      q()
   }
   else {
      ret <- try(.panel_plot(obj,add=TRUE,...))
      if (inherits(ret,"try-error")) {
         opW <- options(warn=1)
         warning(paste("Unable to call 'plot' method for class"
                      ,.sQuote(class(obj))
                      ,"\nIt seems that package 'methods' is required."))
         options(opW)
         ret <- NULL
      }
   }
   aname <- names(arglist2)
  # str(arglist2)
   if (is_spatial_points(obj)) {
      ret <- .legend.skeleton()
      ret$name <- oname
      ret$type <- "POINT"
      ret$col <- arglist2$col
      ret$pch <- arglist2$pch
      ret$pt.cex <- arglist2$cex
      ret$pt.lwd <- arglist2$lwd
      ret$pt.bg <- arglist2$bg
      ret$border <- "transparent"
   }
   else {
      ret <- .legend.skeleton()
      ret$name <- oname
      rname <- names(ret)
      if (.lgrep("polygon",geoType)) { # 20171215 -- 'if (geoType %in% c("POLYGON","MULTIPOLYGON"))'
         ret$pch <- NA
        # ret$cex <- 3
      }
      for (i in seq_along(rname)) {
         if (is.na(j <- match(rname[i],aname)))
            next
         ret[[i]] <- arglist2[[j]]
      }
      if ((TRUE)&&(.lgrep("line",geoType))) {
         ##~ str(ret)
         ##~ str(arglist)
         ##~ str(arglist2)
         ##~ q()
         if (!is.list(ret$col)) {
            .col <- ret$col
            ret$col <- ret$border
            ret$border <- .col
         }
      }
      if ((TRUE)&&(.lgrep("point",geoType))&&(!is.null(ct))) {
         ret$col <- ct$colortable
      }
      if ((isTRUE(all(ret$bg!="transpareny")))&&(isTRUE(all(ret$border=="transparent")))) {
         ret$fill <- ret$bg
      }
      if ((TRUE)&&(.lgrep("polygon",geoType))) {
         if (F & is_ursa(ret$col,"colortable")) {
           # rname <- unique(names(ret$col))
            ret$fill <- unique(ret$col)
            ret$name <- unique(names(ret$col))
           # q()
         }
         else
            ret$fill <- ret$col
         if (is.null(arglist2$col))
            ret$col <- NA
         if (is.null(arglist2$lwd))
            ret$lwd <- NA
         if (is.null(arglist2$lty))
            ret$lty <- NA
      }
   }
   if ((is_spatial(obj))&&(isLang)&&(length(spatial_colnames(obj))>0)) {
      ##~ sink("C:/tmp/res1.out")
      ##~ str(arglist)
      ##~ str(obj)
      ##~ str(eval.parent(arglist[["obj"]]))
      ##~ str(eval(arglist[["obj"]]))
      ##~ sink()
     # ret$name <- spatial_data(eval(arglist[["obj"]]))[[1]]
      if (isTRUE(getOption("ursaNoticeMatchCall")))
         message('panel_plot: try `mget(names(match.call())[-1])` instead of `as.list(match.call())`')
      ret$name <- spatial_data(eval.parent(arglist[["obj"]]))[[1]]
     # if (length(unique(ret$name))==length(ret$pt.bg))
     #    ret$name <- unique(ret$name)
   }
   if (FALSE) {
      if (length(geoType)>1) {
         print(geoType)
        # spatial_write(obj,"res1.sqlite")
      }
   }
  # str(ret)
   if (!is.null(ct)) {
      ct <- ursa_colortable(ct)
      ret$name <- names(ct)
      if (is_spatial_polygons(obj))
         ret$fill <- as.character(ct)
      else if (is_spatial_points(obj)) {
         ret$pt.bg <- as.character(ct)
      }
   }
  # class(ret) <- "ursaLegend"
   if (any(nchar(geoType)>0)) {
      opR <- getOption("ursaPngLegend")
      options(ursaPngLegend=if (is.null(opR)) list(ret) else c(opR,list(ret)))
   }
  # if (!is.null(ct))
  #    return(invisible(ct))
   invisible(list(ret))
}
'panel_box' <- function(...){
   if (.skipPlot(FALSE))
      return(NULL)
   bg <- sum(c(col2rgb(getOption("ursaPngBackground")))*c(0.30,0.59,0.11))
   if (!length(list(...))) {
      box(lwd=0.5,col=ifelse(bg<128,"#FFFFFF7F","#0000007F"))
   }
   else
      box(...)
}
'panel_lines' <- function(...){
   if (.skipPlot(TRUE))
      return(NULL)
   lines(...)
}
'panel_points' <- function(...){
   if (.skipPlot(TRUE))
      return(NULL)
   points(...)
}
'panel_text' <- function(...){
   if (.skipPlot(TRUE))
      return(NULL)
   text(...)
}
'panel_polygon' <- function(...){
   if (.skipPlot(TRUE))
      return(NULL)
   polygon(...)
}
'panel_abline' <- function(...){
   if (.skipPlot(TRUE))
      return(NULL)
   abline(...)
}
'panel_segments' <- function(...){
   if (.skipPlot(TRUE))
      return(NULL)
   segments(...)
}
'.zzz.panel_plot.--20171115' <- function(obj,...){
   if (.skipPlot(TRUE))
      return(NULL)
   if (is.null(obj))
      return(obj)
   plot(obj,...)
}
'.panel_plot' <- function(obj,...) {
   if (.skipPlot(TRUE))
      return(NULL)
   if (is.null(obj))
      return(obj)
   arglist <- list(...)
  # str(arglist)
   arglist <- lapply(arglist,function(x1) {
      if (identical(c("index","colortable"),names(x1)))
         return(unclass(unname(x1$colortable))[x1$index])
      x1
   })
   if ((TRUE)&&(.isSF(obj))&&(.lgrep("(dens|angl)",names(arglist)))) {
     # arglist$add <- NULL
      if (!.isPackageInUse())
         message("'sf' doesn't deal with fill patterns; converted to 'Spatial'")
      if (TRUE)
         obj <- sf::as_Spatial(obj)
      else if (FALSE) {
         arglist$add <- NULL
         ret <- lapply(obj,function(x1) {
            do.call("polygon",c(unclass(x1),arglist))
         })
      }
   }
   pkg <- attr(class(obj),"package")
   opW <- NULL
   if (is.character(pkg)) {
      if (pkg=="sp")
         plot <- sp::plot ## unerror "cannot coerce type 'S4' to vector of type 'double'"
         opW <- options(warn=-1) ## release 'warn=-1': no warnings
   }
   ##~ message("-----------------")
   ##~ str(.panel_grid())
   ##~ str(par())
   ##~ str(obj)
   ##~ str(arglist)
   ##~ message("-----------------")
   ret <- do.call("plot",c(list(obj),arglist))
   if (!is.null(opW))
      options(opW)
   ret
}
'.legend.skeleton' <- function()
{
   leg <- list(name="legend item",type="default"
              ,col="transparent",border="transparent",lty=NULL,lwd=NULL,pch=0,cex=NA
              ,fill="transparent",bg="transparent",density=NA,angle=45)
   class(leg) <- "ursaLegend"
   leg
}
