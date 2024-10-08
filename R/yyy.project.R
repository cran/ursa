'.project' <- function(xy,proj,inv=FALSE,verbose=FALSE) {
   on.exit(NULL)
  # print("ENTERED IN"); on.exit(print("ENTERED OUT"),add=TRUE)
  # verbose <- TRUE
   ## because of quicker load of 'proj4' package
  # show.error.messages=verbose
   if (isSF <- .isSF(xy)) {
     # return(sf::st_transform(xy,proj))
      return(spatial_transform(xy,proj))
   }
   else if (isSP <- .isSP(xy)) {
     # return(sp::spTransform(xy,proj))
      return(spatial_transform(xy,proj))
   }
  # print("---")
  # print(class(xy))
  # print("===")
   opW <- options(warn=-11,show.error.messages=verbose);on.exit(options(opW),add=TRUE)
  # if (("package:rgdal" %in% search())||
  #     (!requireNamespace("proj4",quietly=.isPackageInUse())))
  #    res <- project(xy=xy,proj=proj,inv=inv) ## project() from 'rgdal'
  # else
  #    res <- proj4::project(xy=t(xy),proj=proj,inverse=inv)
   a <- FALSE
  # print(summary(xy))
  # proj4 <- requireNamespace("proj4",quietly=.isPackageInUse())
  # print(loadedNamespaces())
   if (F) {
      if (.lgrep("^\\s*\\+init=epsg:\\d+\\s*$",proj)) {
        # proj <- .epsg2proj4(proj,force=TRUE,verbose=TRUE)
        # requireNamespace("rgdal",quietly=.isPackageInUse())
      }
   }
  ## CHECK LATER (currently not quick):
  # dst <- with(PROJ::proj_trans_generic(src,source="EPSG:4326",target=crs),cbind(x_,y_))
   loaded <- loadedNamespaces()
   is_rgdal <- if (F) .forceRGDAL() else ("rgdal" %in% loaded)&&(.forceRGDAL(TRUE))
   is_sf <- TRUE # "sf" %in% loaded ## if F then trying to use `proj4`
   is_gdalraster <- "gdalraster" %in% loaded
   if ((!is_sf)&&(!is_gdalraster)&&(.isWKT(proj)))
      is_sf <- .forceSFpackage(TRUE)
   if (.isWKT(proj)) {
      is_proj4 <- FALSE
   }
   else if (.forceProj4package()) { ## `proj4` faster `sf` 20220216
     # print("B1")
      is_proj4 <- TRUE
     # if (is_sf)
     #    is_sf <- FALSE
   }
   else if ((!is_sf)&&(!is_rgdal)&&(!is_gdalraster)&&(.forceProj4package(TRUE))) {
     # print("B2")
      is_proj4 <- TRUE
   }
   else {
     # print("B3")
      is_proj4 <- FALSE
   }
   if ((!is_sf)&&(!is_rgdal)&&(!is_proj4)&&(!is_gdalraster)) {
     # if (!"rgdal" %in% loadedNamespaces())
      is_sf <- .forceSFpackage(TRUE)
   }
   if (verbose)
      print(c(proj4=is_proj4,rgdal=is_rgdal,sf=is_sf,gdalraster=is_gdalraster))
  # if ((!FALSE)&&(!("package:rgdal" %in% search()))&&
   if ((!a)&&(is_proj4)) {
      if (verbose)
         message("'proj4' is used")
      if (F) {
         dimx <- dim(xy)
         if ((dimx==2)&&(dimx[1]==2)) {
            str(dimx)
         }
      }
      if (is.list(proj)) {
         if ("input" %in% names(proj)) {
            proj <- proj$input
         }
         else {
            str(proj)
            stop("undefined handling for `proj` specification")
         }
      }
      if ((!is.character(proj))||(.lgrep("\\+init=epsg\\:\\d+",proj))) {
         if (verbose)
            message("   EPSG->proj4string conversion")
         proj <- .epsg2proj4(proj,dismissEPSG=TRUE,verbose=verbose)
      }
      a <- .try({
        # str(xy)
        # str(list(xy[,1],xy[,2]))
        ## suppressMessages(require(proj4)) ## uncomment?
         proj4version <- utils::packageVersion("proj4")
         if ((proj4version>="1.0.12")&&(!nchar(Sys.getenv("PROJ_LIB")))) {
            Sys.setenv(PROJ_LIB=system.file("proj",package="sf"))
            if ((!.isPackageInUse())&&(!nchar(Sys.getenv("PROJ_LIB"))))
               Sys.setenv(PROJ_LIB=system.file("proj",package="rgdal"))
         }
         if (proj4version>="1.0.10") {
            res <- .proj4_project(xy=xy,proj=proj,inverse=inv)
         }
         else {
            res <- .proj4_project(xy=t(xy),proj=proj,inverse=inv)
         }
        # res <- proj4::project(xy=list(xy[,1],xy[,2]),proj=proj,inverse=inv)
      },silent=TRUE)
      if ((!FALSE)&&(!a)&&(nrow(xy)==2)) {
         if (verbose) {
            cat(geterrmessage())
            message("         trying to not transpose matrix ('proj4')")
         }
         a <- .try({
           ## suppressMessages(require(proj4)) ## uncomment?
            res <- .proj4_project(xy=xy,proj=proj,inverse=inv)
         },silent=TRUE)
      }
   }
   if (FALSE) {
      if (!a) {
         if ((!is_rgdal)&&(!.isPackageInUse())) {
            is_rgdal <- .rgdal_requireNamespace()
         }
         if ((!is_rgdal)&&(!is_sf))
            is_sf <- TRUE
      }
   }
   if ((!a)&&(is_proj4)) {
      is_sf <- TRUE
   }
   if ((!a)&&(is_rgdal)) {
      if (verbose)
         message("'rgdal' is used")
      if (.isPackageInUse()) {
         opWG <- options(warn=1)
         warning("Unable to reproject without `rgdal` package")
         options(opWG)
      }
     # if (!("rgdal" %in% loadedNamespaces())) {
     #    .rgdal_requireNamespace()
     # }
      if (!is.character(proj))
         proj <- .epsg2proj4(proj)
      if (is.list(xy))
         xy <- cbind(xy[[1]],xy[[2]])
      else if (!is.matrix(xy))
         xy <- matrix(xy,ncol=2)
     # ind <- which((is.na(xy[,1]))|(is.na(xy[,2])))
      ind <- which(is.na(xy[,1])) ## less conditions
      if (length(ind)) {
         res <- matrix(NA,ncol=2,nrow=nrow(xy))
         a <- .try(res[-ind,] <- .rgdal_project(xy=xy[-ind,],proj=proj,inv=inv))
      }
      else {
         a <- .try(res <- .rgdal_project(xy=xy,proj=proj,inv=inv))
      }
      if (!a) {
         if (verbose)
            message("patch with backing to 'proj4' via interim projection")
         .epsg3411 <- paste("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1"
                           ,"+x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")
         if (.proj4_requireNamespace()) {
            a <- .try(res <- .proj4_project(xy=xy,proj=.epsg3411))
            if (a) {
               a <- .try(res <- .proj4_ptransform(res,.epsg3411,proj))
               if (a)
                  return(res)
            }
         }
      }
      if ((FALSE)&&(!a)) {
         str(xy)
         str(proj)
         if (inv) {
            xy <- as.data.frame(xy)
            sp::coordinates(xy) <- ~x+y
            sp::proj4string(xy) <- sp::CRS(proj,doCheckCRSArgs=FALSE)
            a <- .try(res <- sp::spTransform(xy,sp::CRS("+init=epsg:4326")))
            str(a)
            q()
         }
         q()
      }
   }
   if ((!a)&&((is_sf)||(is_gdalraster))) {
      if (verbose)
         message("'sf' is used")
      if (inv) {
         crs_t <- .crsWGS84()
         crs_s <- proj # unclass(proj)
      }
      else {
         crs_s <- .crsWGS84()
         crs_t <- proj # unclass(proj)
      }
      if (is.list(xy))
         xy <- cbind(xy[[1]],xy[[2]])
      else if (!is.matrix(xy))
         xy <- matrix(xy,ncol=2)
     # ind <- which((is.na(xy[,1]))|(is.na(xy[,2])))
      hasNA <- anyNA(xy[,1])
      tryMatrix <- T # (.isProj4(crs_s))&&(.isProj4(crs_t))
      if ((FALSE)&&(!.crsForceProj4())&&(.crsForceWKT())) {
         crs_s <- list(input=NULL,wkt=crs_s)
         class(crs_s) <- "crs"
         crs_t <- list(input=NULL,wkt=crs_t)
         class(crs_t) <- "crs"
      }
      if (F) {
        # dimnames(xy) <- NULL
         str(crs_s)
         str(crs_t)
      }
      if (verbose)
         .elapsedTime(paste(ifelse(tryMatrix,"sf_project","st_transform")
         ,ifelse(.isWKT(crs_s),"(wkt)","(proj4string)")," -- start"))
      if (omitOutside <- FALSE) {
        # if (length(ind180 <- which(xy[,1]>180)))
        #    xy[ind180,1] <- xy[ind180,1]-180
         lim <- c(-18000,10,180000,80)
         if (length(ind180 <- which(xy[,1]<=lim[1])))
            xy[ind180,1] <- lim[1]
         if (length(ind180 <- which(xy[,1]>=lim[3])))
            xy[ind180,1] <- lim[3]
         if (length(ind180 <- which(xy[,2]<=lim[2])))
            xy[ind180,2] <- lim[2]
         if (length(ind180 <- which(xy[,2]>=lim[4])))
            xy[ind180,4] <- lim[4]
      }
      if (hasNA) {
         ind <- which(is.na(xy[,1])) ## less conditions
         res <- matrix(NA,ncol=2,nrow=nrow(xy))
         if (!tryMatrix) {
            a <- .try(res[-ind,] <- unclass(sf::st_transform(sf::st_sfc(
                              sf::st_multipoint(xy[-ind,]),crs=crs_s),crs_t)[[1]]))
         }
         else {
            if (is_gdalraster) {
               a <- .try(res[-ind,] <- gdalraster::transform_xy(pts=xy[-ind,]
                        ,srs_from=gdalraster::srs_to_wkt(crs_s)
                        ,srs_to=gdalraster::srs_to_wkt(crs_t)))
            }
            if (!a) {
              # qs::qsave(list(from=crs_s,to=crs_t,pts=xy[-ind,],keep=TRUE),"C:/tmp/interim.qs")
               a <- .try(res[-ind,] <- sf::sf_project(from=crs_s,to=crs_t,pts=xy[-ind,]
                                                     ,keep=TRUE))
            }
         }
      }
      else {
         if (!tryMatrix) {
            a <- .try(res <- unclass(sf::st_transform(sf::st_sfc(
                                     sf::st_multipoint(xy),crs=crs_s),crs_t)[[1]]))
         }
         else {
            if (is_gdalraster) {
               a <- .try(res <- gdalraster::transform_xy(pts=xy
                        ,srs_from=gdalraster::srs_to_wkt(crs_s)
                        ,srs_to=gdalraster::srs_to_wkt(crs_t)))
            }
            if (!a) {
               if (T & !sf::sf_proj_network())
                  try(sf::sf_proj_network(url="",TRUE))
               if (F & verbose) {
                  print(xy)
                  print(class(crs_s))
                  print(class(crs_t))
                  print(.isProj4(crs_s))
                  print(.isProj4(crs_t))
                  print(crs_s)
                  print(crs_t)
                  print(sf::st_crs(crs_t)$proj4string)
               } ## slow for WKT
               a <- .try(res <- sf::sf_project(from=crs_s,to=crs_t,pts=xy
                                              ,keep=TRUE))
            }
         }
      }
      if (verbose)
         .elapsedTime(paste(ifelse(tryMatrix,"sf_project","st_transform")," -- finish"))
   }
   if ((FALSE)&&(!inv)&&(.lgrep("\\+proj=merc",g1$crs))) {
      g1 <- session_grid()
      ext <- 20037508
      if (g1$maxx>ext) {
         ind <- which(res[,1]<0)
        # print("before:")
        # print(res)
         if (length(ind))
            res[ind,1] <- 2*ext+res[ind,1]
        # print("after:")
        # print(res)
      }
   }
   if (!exists("res"))
      return(NULL)
   return(res)
}
'.epsg2proj4' <- function(code,force=FALSE,dismissEPSG=FALSE,verbose=FALSE) {
  # dismissEPSG: ## dev - FALSE, release and next versions - TRUE
  ## 'proj4::project' doesnot understand pure EPSG
  # a <- try(as.integer(code),silent=TRUE)
   if (.isUrsaCRS(code))
      return(code)
   if ((is.character(code))&&(!nchar(code)))
      return(code)
   if (is.matrix(code))
      return(NULL)
   shortNames <- c("WGS84","NAD27")
   if (!.lgrep("\\D",code))
      p4epsg <- paste0(c("+init=epsg:","EPSG:")[2],code)
   else if (.lgrep("^epsg:\\d+",code))
      p4epsg <- c(paste0("+init=",code),toupper(code))[2]
   else if (.lgrep("^(\\s+)*\\+init=epsg:\\d+",code))
      p4epsg <- c(.gsub("^\\s+","",code),.gsub("^\\s+",""
                 ,toupper(.gsub("\\+init=","",code))))[2]
   else if ((force)&&(.lgrep("^ESRI\\:",code,ignore.case=FALSE)))
      p4epsg <- code
   else if (is.character(code)) {
      if ((code %in% shortNames)||(grepl("\\s",code))) {
         if (!force)
            p4epsg <- code
         else
            return(code)
      }
      else if (nchar(code)<16)
         return("")
      else if (.isWKT(code)) {
         return(code)
      }
      else {
         p4epsg <- code
      }
   }
   else
      stop(code)
  # print(c(input=code,p4=p4epsg))
   loaded <- loadedNamespaces()
   if (dismissEPSG)
      force <- TRUE
   if (!force) {
      if ("sf" %in% loaded) {
         if (verbose)
            message("force to use 'sf'")
         if (.crsForceProj4())
            p4s <- .proj4string(code)
         else
            p4s <- .WKT(code)
      }
      else {
         if (verbose)
            message("force to use 'sp/rgdal'")
        # requireNamespace(c("sp","rgdal")[2],quietly=.isPackageInUse())
         p4s <- p4epsg
      }
   }
   else { ## force
      fail <- TRUE
      if ((fail)&&("sf" %in% loaded)) {
         if (verbose)
            message("'sf' loaded")
        # p4s <- try(sf::st_crs(as.integer(code))$proj4string) ## 'code', not 'p4epsg'
         if (.crsForceProj4()) {
            p4s <- try(.proj4string(.p4s2epsg(p4epsg)),silent=!verbose)
         }
         else {
            p4s <- try(.WKT(p4epsg),silent=!verbose)
         }
         if (!inherits(p4s,"try-error"))
            fail <- FALSE
      }
      if ((fail)&&(any(c("sp","rzzgdal") %in% loaded))) {
         if (verbose)
            message("'sp'/'rgdal' loaded")
         if (dismissEPSG) {
            opW <- options(warn=ifelse(verbose,0,-1))
            p4s <- try(methods::slot(sp::CRS(p4epsg,doCheckCRSArgs=TRUE),"projargs")
                      ,silent=!verbose)
            options(opW)
         }
         else {
            ##~ p4s <- sp::CRS(p4epsg,doCheckCRSArgs=FALSE)
            ##~ print(comment(p4s))
            ##~ p4s <- meslot(p4s,"projargs")
            ##~ str(p4s)
           ##~ # sp::CRS(SRS_string=p4epsg)
            ##~ q()
            p4s <- try(methods::slot(sp::CRS(p4epsg,doCheckCRSArgs=TRUE),"projargs")
                      ,silent=!verbose)  ## -- 20220124 FALSE
         }
         if (!inherits(p4s,"try-error")) {
            fail <- FALSE
           # if (!("rgdal" %in% loaded))
           #    requireNamespace("rgdal",quietly=.isPackageInUse())
         }
      }
      if ((fail)&&(!isNamespaceLoaded("sf"))&&
                  (requireNamespace("sf",quietly=.isPackageInUse()))) {
         if (verbose)
            message("force to load 'sf'")
         if (.crsForceProj4())
            p4s <- try(.proj4string(code),silent=!verbose)
         else {
            p4s <- try(.WKT(code),silent=!verbose)
         }
         if (!inherits(p4s,"try-error"))
            fail <- FALSE
      }
      if ((fail)&&(nchar(system.file(package="sp"))>0)) {
         if (verbose)
            message("Otherwise, use 'sp' + ('rgdal' for reprojection - SKIPPED)")
         if (dismissEPSG) {
            opW <- options(warn=ifelse(verbose,0,-1))
            p4s <- try(methods::slot(sp::CRS(p4epsg,doCheckCRSArgs=TRUE),"projargs")
                      ,silent=!verbose)
            options(opW)
         }
         else {
            p4s <- try(methods::slot(sp::CRS(p4epsg,doCheckCRSArgs=TRUE),"projargs")
                      ,silent=!verbose) ## -- 20220124 FALSE
         }
         if (inherits(p4s,"try-error")) {
            fail <- TRUE
           # if (!("rgdal" %in% loaded))
           #    requireNamespace("rgdal",quietly=.isPackageInUse())
         }
         else
            fail <- FALSE
      }
      if (fail) {
         p4s <- ""
         if (verbose)
            message("Unable to get 'proj4string' from EPSG code")
      }
   }
   p4s
}
'.lonPlus360' <- function(xy) {
   if (scalar <- is.null(dim(xy))) {
      if (xy[1]<0)
         xy[1] <- xy[1]+360
   }
   else if (length(ind <- which(xy[,1]<0)))
      xy[ind,1] <- xy[ind,1]+360
   xy
}
'.p4s2epsg' <- function(p4s) {
   patt <- "EPSG\\:(\\d+)$"
   if (!length(grep(patt,p4s)))
      return(p4s)
   code <- gsub(patt,"\\1",p4s)
   if (code==p4s)
      return(p4s)
   as.integer(code)
}
