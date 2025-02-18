'ursa_open' <- function(fname,verbose=FALSE) open_gdal(fname=fname,verbose=verbose)
'open_gdal' <- function(fname,engine=c("native","sf","gdalraster","vapour")
                       ,verbose=FALSE) {
   fun <- if (.isPackageInUse()) match.fun(ursa::open_gdal) else match.fun("open_gdal")
   engList <- as.character(as.list(fun)[["engine"]])[-1]
   if (length(engine)<length(engList)) {
      if (!.isPackageInUse()) {
         engList <- c(engList,"rgdal")
      }
   }
   engine <- match.arg(engine,engList)
  # if (engine=="native")
  #    engine <- "sf" ## replace to 'sf'
  # if (verbose)
  #    print(c(engine=engine),quote=FALSE)
   fname <- gsub("\\.$","",fname)
   if (length(envi_list(fname))==1) {
      return(open_envi(fname,cache=TRUE,verbose=verbose))
   }
   if ((engine=="vapour")&&(requireNamespace("vapour",quietly=.isPackageInUse()))) {
      return(.open_vapour(fname,engine=engine,verbose=verbose))
   }
   if ((engine=="gdalraster")&&(requireNamespace("gdalraster",quietly=.isPackageInUse()))) {
      return(.open_gdalraster(fname,engine=engine,verbose=verbose))
   }
   if (engine!="rgdal") {
      return(.open_sfgdal(fname,engine=engine,verbose=verbose))
   }
   if (engine=="rgdal") {
      return(.open_rgdal(fname,engine=engine,verbose=verbose))
   }
  ## 20170116 removed '...' argument
   if (!is.character(fname))
      return(NULL)
   stop("unknown 'engine'")
}
