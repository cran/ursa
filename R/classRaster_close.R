'close_envi' <- function(...) close(...)
'close.ursaRaster' <- function(...)
{
   args <- list(...)
   verbose <- .getPrm(args,"^verb",default=getOption("ursaDevel",FALSE))
   if (!is.null(names(args)))
      args <- args[grep("^verb",names(args),invert=TRUE)]
   for (i in seq(along=args))
   {
      con <- args[[i]]$con
     # print(class(con))
      if (!.is.con(con))
         next
      if (inherits(con$handle,"connection")) {
         if ((F)&&(con$compress %in% c(0L,3L))&&(con$connection=="file")) {
            str(args[[i]])
            str(con)
         }
         if (any(con$fname %in% showConnections()[,"description"]))
            close(con$handle)
         con$handle <- NA
         if ((con$driver=="EGDAL")&&(length(con$fname)==2)) {
            with(con,.envi2gdal(src=fname[2],dst=fname[1],datatype=datatype,bands=bands))
            envi_remove(con$fname[2])
            con$compress <- 0L
         }
         if (con$compress==-1L)
            file.remove(con$fname)
         else if (con$compress==-3L) {
            extra <- attr(con$fname,"source")
            dst <- file.path(dirname(extra)
                            ,.gsub("\\.unpacked(.*)~$",".envi",basename(con$fname)))
           # print(c(src=con$fname,dst=dst,extra=extra))
            file.remove(extra)
            file.rename(con$fname,dst)
         }
         else if (con$compress==-1002L)
         {
            if (is.null(fname <- attr(con$fname,"source")))
               fname <- .gsub("\\.unpacked(.*)~$",".envi",con$fname)
            else
               fname <- gsub("gz$","",fname)
            file.rename(con$fname,fname)
            if (file.exists(ftmp <- paste0(fname,".gz")))
               file.remove(ftmp)
            if (file.exists(ftmp <- paste0(.gsub("\\.bin","",fname),".gz")))
               file.remove(ftmp)
            if (file.exists(ftmp <- paste0(.gsub("\\.envi","",fname),".gz")))
               file.remove(ftmp)
         }
         else if (con$compress==-2002L) ## DEVEL: opened cache=T compress=T, then replaced
         {
            if (is.null(fname <- attr(con$fname,"source")))
               fname <- .gsub("\\.unpacked(.*)~$",".envi",con$fname)
            if (verbose)
               cat("compress update to",.dQuote(basename(fname)),"...")
            system2("gzip",c("-f -9 -c -n",.dQuote(con$fname))
                   ,stdout=fname,stderr=FALSE)
            if (verbose)
               cat(" done!\n")
         }
         else if (con$compress==1L)
         {
           # .elapsedTime("CLOSE 1")
            if (file.exists(ftmp <- paste0(con$fname,".gz")))
               file.remove(ftmp)
            if (nchar(Sys.which("gzip"))) {
               for (i in seq(10)) {
                  break
                  s <- file.size(con$fname)
                  print(c(i=i,s=s))
                  if (s)
                     break
                  Sys.sleep(7)
               }
               if (verbose)
                  cat("compress",.dQuote(basename(con$fname)),"...")
               a <- system(paste("gzip","-f -Sgz",dQuote(con$fname))) ##keep
               if (verbose)
                  cat(" done!\n")
              # str(a)
            }
           # .elapsedTime("PASSED")
           # src <- paste0(con$fname,"gz")
           # dst <- file.path(dirname(src),.gsub("\\.bin",".gz",basename(con$fname)))
           # file.rename(src,dst)
         }
         else if (con$compress %in% c(-2L,3L)) { ## compress==0 and at least one 'replace'
            if (dirname(con$fname)==.ursaCacheDir()) {
               was <- .ursaCacheRead()
               if (!is.null(was)) {
                  ind <- tail(which(!is.na(match(was$dst,basename(con$fname)))),1)
                  if (length(ind)==1) {
                    # print(was[ind,])
                    # print(file.mtime(was$src[ind]))
                    # print(as.integer(file.mtime(was$src[ind])))
                     dst <- was$src[ind]
                   #  print(c('con$fname'=con$fname,'dst'=dst))
                   #  print(was)
                   #  print(file.size(file.path(.ursaCacheDir(),was$dst[ind])))
                   #  q()
                     if (file.exists(dst)) {
                        if (verbose)
                           cat("compress update to",.dQuote(basename(dst)),"...")
                        if ((.lgrep("(envi|bin|\\.)gz$",dst))&&
                                                     (nchar(Sys.which("gzip")))) {
                           system2("gzip",c("-f -9 -c -n",.dQuote(con$fname))
                                  ,stdout=dst,stderr=FALSE)
                        }
                        else if ((.lgrep("\\.bz2$",dst))&&
                                                     (nchar(Sys.which("bzip2")))) {
                           system2("bzip2",c("-f -9 -n",.dQuote(con$fname))
                                  ,stdout=dst,stderr=FALSE)
                        }
                        else if ((.lgrep("\\.zst$",dst))&&
                                                     (nchar(Sys.which("zstd")))) {
                           system2("zstd",c("-f -11 -c",.dQuote(con$fname),"-o",dst)
                                  ,stdout=dst,stderr=FALSE)
                        }
                        if (verbose)
                           cat(" done!\n")
                     }
                     was$size[ind] <- file.size(file.path(.ursaCacheDir(),was$dst[ind]))
                     was$stamp[ind] <- as.integer(file.mtime(was$src[ind]))
                     was$visits[ind] <- was$visits[ind]+1L
                     was$time[ind] <- format(Sys.time(),format="%Y-%m-%dT%H:%M:%SZ",tz="UTC")
                    # print(was[ind,])
                     .ursaCacheWrite(was[ind,])
                  }
               }
            }
           # print(.ursaCasheLoc())
           # a <- .ursaCacheRead()
           # ind <- match(con$fname,)
           # str(a)
         }
      }
      else if (inherits(con$handle,"GDALTransientDataset")) {
         bname <- args[[i]]$name
         .rgdal_close_Transient(con,bname)
      }
      else if (inherits(con$handle,"GDALReadOnlyDataset")) {
        # print(class(con$handle))
         .rgdal_close_ReadOnly(con)
         con$handle <- NA
      }
   }
   invisible(NULL)
}
'.reopen' <- function(con) ## not called everywhere
{
   close(con$handle)
   open(con$handle)
   if (con$offset)
   {
      if (con$seek)
         seek(con$handle,where=con$offset,origin="start")
      else
         readBin(con$handle,raw(),n=con$offset)
   }
}
