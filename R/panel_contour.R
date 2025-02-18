'panel_contour' <- function(obj,type="",...) {
   if (.skipPlot(TRUE))
      return(NULL)
   if (!is.ursa(obj))
      return(contour(obj,...))
   isLabel <- .lgrep("label",type)>0
   isLine <- .lgrep("line",type)>0
   isFilled <- .lgrep("fill",type)>0
   isColored <- .lgrep("colo(u)*r",type)>0
   if ((!isLine)&&(!isFilled)&&(!isColored)&&(!isLabel))
      isLine <- TRUE
   if ((isFilled)&&(isColored))
      isColored <- FALSE
   if (isLabel){
     # if (isLine)
     #    isLabel <- FALSE
     # if ((isFilled)||(isColored))
     #    isLine <- TRUE
   }
   verbose <- .getPrm(list(...),name="verb(ose)*",default=FALSE)
   if (T & verbose)
      print(c(line=isLine,filled=isFilled,colored=isColored,label=isLabel))
   if ((!TRUE)&&(isLabel)&&(!isFilled)&&(!isColored)&&(!isLine))
      res <- .panel_contour(obj,expand=0,...)
   else {
      res <- .panel_contour(obj,category=isColored,...)
   }
   if (isFilled) {
      with(res,.filled.contour(x,y,z,levels=lev,col=col))
   }
   if (isColored) {
      cl <- with(res,contourLines(x,y,z,levels=lev))
      arglist <- list(...)
      col.fg <- res$col
      col.bg <- .getPrm(arglist,name="bg",default="black")
      lwd.fg <- .getPrm(arglist,name="lwd(\\.fg)*",default=2)
      lwd.bg <- .getPrm(arglist,name="lwd.bg",default=lwd.fg*1.5)
      short <- .getPrm(arglist,name="short",default=0L)
      verbose <- .getPrm(arglist,name="verb(ose)*",default=FALSE)
      if (verbose)
         print(list(col.fg=col.fg,col.bg=col.bg,lwd.fg=lwd.fg,lwd.bg=lwd.bg
                   ,short=short))
      val <- .deintervale(col.fg)
      isbg <- lwd.bg>lwd.fg
      if (isbg) {
         col.bg <- rep(col.bg,length=length(col.fg))
      }
      lapply(cl,function(p){
         with(p,{
            if (length(x)<short)
               return(NULL)
            ind <- match(level,val)
            if (isbg)
               lines(x,y,col=col.bg[ind],lwd=lwd.bg)
            lines(x,y,col=col.fg[ind],lwd=lwd.fg)
         })
      })
   }
   if ((isLine)||(isLabel)) {
      arglist <- list(...)
      ct <- res$col
      val <- .deintervale(ct)
     # if ((!FALSE)&&(!isColored)&&(length(ct)==length(val))) {
     #    res$lev <- head(res$lev,-1)+diff(res$lev)/2
     # }
     # add <- .getPrm(arglist,name="add",default=TRUE)
      if ((isLabel)&&(!isLine))
         lwd <- NA
      else
         lwd <- .getPrm(arglist,name="lwd$",default=0.5)
      col <- .getPrm(arglist,name="col",class=c("character","logical")
                    ,default="black") ## res$col col.bg
      if ((isLabel)&&(!isLine))
         lty <- "blank"
      else
         lty <- .getPrm(arglist,name="lty",class=list("character","numeric")
                       ,default=1)
      labcex <- .getPrm(arglist,name="(lab)*cex",default=1) ## 0.85
      method <- .getPrm(arglist,name="method",default="flattest")
      labels <- .getPrm(arglist,name="label(s)",class="character",default=NULL)
      if ((isLine)&&(!isLabel))
         drawL <- FALSE
      else if ((isFilled)&&(!isLabel))
         drawL <- FALSE
      else
         drawL <- TRUE
     # str(list(res="res",levels=res$lev,col=col,lwd=lwd,lty=lty,labels=labels
     #                  ,labcex=labcex,method=method,drawlabels=drawL,add=TRUE))
      if (is.character(res$lev)) {
         if (is.null(labels)) {
            labels <- res$lev
         }
         res$lev <- seq_along(res$lev)-1
      }
      if (!length(res$lev)) {
         if (is.character(res$z)) {
            res$lev <- seq_along(res$col)-1
            dimz <- dim(res$z)
            res$z <- as.integer(factor(res$z,levels=names(res$col)))-1
            dim(res$z) <- dimz
         }
      }
      if (isTRUE(is.logical(col))) {
         bright <- mean(colSums(col2rgb(res$col)*.greyscale()))
         bgcol <- ifelse(bright<160,"white","black")
         col <- c(bgcol,res$col,bgcol)
         reps <- round(strwidth(labels,cex=labcex)/strwidth(" ",cex=labcex))
         spaces <- sapply(reps, function(x) paste(rep(" ", round(x)), collapse=""))
         contour(res,levels=unique(res$lev),col=bgcol,lwd=tail(lwd,1)
                ,lty=lty,labels=spaces
                ,labcex=labcex,method=method,drawlabels=T,add=TRUE)
         contour(res,levels=unique(res$lev),col=col,lwd=head(lwd,1),lty=lty
                ,labels=spaces
                ,labcex=labcex,method=method,drawlabels=drawL,add=TRUE)
         len <- 500
         usr <- par()$usr
         xo <- seq(usr[1],usr[2],len=len)
         yo <- seq(usr[3],usr[4],len=len)
         dx <- diff(xo[1:2])
         dy <- diff(yo[1:2])
         labrange <- seq(-2,2)
         for (di in labrange) {
            for (dj in labrange) {
               if (abs(dj)+abs(di)>3)
                  next
               if ((!dj)&(!di))
                  next
               o <- res
               o$x <- o$x+dx/2*di
               o$y <- o$y+dy/2*dj
               contour(o,lty=0,labels=labels,levels=unique(res$lev)
                      ,labcex=labcex,add=TRUE,col=bgcol)
            }
         }
         contour(res,lty=0,labels=labels,levels=unique(res$lev)
                ,labcex=labcex,add=TRUE,col=col)
      }
      else
         contour(res,levels=unique(res$lev),col=col,lwd=head(lwd,1)
                ,lty=lty,labels=labels
                ,labcex=labcex,method=method,drawlabels=drawL,add=TRUE)
   }
   res$col
}
'.panel_contour' <- function(obj,category=FALSE,...) {
   arglist <- list(...)
   g0 <- session_grid()
   isCT <- .is.colortable(obj)
   verbose <- .getPrm(arglist,name="verb(ose)*",default=FALSE)
   sc <- .getPrm(arglist,name="expand",default=NA_real_)
   before <-  if (isCT) FALSE else .getPrm(arglist,name="before",default=TRUE)
   if (is.na(sc)) {
      sc <- getOption("ursaPngScale")
      if (!is.numeric(sc))
         sc <- 300/with(obj$grid,sqrt(columns*rows))
   }
   if (verbose)
      print(data.frame(sc=sc,before=before))
   '.smooth' <- function(obj,sc) {
      proposed <- FALSE ## added 20170608 (TRUE) removed 20180218 (FALSE)
      if (sc<=1)
         return(obj)
     # verbose <- TRUE
      g2 <- regrid(g0,mul=sc,border=1)
      cov <- .getPrm(arglist,name="cover",default=NA_real_)
      if (proposed) {
         ct2 <- ursa(obj,"colortable")
         if (length(ct2)) {
           # print(ursa(obj,"table"))
            obj <- discolor(obj)
         }
      }
      obj <- regrid(obj,border=1,cover=cov,resample=1+1e-6,fillNA=TRUE
                   ,verbose=verbose)
      obj <- regrid(obj,mul=sc,cover=cov,cascade=TRUE,verbose=verbose)
      obj <- regrid(obj,g2,verbose=verbose)
      if (proposed) {
         if (length(ct2)) {
            obj <- colorize(obj,colortable=ct2,lazyload=FALSE)
           # obj <- as.integer(round(obj));ursa(obj,"colortable") <- ct2
         }
      }
      obj
   }
   if (before) {
      if (isCT)
         obj <- reclass(obj) ## category -> real
      obj <- .smooth(obj,sc)
   }
   if (!isCT) {
      arglist$stretch <- .getPrm(arglist,name="stretch",default="linear")
      arglist$ramp <- .getPrm(arglist,name="ramp",default=FALSE)
      arglist$interval <- .getPrm(arglist,name="interval",default=1L)
      arglist$value <- .getPrm(arglist,name="^value",class="numeric",default=NULL)
      if ((arglist$interval==0)||(!is.na(pmatch(arglist$stretch,"category"))))
      {
         if (TRUE) {
            opW <- options(warn=-2)
            warning("Unable to make 'filled.contour' with categories")
            options(opW)
         }
         return(NULL)
      }
      isJulian <- arglist$stretch %in% c("julian")
      if (isJulian)
         arglist$interval <- 0L
      obj <- do.call("colorize",c(quote(obj[1]),arglist))
      val <- .deintervale(obj)
      arglist$interval <- !arglist$interval
      arglist$stretch <- "linear"
      if (isJulian) {
        # arglist$name <- val
         val <- seq_along(val)
         arglist$ncolor <- length(val)
      }
      o <- as.ursa(matrix(val,ncol=1))
      o <- do.call("colorize",c(quote(o),arglist))
      ct2 <- ursa_colortable(o)
   }
   else
      isJulian <- FALSE
   ct <- ursa_colortable(obj)
   if (!before) {
      ct3 <- ursa_colortable(obj)
      obj <- .extract(obj)
     # obj <- reclass(obj)
      ursa_colortable(obj) <- character(0)
      obj <- .smooth(obj,sc)
      ursa_colortable(obj) <- ct3
   }
   session_grid(g0)
   if ((!FALSE)&&(category)) {
     # levels <- reclass(ct)
      val <- .deintervale(ct)
      if (.is.nominal(ct)) {
         val <- val[-1]-diff(val)/2
      }
      if (isCT)
         ct2 <- colorRampPalette(ct)(2*length(val)+1)[2*seq(val)]
      names(ct2) <- val
      class(ct2) <- "ursaColorTable"
   }
   obj <- reclass(obj,ct)
  # obj <- reclass(discolor(obj),ct) ## 20170608 proposed
   res <- as.matrix(obj,coords=TRUE)
   val <- .deintervale(ct)
   if (isJulian) {
      vname <- val
      val <- seq_along(vname)
   }
   oneBreak <- length(val)==1
   if (!is.character(val))
      dval <- if (oneBreak) 0 else diff(val)/2
   if (((category)&&(.is.nominal(ct)))||(is.character(val))) {
      val2 <- .deintervale(ct2)
      dval2 <- diff(val2)
      res$lev <- c(head(val2,1)-head(dval2,1),val2,tail(val2,1)+tail(dval2,1))
   }
   else {
      res$lev <- c(head(val,1)-2*head(dval,1),val,tail(val,1)+2*tail(dval,1))
      if (isJulian) {
         lev <- rep("***",length(res$lev))
         lev[1] <- "low"
         lev[length(lev)] <- "high"
         lev[match(val,res$lev)] <- vname
         res$lev <- lev
      }
      val <- c(head(val,1)-head(dval,1),head(val,-1)+dval,tail(val,1)+tail(dval,1))
   }
   if (oneBreak) {
      if (val[1]==0)
         val <- c(-1e-6,1e-6)
      else
         val <- c(val[1]-1e-6/val[1],val[2]+1e-6/val[2])
      res$lev <- res$lev[1:2]
   }
   res$col <- if (category) ct2 else ct
   res$z[] <- val[res$z+1L]
   attr(res,"colortable") <- NULL
   invisible(res)
}
