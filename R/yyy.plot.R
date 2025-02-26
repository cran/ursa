'.makeFileout' <- function(obj,fileout="")
{
   if (!nchar(fileout))
   {
      if (!is.na(obj$con$fname))
      {
         f <- obj$con$fname
         string <- "\\.(unpacked(.*)~|bin|bingz|envi|enviz)$"
         if (.lgrep(string,f))
            f <- .gsub(string,"",f)
         fileout <- paste0(basename(f),basename(.maketmp()),".png")
         rm(f)
      }
      else
         fileout <- .maketmp(ext="png")
   }
   if (!.lgrep("\\.png$",fileout))
      fileout <- paste0(fileout,".png")
   fileout
}
'.repairForScatterPlot' <- function() {
   g1 <- session_grid()
   if ((nchar(g1$crs)==0)||(is.na(g1$crs)))
   {
      cexx <- cexy <- 1
      x <- seq(g1,"x")
      y <- seq(g1,"y")
      img <- list(x=x,y=y)
      labx0 <- attr(g1$seqx,"lab")
      laby0 <- attr(g1$seqy,"lab")
      if (!is.null(labx0)) {
         if (length(g1$seqx)!=g1$columns) {
            attrx <- attributes(g1$seqx)
            g1$seqx <- seq(g1,"x")
            attributes(g1$seqx) <- attrx
         }
      }
      if (!is.null(laby0)) {
         if (length(g1$seqy)!=g1$rows) {
            attry <- attributes(g1$seqy)
            g1$seqy <- seq(g1,"y")
            attributes(g1$seqy) <- attry
         }
      }
      if ((!is.null(g1$seqx))&&(length(g1$seqx)))
      {
         if (length(labx0))
         {
            if (length(names(labx0))==length(labx0))
               xlab <- names(labx0)
            else
               xlab <- labx0
            xo <- data.frame(at=NA,lab=xlab,stringsAsFactors=FALSE)
            for (i in seq(nrow(xo)))
            {
               v <- labx0[i]
               i2 <- which(v<g1$seqx)[1]
               if ((is.na(i2))||(i2<2))
                  next
               i1 <- i2-1
               sc <- (v-g1$seqx[i1])/(g1$seqx[i2]-g1$seqx[i1])
               xo$at[i] <- img$x[i1]+sc*(img$x[i2]-img$x[i1])
            }
           # xo <- subset(xo,!is.na(at))
            xo <- xo[!is.na(xo$at),]
         }
         else
         {
            xo <- .prettyLabel(img$x,ncol=11)
           # xo <- subset(xo,at>=g1$minx & at<=g1$maxx)
            xo <- xo[xo$at>=g1$minx & xo$at<=g1$maxx,]
            if (F & 0 %in% xo$at) ## WHY???
               xo$at <- xo$at+1L
            if (F & all(xo$at>0)) ## WHY?
               xo$lab <- g1$seqx[xo$at]
            if (F) ## WHY?
               xo$at <- xo$at-0.5
         }
      }
      else
      {
         xt <- axTicks(1)
         xo <- data.frame(at=xt,lab=as.character(xt),stringsAsFactors=FALSE)
        # cexx <- 0.5
      }
      if ((!is.null(g1$seqy))&&(length(g1$seqy)))
      {
         if (length(laby0))
         {
            if (length(names(laby0))==length(laby0))
               ylab <- names(laby0)
            else
               ylab <- laby0
            yo <- data.frame(at=NA,lab=ylab,stringsAsFactors=FALSE)
            for (i in seq(nrow(yo)))
            {
               v <- laby0[i]
               i2 <- which(v<g1$seqy)[1]
               if ((is.na(i2))||(i2<2))
                  next
               i1 <- i2-1
               sc <- (v-g1$seqy[i1])/(g1$seqy[i2]-g1$seqy[i1])
               yo$at[i] <- img$y[i1]+sc*(img$y[i2]-img$y[i1])
            }
           # yo <- subset(yo,!is.na(at))
            yo <- yo[!is.na(yo$at),]
         }
         else
         {
            yo <- .prettyLabel(img$y,ncol=11)
           # yo <- subset(yo,at>=g1$miny & at<=g1$maxy)
            yo <- yo[yo$at>=g1$miny & yo$at<=g1$maxy,]
            if (0 %in% yo$at)
               yo$at <- yo$at+1L
            if (all(yo$at>0))
               yo$lab <- g1$seqy[yo$at]
            yo$at <- yo$at-0.5
         }
      }
      else
      {
         yt <- axTicks(2)
         yo <- data.frame(at=yt,lab=as.character(yt),stringsAsFactors=FALSE)
        # cexy <- 0.5
      }
      abline(v=xo$at,h=yo$at,lty=2,col="#0000002F")
      panel <- options()[.grep("^ursaPng.+",names(options()))]
      layout <- panel[["ursaPngLayout"]][["layout"]]
      figure <- panel[["ursaPngFigure"]]
      isTop <- figure %in% layout[3,]
      isBottom <- figure %in% layout[nrow(layout)-2,]
      isLeft <- figure %in% layout[,3]
      isRight <- figure %in% layout[,ncol(layout)-2]
      rm(panel,layout,figure)
     # axis(side=1,at=xo$at,labels=NA,tck=-0.2,col="red")
     # axis(side=2,at=yo$at,labels=NA,tck=-0.2,col="blue")
      width <- strwidth(paste(xo$lab,collapse=" "),units="inches",cex=cexy)
      if (width>0.5*par()$pin[1])
      {
         i0 <- which(seq(nrow(xo))%%2==0)
         i1 <- which(seq(nrow(xo))%%2==1)
         if (isTop)
            mtext(side=3,text=xo$lab[i0],at=xo$at[i0],padj=-0.25,adj=0.5,line=0
                 ,cex=cexx,las=1)
         if (isBottom)
            mtext(side=1,text=xo$lab[i1],at=xo$at[i1],padj=0.5,adj=0.5
                 ,cex=cexx,las=1)
         rm(i0,i1)
      }
      else if (isBottom)
         mtext(side=1,text=xo$lab,at=xo$at,padj=0.5,adj=0.5,cex=cexx,las=1)
      if (isLeft) {
         mtext(side=2,text=yo$lab,at=yo$at,padj=0.4,adj=1.0,line=0.4,cex=cexy,las=1)
      }
      xu <- attr(g1$seqx,"units")
      if ((isBottom)&&(is.character(xu)))
      {
         xu <- as.expression(substitute(bold(u),list(u=xu)))
         mtext(xu,side=1,padj=1,adj=0.5,las=1,col="black",cex=cexx,line=0.85)
      }
      yu <- attr(g1$seqy,"units")
      if ((isLeft)&&(is.character(yu)))
      {
         width <- max(strwidth(yo$lab,units="inches",cex=cexy))
         height <- max(strheight(yo$lab,units="inches",cex=cexy))
         yu <- as.expression(substitute(bold(u),list(u=yu)))
         mtext(yu,side=2,padj=0,adj=0.5,las=3,col="black",cex=cexy
              ,line=0.8+width/height)
      }
      return(NULL)
   }
   NULL
}
'.getSide' <- function()
{
   figN <- getOption("ursaPngFigure")
   figP <- getOption("ursaPngLayout")$image
   while (figN<figP) {
      panel_new()
      figN <- figN+1L
   }
   panel <- getOption("ursaPngLayout")
   if (figN==panel$image) {
      .panel_attribution()
      if (getOption("ursaPngBox"))
         panel_box()
   }
   figN <- figN+1L
   options(ursaPngFigure=figN)
   mat <- panel$layout
   ind <- which(c(mat)==figN)
   indr <- c(row(mat))[ind]
   indc <- c(col(mat))[ind]
   if (all(indr==1))
      side <- 3L
   else if (all(indr==nrow(mat)))
      side <- 1L
   else if (all(indc==ncol(mat)))
      side <- 4L
   else if (all(indc==1))
      side <- 2L
   else
      stop("cannot identify side")
   side
}
'.panel_attribution' <- function(pos,vertical,cex) {
  # if (.skipPlot(TRUE))
  #    return(NULL)
   g0 <- .panel_grid() # getOption("ursaSessionGrid_prev")
   g1 <- getOption("ursaSessionGrid")
   prev <- !identical(g0,g1)
   if (prev)
      session_grid(g0)
   isWindows <- (.Platform$OS.type=="ZZZwindows")&&(getOption("ursaPngDevice")=="ZZZwindows")
   if (isWindows) {
      wf <- do.call("grDevices::windowsFont",list("TT Arial Narrow"))
      do.call("grDevices::windowsFonts",list('Arial Narrow'=wf))
   }
   ann <- paste0("",paste(unique(getOption("ursaPngCopyright")),collapse=" | "))
   if ((missing(pos))&&(missing(vertical))&&(missing(cex))) {
      pos <- getOption("ursaPngAttribution","bottomright vertical")
      pos <- strsplit(pos,split="\\s+")[[1]]
      ind1 <- grep("^[hv]",pos)
      ind2 <- grep("(^top(left|right)*|^bottom(left|right)*|^(left|right))",pos)
      ind3 <- grep("\\d+(\\.\\d+)",pos)
      cex <- 0.7
      vertical <- TRUE
      if (length(ind1)) {
         if (grepl("^h",pos[ind1]))
            vertical <- FALSE
         else if (grepl("^v",pos[ind2]))
            vertical <- TRUE
      }
      if (length(ind3)) {
         cex <- as.numeric(pos[ind3])
      } 
      if (length(ind2)) {
         pos <- pos[ind2]
      } else {
         pos <- "bottomright"
      }
   }
  # print(data.frame(pos=pos,vertical=vertical,cex=cex));q()
  # ann <- paste(c(getOption("ursaPngCopyright")),collapse="\n")
   if (nchar(ann)) {
      dark <- getOption("ursaPngBasemapBright",255)<140
      panel_annotation(ann,pos=pos,cex=cex
                      ,font=ifelse(getOption("ursaPngDevice")=="windows"
                                  ,par("family"),"Arial Narrow")
                      ,fg=sprintf(paste0("#",ifelse(dark,"FFFFFF","000000"),"%s"),"4F")
                      ,fill=ifelse(dark,"#0000001F","#FFFFFF1F")
                      ,vertical=vertical
                      )
   }
   if (prev)
      session_grid(g1)
   options(ursaPngCopyright=NULL,ursaPngBasemapBright=NULL)
}
