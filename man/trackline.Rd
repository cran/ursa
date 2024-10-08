\name{trackline}
\alias{trackline}
\title{
Create segmented line from points' sequence
}
\description{
Connect sequence of points (locations) by direct lines (tracks)
}
\usage{
trackline(obj, by=NULL, connect=c("united", "consequent"), gentle=FALSE)
}
\arguments{
  \item{obj}{
   Simple feature (package \pkg{sf}) or Spatial abstract class (package \pkg{sp}) with POINTS spatial geometry.
}
  \item{by}{
   Either field name or \code{NULL}. If specified, then value \code{"united"} is applied for argument \code{connect}, returned spatial object is splitted regarding values of \code{by} with linked single-column attribute table with specified field. Default is \code{NULL}.
}
  \item{connect}{
   Structure of output segments; either sequence of single segments (\code{"consequent"}) or single multi-segment (\code{"united"}).
}
  \item{gentle}{
   Logical. Value \code{TRUE} directs repetition of binded columns of data table. Value \code{FALSE} omits duplicated column values. Default is \code{FALSE}.
}
}
\details{
   Function generates \code{n-1} segments from \code{n} input points. Data (attribute table) is trasfered to output object with excluding of first row.

   \code{gentle=TRUE} may be useful to keep geterogenic data structure for spatial binding (\code{\link[ursa:spatial_bind]{spatial_bind}}).
}
\value{
   Simple feature (package \pkg{sf}) or Spatial abstract class (package \pkg{sp}) with LINESTRING spatial geometry.
}
\author{
Nikita Platonov \email{platonov@sevin.ru}
}
\examples{
session_grid(NULL)
n <- 15
lon <- rev(sort(runif(n,min=40,max=60)))
lat <- sort(runif(n,min=30,max=50))
pt <- data.frame(lon=lon,lat=lat,value=seq(n))
if (requireNamespace("sp")) {
   sp::coordinates(pt) <- c("lon","lat")
   sp::proj4string(pt) <- "EPSG:4326"
} else {
   pt <- sf::st_as_sf(pt,coords=c("lon","lat"),crs="WGS84")
}
ct <- ursa_colortable(colorize(pt$value))
tr <- trackline(pt,connect="consequent")
#opW <- options(warn=0)
session_grid(pt,expand=1.1)
compose_open(2)
panel_new()
panel_plot(pt,col=ct)
panel_decor()
panel_new()
panel_plot(tr,col=ct,lwd=3)
panel_decor()
compose_legend(ct,unit="step number")
compose_close()
}
\keyword{attribute}
\keyword{spatial}
