\name{ursa_match}
\alias{ursa_match}
\alias{mtfrm.ursaRaster}
\title{
Raster value matching
}
\description{
\code{ursa_match} creates raster mask, which is corresponded to vector of reference values
}
\usage{

\method{mtfrm}{ursaRaster}(x)

ursa_match(obj, value)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, obj}{
Object of class \code{ursaRaster}
}
  \item{value}{
Vector of reference values, generally, numeric. If \code{obj} is categorical, then values can be character for matching to category names.
}
}
\details{
\code{ursa_match(obj,c("blue","yellow")} is equal to \code{obj=="blue" | obj=="yellow"}
}
\value{
Object of class \code{ursaRaster} as a mask of correspondence to reference values
}
\author{
Nikita Platonov \email{platonov@sev-in.ru}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Comparison \code{`==`} in \link[ursa]{groupGeneric}
}
\examples{
session_grid(NULL)
a <- colorize(ursa_dummy(1),name=c("low","medium","high"),stretch="equal")
ta <- as.table(a)
ta
c('category value for "low"'=category_value(a,"low"))
res <- c('match to single char'=ursa_match(a,c("medium"))
        ,'match to multiple num'=ursa_match(a,c(0,category_value(a,"high")))
        ,'match to multiple char'=ursa_match(a,c("low","high"))
        ,'logical OR' = a=="low" | a=="high")
res
c(band_n(res)[1]==ta[2],band_n(res)[3]==sum(ta[c(1,3)]))
}
\keyword{methods}
