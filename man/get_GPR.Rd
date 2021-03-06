% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funs.R
\name{get_GPR}
\alias{get_GPR}
\title{Get Geopolitical Risk Index (GPR) data}
\usage{
get_GPR(type = 1)
}
\arguments{
\item{type}{a numeric indicating the type. 1 for quarterly GRI, 2 for GPRH, 3 for GPR of countries, and 4 for GPR words.}
}
\value{
an xts data object
}
\description{
Get Geopolitical Risk Index (GPR) data
}
\examples{
gpr <- get_GPR(1)
plot(gpr$GPR)
}
\references{
\url{https://www.matteoiacoviello.com/gpr.htm}
}
\seealso{
\code{\link{xts}}
}
