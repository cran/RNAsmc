\name{bulgeLoopsPlot}
\alias{bulgeLoopsPlot}
\title{A function for bulge loops plotting}
\usage{
bulgeLoopsPlot(ctFile)
}
\arguments{
\item{ctFile}{A RNA secondary structure file containing structure information}
}
\description{
Given a RNA secondary structure,it compute bulge loops in the RNA secondary structure and plots the RNA secondary structure
}
\value{
Return a list containing base positions in bulge loops
}
\examples{
###
data(DataRNAstr)
bulgeLoopsPlot(DataRNAstr)
}
