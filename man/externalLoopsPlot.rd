\name{externalLoopsPlot}
\alias{externalLoopsPlot}
\title{A function for bulge loops plotting}
\usage{
externalLoopsPlot(ctFile)
}
\arguments{
\item{ctFile}{A RNA secondary structure file in CT format}
}
\description{
Given a RNA secondary structure,it compute external loops in the RNA secondary structure and plots the RNA secondary structure
}
\value{
Return a list containing base positions in external loops and plot the given RNA secondary
}
\examples{
###
data(DataRNAstr)
externalLoopsPlot(DataRNAstr)
}
