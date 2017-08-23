\name{stemPlot}
\alias{stemPlot}
\title{A function for stems plotting}
\usage{
stemPlot(ctFile)
}
\arguments{
\item{ctFile}{A RNA secondary structure file containing structure information}
}
\description{
Given a RNA secondary structure,it compute stems in the RNA secondary structure and plots the RNA secondary structure
}
\value{
Return a list containing base positions in stems
}
\examples{
###
data(DataRNAstr)
stemPlot(DataRNAstr)
}
