\name{hairpinLoopsPlot}
\alias{hairpinLoopsPlot}
\title{A function for hairpin loops plotting}
\usage{
hairpinLoopsPlot(ctFile)
}
\arguments{
\item{ctFile}{A RNA secondary structure file containing structure information}
}
\description{
Given a RNA secondary structure,it compute hairpin loops in the RNA secondary structure and plots the RNA secondary structure
}
\value{
Return a list containing base positions in hairpin loops
}
\examples{
###
data(DataRNAstr)
hairpinLoopsPlot(DataRNAstr)
}
