\name{internalLoopsPlot}
\alias{internalLoopsPlot}
\title{A function for internal loops plotting}
\usage{
internalLoopsPlot(ctFile)
}
\arguments{
\item{ctFile}{A RNA secondary structure file without the first line of free energy information}
}
\description{
Given a RNA secondary structure,it compute internal loops in the RNA secondary structure and plots the RNA secondary structure
}
\value{
Return a list containing base positions in internal loops
}
\examples{
###
data(DataRNAstr)
internalLoopsPlot(DataRNAstr)
}
