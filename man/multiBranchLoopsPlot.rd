\name{multiBranchLoopsPlot}
\alias{multiBranchLoopsPlot}
\title{A function for multi-branch loops plotting}
\usage{
multiBranchLoopsPlot(ctFile)
}
\arguments{
\item{ctFile}{A RNA secondary structure file containing structure information}
}
\description{
Given a RNA secondary structure,it compute multi-branch loops in the RNA secondary structure and plots the RNA secondary structure
}
\value{
Return a list containing base positions in multi-branch loops
}
\examples{
###
data(DataRNAstr)
multiBranchLoopsPlot(DataRNAstr)
}
