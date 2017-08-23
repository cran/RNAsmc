\name{RNAstrPlot}
\alias{RNAstrPlot}
\title{A function for whole RNA secondary structure plotting}
\usage{
RNAstrPlot(ctFile)
}
\arguments{
  \item{ctFile}{
A RNA secondary structure file containing structure information
}
}
\description{
Given a RNA secondary structure,it plots RNA structure and specify strucutre units by different colors
}
\value{
Return a list containing structure information
}
\author{
Zheng Hewei
}
\examples{
##########
data(DataRNAstr)
RNAstrPlot(DataRNAstr)
}
