\name{hairpin_loop}
\alias{hairpin_loop}
\title{Internal function for getting hairpin loops}
\usage{
hairpin_loop(ctFile)
}
\arguments{
\item{ctFile}{A RNA secondary structure file containing structure information}
}
\description{
Given a RNA secondary structure,it compute hairpin loops in the RNA secondary structure
}
\value{
Return a list containing base positions in hairpin loops,and the length of the number of hairpin loops
}
\examples{
###
data(DataRNAstr)
hairpin_loop(DataRNAstr)
}
