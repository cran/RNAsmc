\name{internal_loop}
\alias{internal_loop}
\title{Internal function for getting internal loops}
\usage{
internal_loop(ctFile)
}
\arguments{
\item{ctFile}{A RNA secondary structure file containing structure information}
}
\description{
Given a RNA secondary structure,it compute internal loops in the RNA secondary structure
}
\value{
Return a list containing base positions in internal loops,and the length of the number of internal loops
}
\examples{
###
data(DataRNAstr)
internal_loop(DataRNAstr)
}
