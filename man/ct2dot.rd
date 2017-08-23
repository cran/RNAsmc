\name{ct2dot}
\alias{ct2dot}
\title{ct2dot}
\usage{
ct2dot(ctFile)
}
\arguments{
\item{ctFile}{A RNA secondary structure file containing structure information}
}
\description{
Given a RNA secondary structure,it compute the RNA secondary structure in dot-bracket notation
}
\value{
return a list including the RNA seqence and the RNA secondary structure in bracket dot form
}
\examples{
###
data(DataRNAstr)
ct2dot(DataRNAstr)
}
