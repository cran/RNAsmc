\name{stem}
\alias{stem}
\title{Internal function for getting stems}
\usage{
stem(ctFile)
}
\arguments{
\item{ctFile}{A RNA secondary structure file containing structure information}
}
\description{
Given a RNA secondary structure,it compute stem in the RNA secondary structure
}
\value{
Return a list containing base positions in stems
}
\examples{
###
data(DataRNAstr)
stem(DataRNAstr)
}
