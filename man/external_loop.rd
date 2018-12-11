\name{external_loop}
\alias{external_loop}
\title{Internal function for getting external loops in a RNA secondary structure}
\usage{
external_loop(ctFile)
}
\arguments{
  \item{ctFile}{
A RNA secondary structure file in CT format
}
}
\description{
Given a RNA secondary structure,it compute external loops in the RNA secondary structure
}
\value{
Return a list containing information of external loops
}
\author{
Zheng Hewei
}
\examples{
##########
data(DataRNAstr)
external_loop(DataRNAstr)
}
