\name{getSubStr}
\alias{getSubStr}
\title{Internal function for getting substructure information in a RNA secondary structure}
\usage{
getSubStr(ctfile)
}
\arguments{
  \item{ctfile}{
A RNA secondary structure file containing structure information
}
}
\description{
Given a RNA secondary structure,it gets all substructures of the RNA
}
\value{
Return a list containing information of all substructures of the RNA
}
\author{
Zheng Hewei
}
\examples{
##########
data(DataRNAstr)
getSubStr(DataRNAstr)
}
