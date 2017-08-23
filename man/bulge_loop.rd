\name{bulge_loop}
\alias{bulge_loop}
\title{Internal function for getting bulge loops in a RNA secondary structure}
\usage{
bulge_loop(ctFile)
}
\arguments{
  \item{ctFile}{
A RNA secondary structure file containing structure information
}
}
\description{
Given a RNA secondary structure,it compute bulge loops in the RNA secondary structure
}
\value{
Return a list containing information of bulge loops
}
\author{
Zheng Hewei
}
\examples{
##########
data(DataRNAstr)
bulge_loop(DataRNAstr)
}
