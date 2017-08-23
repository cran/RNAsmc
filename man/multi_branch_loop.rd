\name{multi_branch_loop}
\alias{multi_branch_loop}
\title{Internal function for getting multi-branch loops}
\usage{
multi_branch_loop(ctFile)
}
\arguments{
\item{ctFile}{A RNA secondary structure file containing structure information}
}
\description{
Given a RNA secondary structure,it compute multi-branch loops in the RNA secondary structure
}
\value{
Return a list containing base positions in multi-branch loops,and the length of the number of multi-branch loops
}
\examples{
###
data(DataRNAstr)
multi_branch_loop(DataRNAstr)
}
