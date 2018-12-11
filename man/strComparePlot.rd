\name{strComparePlot}
\alias{strComparePlot}
\title{strComparePlot}
\usage{
strComparePlot(ctFile1,ctFile2)
}
\arguments{
\item{ctFile1}{A RNA secondary structure file containing structure information}
\item{ctFile2}{A RNA secondary structure file containing structure information}
}
\description{
return similarity score of two RNA secondary structures
}
\value{
Returns a numerical value which represent the similarity of the two RNA secondary structures.The larger the value, the more similar the two RNA structures are.The maximum value is 10, representing the two RNA secondary structures exactly the same,and 0 is the minmum value.
}
\examples{
###
data(DataCluster1)
data(DataCluster2)
#####RNAstrPlot(DataCluster1)
#####RNAstrPlot(DataCluster2)
strComparePlot(DataCluster1,DataCluster2)
}
