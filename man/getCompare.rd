\name{getCompare}
\alias{getCompare}
\title{Internal function for getting similarity score in a RNA secondary structure}
\usage{
getCompare(subStrList)
}
\arguments{
  \item{subStrList}{
A list comprised by the return of function getSubStr
}
}
\description{
Given a substructure list,it compute similarity score of give RNA structures
}
\value{
Return a similarity score
}
\author{
Zheng Hewei
}
\examples{
##########
data(DataCluster1)
data(DataCluster2)
data(DataCluster3)
data(DataCluster4)
subStrList <- list(substr1 = getSubStr(DataCluster1),
                   substr2 = getSubStr(DataCluster2),
                   substr3 = getSubStr(DataCluster3),
                   substr4 = getSubStr(DataCluster4))
getCompare(subStrList)
}
