\name{RNAstrCluster}
\alias{RNAstrCluster}
\title{A function for RNAs clustering by their structure similarites}
\usage{
RNAstrCluster(ctFiles)
}
\arguments{
  \item{ctFiles}{
A list contain RNA structures needed to cluster
}
}
\description{
Given a RNA secondary structure,it compute simility of these RNAs and cluster them
}
\value{
Return simility matrix and cluster tree
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
###
a <- list(str1=DataCluster1,str2=DataCluster2,str3=DataCluster3,str4=DataCluster4)
RNAstrCluster(a)
}
