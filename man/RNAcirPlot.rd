\name{RNAcirPlot}
\alias{RNAcirPlot}
\title{function for plotting RNA secondary structure in a circos style}
\usage{
RNAcirPlot(ctFile,cex = 1,posNum = 2.5,ifNum = FALSE,
            ifBulge = FALSE,ifExternalLoops = FALSE,ifHairpin = FALSE,
            ifInternalLoops = FALSE,ifMultiBranchLoops = FALSE)
}
\arguments{
  \item{ctFile}{A RNA secondary structure file containing structure information}
  \item{cex}{The size of base or number in plotting}
  \item{posNum}{The position of number in plotting. Change the position of number if number is plotted}
  \item{ifNum}{Whether to draw number}
  \item{ifBulge}{Whether to emphasize bulge loops}
  \item{ifExternalLoops}{Whether to emphasize external loops}
  \item{ifHairpin}{Whether to emphasize hairpin loops}
  \item{ifInternalLoops}{Whether to emphasize internal loops}
  \item{ifMultiBranchLoops}{Whether to emphasize multi branch loops}
}

\description{
Given a RNA secondary structure,it plots RNA secondary structure in a circos style
}
\value{
Return a RNA secondary structure in a circos style
}
\author{
Zheng Hewei
}
\examples{
##########
data(DataRNAstr)
RNAcirPlot(DataRNAstr)
}
