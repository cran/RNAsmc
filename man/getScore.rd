\name{getScore}
\alias{getScore}
\title{Internal function for getting similarity score in a RNA secondary structure}
\usage{
getScore(h1,i1,b1,m1,s1,e1,
         seqA,seqCode1,h1Num,i1Num,b1Num,m1Num,s1Num,e1Num,
         h2,i2,b2,m2,s2,e2,
         seqB,seqCode2,h2Num,i2Num,b2Num,m2Num,s2Num,e2Num)
}
\arguments{
  \item{h1}{hairpin_loop result A}
  \item{i1}{internal_loop result A}
  \item{b1}{bulge_loop result A}
  \item{m1}{multi_branch_loop result A}
  \item{s1}{stem result A}
  \item{e1}{external_loop result A}
  \item{seqA}{sequence A}
  \item{seqCode1}{coding sequence A}
  \item{h1Num}{hairpin_loop number A}
  \item{i1Num}{internal_loop number A}
  \item{b1Num}{bulge_loop number A}
  \item{m1Num}{multi_branch_loop number A}
  \item{s1Num}{stem number A}
  \item{e1Num}{external_loop number A}
  \item{h2}{hairpin_loop result B}
  \item{i2}{internal_loop result B}
  \item{b2}{bulge_loop result B}
  \item{m2}{multi_branch_loop result B}
  \item{s2}{stem result B}
  \item{e2}{external_loop result B}
  \item{seqB}{sequence B}
  \item{seqCode2}{coding sequence B}
  \item{h2Num}{hairpin_loop number B}
  \item{i2Num}{internal_loop number B}
  \item{b2Num}{bulge_loop number B}
  \item{m2Num}{multi_branch_loop number B}
  \item{s2Num}{stem number B}
  \item{e2Num}{external_loop number B}
}
\description{
Internal function for getting similarity score in a RNA secondary structure
}
\value{
Return a list countains similarity score and other information
}
\author{
Zheng Hewei
}
\examples{
##########
#nothing
}
