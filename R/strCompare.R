strCompare <- function(ctFile1,ctFile2,randomTime = 1000){
  if(dim(ctFile1)[1] >= dim(ctFile2)[1]){
    RNAstructure1 <- ctFile1
    RNAstructure2 <- ctFile2
  }else{
    RNAstructure1 <- ctFile2
    RNAstructure2 <- ctFile1
  }
  ###
  h1 <- hairpin_loop(RNAstructure1)
  hairpin_loop_1 <- unlist(h1)
  h1Num <- length(h1)
  h2 <- hairpin_loop(RNAstructure2)
  hairpin_loop_2 <- unlist(h2)
  h2Num <- length(h2)
  i1 <- internal_loop(RNAstructure1)
  internal_loop_1 <- unlist(i1)
  i1Num <- length(i1)
  i2 <- internal_loop(RNAstructure2)
  internal_loop_2 <- unlist(i2)
  i2Num <- length(i2)
  b1 <- bulge_loop(RNAstructure1)
  bulge_loop_1 <- unlist(b1)
  b1Num <- length(b1)
  b2 <- bulge_loop(RNAstructure2)
  bulge_loop_2 <- unlist(b2)
  b2Num <- length(b2)
  m1 <- multi_branch_loop(RNAstructure1)
  multi_branch_loop_1 <- unlist(m1)
  m1Num <- length(m1)
  m2 <- multi_branch_loop(RNAstructure2)
  multi_branch_loop_2 <- unlist(m2)
  m2Num <- length(m2)
  s1 <- stem(RNAstructure1)
  stem_1 <- unlist(s1)
  s1Num  <- length(s1)
  s2 <- stem(RNAstructure2)
  stem_2 <- unlist(s2)
  s2Num <- length(s2)
  e1 <- external_loop(RNAstructure1)
  external_loop1 <- unlist(e1)
  e1Num  <- length(e1)
  e2 <- external_loop(RNAstructure2)
  external_loop2 <- unlist(e2)
  e2Num <- length(e2)

  ###
  seqCode1 <- ""
  for(index1 in 1:dim(RNAstructure1)[1]){
    if(index1 %in% hairpin_loop_1){
      seqCode1 <- paste0(seqCode1,"H")
    }else if(index1 %in% bulge_loop_1){
      seqCode1 <- paste0(seqCode1,"B")
    }else if(index1 %in% internal_loop_1){
      seqCode1 <- paste0(seqCode1,"I")
    }else if(index1 %in% multi_branch_loop_1){
      seqCode1 <- paste0(seqCode1,"M")
    }else if(index1 %in% stem_1){
      if(RNAstructure1[index1,5] > RNAstructure1[index1,6]){
        seqCode1 <- paste0(seqCode1,"s")
      }else{
        seqCode1 <- paste0(seqCode1,"S")
      }
    }else{
      seqCode1 <- paste0(seqCode1,"E")
    }
  }
  seqCode2 <- ""
  for(index2 in 1:dim(RNAstructure2)[1]){
    if(index2 %in% hairpin_loop_2){
      seqCode2 <- paste0(seqCode2,"H")
    }else if(index2 %in% bulge_loop_2){
      seqCode2 <- paste0(seqCode2,"B")
    }else if(index2 %in% internal_loop_2){
      seqCode2 <- paste0(seqCode2,"I")
    }else if(index2 %in% multi_branch_loop_2){
      seqCode2 <- paste0(seqCode2,"M")
    }else if(index2 %in% stem_2){
      if(RNAstructure2[index2,5] > RNAstructure2[index2,6]){
        seqCode2 <- paste0(seqCode2,"s")
      }else{
        seqCode2 <- paste0(seqCode2,"S")
      }
    }else{
      seqCode2 <- paste0(seqCode2,"E")
    }
  }
  ###

  seqA <- RNAstructure1[,2]
  seqB <- RNAstructure2[,2]

  result0 <- getScore(h1,i1,b1,m1,s1,e1,
                     seqA,seqCode1,h1Num,i1Num,b1Num,m1Num,s1Num,e1Num,
                     h2,i2,b2,m2,s2,e2,
                     seqB,seqCode2,h2Num,i2Num,b2Num,m2Num,s2Num,e2Num)
  arrSimilarity <- c()
  for(i in 1:randomTime){
    randomSeq1 <- sample(strsplit(seqCode1,split = "")[[1]],nchar(seqCode1))
    seqCode1   <- paste(randomSeq1,sep = "",collapse = "")
    randomSeq2 <- sample(strsplit(seqCode2,split = "")[[1]],nchar(seqCode2))
    seqCode2   <- paste(randomSeq2,sep = "",collapse = "")
    result <- getScore(h1,i1,b1,m1,s1,e1,
                       seqA,seqCode1,h1Num,i1Num,b1Num,m1Num,s1Num,e1Num,
                       h2,i2,b2,m2,s2,e2,
                       seqB,seqCode2,h2Num,i2Num,b2Num,m2Num,s2Num,e2Num)
    arrSimilarity <- c(arrSimilarity,result$Similarity)
  }
  pValue <- 1 - pnorm(result0$Similarity,mean = mean(arrSimilarity),sd = sd(arrSimilarity))
  result2 <- list("Similarity" = result0$Similarity,"Pvalue" = pValue,
                  "longSeq" = result0$longSeq,"shortSeq" = result0$shortSeq,
                  "longSeqCode" = result0$longSeqCode,"shortSeqCode" = result0$shortSeqCode)
  ###################
  return(result2)
}

