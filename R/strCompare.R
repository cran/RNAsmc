strCompare <- function(ctFile1,ctFile2){
  if(dim(ctFile1)[1] >= dim(ctFile2)[1]){
    RNAstructure1 <- ctFile1
    RNAstructure2 <- ctFile2
  }else{
    RNAstructure1 <- ctFile2
    RNAstructure2 <- ctFile1
  }
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
  seqA <- RNAstructure1[,2]
  seqB <- RNAstructure2[,2]
  aList <- strsplit(paste(RNAstructure1[,2],collapse = ""),"")[[1]]
  aList2 <- strsplit(seqCode1,"")[[1]]
  bList <- strsplit(paste(RNAstructure2[,2],collapse = ""),"")[[1]]
  bList2 <- strsplit(seqCode2,"")[[1]]
  l_1 <- nchar(seqCode1)
  l_2 <- nchar(seqCode2)
  score <- matrix(sample(0,(l_1+1)*(l_2+1),replace = T),nrow = (l_1 + 1),ncol = (l_2+1))
  StateM <- matrix(sample(0,(l_1+1)*(l_2+1),replace = T),nrow = (l_1 + 1),ncol = (l_2+1))

  for(i in 1:(l_1+1)){
    score[i,1] <- 0
  }
  for(j in 1:(l_2+1)){
    score[1,j] <- 0
  }
  for(i in 2:(l_1+1)){
    for(j in 2:(l_2+1)){
      if(aList2[i-1] == bList2[j-1]){
        score[i,j] <- score[i-1,j-1] + 5
        StateM[i,j] <- 1
      }else if(score[i-1,j] >= score[i,j-1]){
        score[i,j] <- score[i-1,j] - 4
        StateM[i,j] <- 2
      }else{
        score[i,j] <- score[i,j-1]  - 4
        StateM[i,j]<- 3
      }
    }
  }
  ###############
  Common1 <- ""
  Common2 <- ""
  seqCom1 <- ""
  seqCom2 <- ""
  posLast <- which(t(score) == max(score))[1]
  if(posLast %% dim(score)[2] == 0){
    i <- posLast/dim(score)[2]
    if(dim(score)[1] > i){
      row1 <- dim(score)[1]
      while (row1 > i) {
        Common1 <- paste0(aList2[row1-1],Common1)
        Common2 <- paste0("-",Common2)
        seqCom1 <- paste0(seqA[row1-1],seqCom1)
        seqCom2 <- paste0("-",seqCom2)
        row1 <- row1 - 1
      }
    }
  }else{
    i <- l_1 + 1
  }
  j <- l_2 + 1

  while (i > 1 && j >  1) {
    if(StateM[i,j] == 1){
      Common1 <- paste0(aList2[i-1],Common1)
      Common2 <- paste0(bList2[j-1],Common2)
      seqCom1 <- paste0(seqA[i-1],seqCom1)
      seqCom2 <- paste0(seqB[j-1],seqCom2)
      i <- i - 1
      j <- j - 1
    }else if(StateM[i,j] == 2){
      Common1 <- paste0(aList2[i-1],Common1)
      Common2 <- paste0("-",Common2)
      seqCom1 <- paste0(seqA[i-1],seqCom1)
      seqCom2 <- paste0("-",seqCom2)
      i <- i - 1
    }else if (StateM[i,j] == 3){
      Common1 <- paste0("-",Common1)
      Common2 <- paste0(bList2[j-1],Common2)
      seqCom1 <- paste0("-",seqCom1)
      seqCom2 <- paste0(seqB[j-1],seqCom2)
      j <- j - 1
    }
  }
  if(i > 1){
    while(i > 1){
      Common1 <- paste0(aList2[i-1],Common1)
      Common2 <- paste0("-",Common2)
      seqCom1 <- paste0(seqA[i-1],seqCom1)
      seqCom2 <- paste0("-",seqCom2)
      i <- i - 1
    }
  }else if(j > 1){
    while(j > 1){
      Common1 <- paste0("-",Common1)
      Common2 <- paste0(bList2[j-1],Common2)
      seqCom1 <- paste0("-",seqCom1)
      seqCom2 <-  paste0(seqB[j-1],seqCom2)
      j <- j - 1
    }
  }
  Common1Split <- strsplit(Common1,split = "")[[1]]
  Common2Split <- strsplit(Common2,split = "")[[1]]
  for(start1 in 1:length(Common1Split)){
    if(Common2Split[start1] != "-"){
      break()
    }
  }
  for(end1 in length(Common1Split):1){
    if(Common2Split[end1] != "-"){
      break()
    }
  }
  ##############
  arr1Num <- c()
  n = 0
  for(num1 in 1:length(Common1Split)){
    if(Common1Split[num1] == "-"){
      arr1Num <- c(arr1Num,0)
    }else{
      n <- n + 1
      arr1Num <- c(arr1Num,n)
    }
  }
  arrPos <- arr1Num[start1:end1]
  h1Num2 <- 0
  if(h1Num != 0){
    for(i in 1:h1Num){
      if(length(intersect(h1[[i]],arrPos)) != 0){
        h1Num2 <- h1Num2 + 1
      }
    }
  }
  b1Num2 <- 0
  if(b1Num != 0){
    for(i in 1:b1Num){
      if(length(intersect(b1[[i]],arrPos)) != 0){
        b1Num2 <- b1Num2 + 1
      }
    }
  }
  e1Num2 <- 0
  if(e1Num != 0){
    for(i in 1:e1Num){
      if(length(intersect(e1[[i]],arrPos)) != 0){
        e1Num2 <- e1Num2 + 1
      }
    }
  }
  i1Num2 <- 0
  if(i1Num != 0){
    for(i in 1:i1Num){
      if(length(intersect(i1[[i]],arrPos)) != 0){
        i1Num2 <- i1Num2 + 1
      }
    }
  }
  m1Num2 <- 0
  if(m1Num != 0){
    for(i in 1:m1Num){
      if(length(intersect(m1[[i]],arrPos)) != 0){
        m1Num2 <- m1Num2 + 1
      }
    }
  }
  s1Num2 <- 0
  if(s1Num != 0){
    for(i in 1:s1Num){
      if(length(intersect(s1[[i]],arrPos)) != 0){
        s1Num2 <- s1Num2 + 1
      }
    }
  }


  h1Arr <- c()
  b1Arr <- c()
  e1Arr <- c()
  i1Arr <- c()
  m1Arr <- c()
  s1Arr <- c()
  for(i1 in start1:end1){
    if(Common1Split[i1] == "H"){
      h1Arr <- c(h1Arr,i1)
    }else if(Common1Split[i1] == "B"){
      b1Arr <- c(b1Arr,i1)
    }else if(Common1Split[i1] == "E"){
      e1Arr <- c(e1Arr,i1)
    }else if(Common1Split[i1] == "I"){
      i1Arr <- c(i1Arr,i1)
    }else if(Common1Split[i1] == "M"){
      m1Arr <- c(m1Arr,i1)
    }else if((Common1Split[i1] == "S") || (Common1Split[i1] == "s")){
      s1Arr <- c(s1Arr,i1)
    }
  }

  h2Arr <- c()
  b2Arr <- c()
  e2Arr <- c()
  i2Arr <- c()
  m2Arr <- c()
  s2Arr <- c()
  for(i2 in start1:end1){
    if(Common2Split[i2] == "H"){
      h2Arr <- c(h2Arr,i2)
    }else if(Common2Split[i2] == "B"){
      b2Arr <- c(b2Arr,i2)
    }else if(Common2Split[i2] == "E"){
      e2Arr <- c(e2Arr,i2)
    }else if(Common2Split[i2] == "I"){
      i2Arr <- c(i2Arr,i2)
    }else if(Common2Split[i2] == "M"){
      m2Arr <- c(m2Arr,i2)
    }else if((Common2Split[i2] == "S") || (Common2Split[i2] == "s")){
      s2Arr <- c(s2Arr,i2)
    }
  }
  Similarity <- 0
  if(length(union(h1Arr,h2Arr)) == 0){
    Similarity <- Similarity + 2
  }else{
    Similarity <- Similarity + length(intersect(h1Arr,h2Arr))/length(union(h1Arr,h2Arr)) + min(h1Num2,h2Num)/max(h1Num2,h2Num)
  }
  if(length(union(b1Arr,b2Arr)) == 0){
    Similarity <- Similarity + 2
  }else{
    Similarity <- Similarity +  length(intersect(b1Arr,b2Arr))/length(union(b1Arr,b2Arr)) + min(b1Num2,b2Num)/max(b1Num2,b2Num)

  }
  if(length(union(e1Arr,e2Arr)) == 0){
    Similarity <- Similarity + 2
  }else{
    Similarity <- Similarity + length(intersect(e1Arr,e2Arr))/length(union(e1Arr,e2Arr)) + min(e1Num2,e2Num)/max(e1Num2,e2Num)

  }
  if(length(union(i1Arr,i2Arr)) == 0){
    Similarity <- Similarity + 2
  }else{
    Similarity <- Similarity + length(intersect(i1Arr,i2Arr))/length(union(i1Arr,i2Arr))+ min(i1Num2,i2Num)/max(i1Num2,i2Num)

  }
  if(length(union(m1Arr,m2Arr)) == 0){
    Similarity <- Similarity + 2
  }else{
    Similarity <- Similarity + length(intersect(m1Arr,m2Arr))/length(union(m1Arr,m2Arr)) + min(m1Num2,m2Num)/max(m1Num2,m2Num)

  }
  if(length(union(s1Arr,s2Arr)) == 0){
    Similarity <- Similarity + 2
  }else{
    Similarity <- Similarity + length(intersect(s1Arr,s2Arr))/length(union(s1Arr,s2Arr)) + min(s1Num2,s2Num)/max(s1Num2,s2Num)

  }
  result <- list("Similarity" = Similarity*5/6,"longSeq" = seqCom1,"shortSeq" = seqCom2,"longSeqCode" = Common1,"shortSeqCode" = Common2)
  ###################
  return(result)
}

