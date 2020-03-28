getScore <- function(h1,i1,b1,m1,s1,e1,
                     seqA,seqCode1,h1Num,i1Num,b1Num,m1Num,s1Num,e1Num,
                     h2,i2,b2,m2,s2,e2,
                     seqB,seqCode2,h2Num,i2Num,b2Num,m2Num,s2Num,e2Num){
  aList <- strsplit(paste(seqA,collapse = ""),"")[[1]]
  aList2 <- strsplit(seqCode1,"")[[1]]
  bList <- strsplit(paste(seqB,collapse = ""),"")[[1]]
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
  result <- list("Similarity" = Similarity*5/6,
                 "longSeq"     = seqCom1, "shortSeq"     = seqCom2,
                 "longSeqCode" = Common1, "shortSeqCode" = Common2)


  return(result)
}
