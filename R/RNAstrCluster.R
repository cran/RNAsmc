##get subStrucutre
getSubStr <- function(ctfile){
  subStrArr <- c()
  bResult <- bulge_loop(ctfile)
  bNum <- length(bResult)
  subStrArr <- c(subStrArr,bNum)
  eResult <- external_loop(ctfile)
  eNum <- length(eResult)
  subStrArr <- c(subStrArr,eNum)
  hResult <- hairpin_loop(ctfile)
  hNum <- length(hResult)
  subStrArr <- c(subStrArr,hNum)
  iResult <- internal_loop(ctfile)
  iNum <- length(iResult)
  subStrArr <- c(subStrArr,iNum)
  mResult <- multi_branch_loop(ctfile)
  mNum <- length(mResult)
  subStrArr <- c(subStrArr,mNum)
  sResult <- stem(ctfile)
  sNum <- length(sResult)
  subStrArr <- c(subStrArr,sNum)
  indexList <- list("numArr" = subStrArr,"bulgeLoop" = bResult,"externalLoop" = eResult,
                    "hairpinLoop" = hResult,"internalLoop" = iResult,
                    "multip-branchLoop" = mResult,"stem" = sResult,"ctFile" = ctfile)
  return(indexList)
}
##get comparison results
getCompare <- function(subStrList){
  numStr <- length(subStrList)
  mat_score <- matrix(rep(0,numStr*numStr),nrow = numStr,byrow = T)
  for(i in 1:numStr){
    mat_score[i,i] <- 10
  }
  for(index_1 in 1:(numStr - 1)){
    for(index_2 in (index_1 + 1):numStr){
      # print(paste0("Being compared RNA: ",index_1," and RNA: ",index_2))
      tempList1 <- list()
      tempList2 <- list()
      if(dim(subStrList[[index_1]]$"ctFile")[1] >= dim(subStrList[[index_2]]$"ctFile")[1]){
        RNAstructure1 <- subStrList[[index_1]]$"ctFile"
        RNAstructure2 <- subStrList[[index_2]]$"ctFile"
        tempList1 <- subStrList[[index_1]]
        tempList2 <- subStrList[[index_2]]
      }else{
        RNAstructure1 <- subStrList[[index_2]]$"ctFile"
        RNAstructure2 <- subStrList[[index_1]]$"ctFile"
        tempList1 <- subStrList[[index_2]]
        tempList2 <- subStrList[[index_1]]
      }
      seqCode1 <- ""
      for(index1 in 1:dim(RNAstructure1)[1]){
        if(index1 %in% unlist(tempList1$"hairpinLoop")){
          seqCode1 <- paste0(seqCode1,"H")
        }else if(index1 %in% unlist(tempList1$"bulgeLoop")){
          seqCode1 <- paste0(seqCode1,"B")
        }else if(index1 %in% unlist(tempList1$"internalLoop")){
          seqCode1 <- paste0(seqCode1,"I")
        }else if(index1 %in% unlist(tempList1$"multip-branchLoop")){
          seqCode1 <- paste0(seqCode1,"M")
        }else if(index1 %in% unlist(tempList1$"stem")){
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
        if(index2 %in% unlist(tempList2$"hairpinLoop")){
          seqCode2 <- paste0(seqCode2,"H")
        }else if(index2 %in% unlist(tempList2$"bulgeLoop")){
          seqCode2 <- paste0(seqCode2,"B")
        }else if(index2 %in% unlist(tempList2$"internalLoop")){
          seqCode2 <- paste0(seqCode2,"I")
        }else if(index2 %in% unlist(tempList2$"multip-branchLoop")){
          seqCode2 <- paste0(seqCode2,"M")
        }else if(index2 %in% unlist(tempList2$"stem")){
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
      ###
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
      h1Num <- tempList1$"numArr"[3]
      h1 <- tempList1$hairpinLoop
      
      h1Num2 <- 0
      if(h1Num != 0){
        for(i in 1:h1Num){
          if(length(intersect(h1[[i]],arrPos)) != 0){
            h1Num2 <- h1Num2 + 1
          }
        }
      }
      b1Num <- tempList1$"numArr"[1]
      b1 <- tempList1$"bulgeLoop"
      b1Num2 <- 0
      if(b1Num != 0){
        for(i in 1:b1Num){
          if(length(intersect(b1[[i]],arrPos)) != 0){
            b1Num2 <- b1Num2 + 1
          }
        }
      }
      e1Num <- tempList1$"numArr"[2]
      e1 <- tempList1$"externalLoop"
      e1Num2 <- 0
      if(e1Num != 0){
        for(i in 1:e1Num){
          if(length(intersect(e1[[i]],arrPos)) != 0){
            e1Num2 <- e1Num2 + 1
          }
        }
      }
      i1Num <- tempList1$"numArr"[4]
      i1 <- tempList1$"internalLoop"
      i1Num2 <- 0
      if(i1Num != 0){
        for(i in 1:i1Num){
          if(length(intersect(i1[[i]],arrPos)) != 0){
            i1Num2 <- i1Num2 + 1
          }
        }
      }
      m1Num <- tempList1$"numArr"[5]
      m1 <- tempList1$"multip-branchLoop"
      m1Num2 <- 0
      if(m1Num != 0){
        for(i in 1:m1Num){
          if(length(intersect(m1[[i]],arrPos)) != 0){
            m1Num2 <- m1Num2 + 1
          }
        }
      }
      s1Num <- tempList1$"numArr"[6]
      s1 <- tempList1$"stem"
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
      b2Num <- tempList2$"numArr"[1]
      e2Num <- tempList2$"numArr"[2]
      h2Num <- tempList2$"numArr"[3]
      i2Num <- tempList2$"numArr"[4]
      m2Num <- tempList2$"numArr"[5]
      s2Num <- tempList2$"numArr"[6]
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
      mat_score[index_1,index_2] <- result$"Similarity"
      mat_score[index_2,index_1] <- result$"Similarity"
    }
  }
  return(mat_score)
}


RNAstrCluster <- function(ctFiles = list()){
  num_str <- length(ctFiles)
  ctNames <- names(ctFiles)
  if(num_str <= 2){
    return("There is less than 2 RNA structure,and could not cluster")
  }else{
    subStrList <- list()
    seq_length <- c()
    for(index in 1:num_str){
      subStrList[[index]] <- getSubStr(ctFiles[[index]])
      seq_length <- c(seq_length, nrow(ctFiles[[index]]))
    }
    mat_score <- getCompare(subStrList)
    
    mat_score <- mat_score[order(seq_length), order(seq_length)] 
    rownames(mat_score) <- ctNames[order(seq_length)]
    colnames(mat_score) <- ctNames[order(seq_length)]
    
    share_list <- list()
    list_index <- 0
    for(row_index in 1:nrow(mat_score)){
      col_index <- which(mat_score[row_index, ] == 10)
      if(length(col_index) >= 2){
        if(length(share_list) == 0){
          list_index <- list_index + 1
          share_list[[list_index]] <- sort(colnames(mat_score)[col_index])
        }else{
          ifIN <- 0
          for(index in 1:length(share_list)){
            num_in <- c()
            num_out <- c()
            for(i in col_index){
              if(colnames(mat_score)[i] %in% share_list[[index]]){
                num_in <- c(num_in, i)
              }else{
                num_out <- c(num_out, i)
              }
            }
            
            if(length(num_in) != 0){
              ifIN <- ifIN + 1
              if(length(num_out) != 0){
                share_list[[index]] <- c(share_list[[index]], colnames(mat_score)[num_out])
                break
              }
            }
          }
          if(ifIN == 0){
            list_index <- list_index + 1
            share_list[[list_index]] <- sort(colnames(mat_score)[col_index])
          }
        }
      }
    }
    share_set <- c()
    if(length(share_list)){
      for(i in 1:length(share_list)){
        share_line <- paste(share_list[[i]], sep = ",", collapse = ",")
        share_set <- c(share_set, share_line)
      }
      share_set <- unique(share_set)
      print("********************************************")
      print("tips:")
      for(share_group in sort(share_set)){
        cts <- strsplit(share_group, split = ",")[[1]]
        ct_rows <- c()
        for(ct in cts){
          ct_rows <- c(ct_rows, nrow(ctFiles[[ct]]))
        }
        if(length(unique(ct_rows)) == 1){
          print(paste0(share_group, " share common global structure"))    
        }else{
          print(paste0(share_group, " share common local structure"))    
        }
      }
      print("********************************************")
    }
    
    d_mat <- stats::as.dist(10 - mat_score)
    d2 <- stats::hclust(d_mat)
    graphics::plot(d2,hang = -1,xlab = "cluster of RNAs",ylab = "height")
    result <- list(simility_mat = mat_score)
    return(result)
  }
}
