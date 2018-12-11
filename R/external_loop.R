external_loop <- function(ctFile){
  hairpin_loop_1 <- unlist(hairpin_loop(ctFile))
  internal_loop_1 <- unlist(internal_loop(ctFile))
  bulge_loop_1 <- unlist(bulge_loop(ctFile))
  multi_branch_loop_1 <- unlist(multi_branch_loop(ctFile))
  stem_1 <- unlist(stem(ctFile))
  loopArr <- c(hairpin_loop_1,internal_loop_1,bulge_loop_1,multi_branch_loop_1,stem_1)
  externalArr <- c()
  for(i in 1:dim(ctFile)[1]){
    if(!(i %in% loopArr)){
      externalArr <- c(externalArr,i)
    }
  }
  if(length(externalArr) == 0){
    print("There is no external loops")
    externalList <- list()
  }else{
    externalList <- list()
    tempArr <- c(externalArr[1])
    n <- 0
    if(length(externalArr) >= 2){
      for(i in 2:length(externalArr)){
        if(externalArr[i - 1] == (externalArr[i] - 1)){
          tempArr <- c(tempArr,externalArr[i])
        }else{
          n <- n + 1
          externalList[[n]] <- tempArr
          tempArr <- c(externalArr[i])
        }
      }
    }

    n <- n + 1
    externalList[[n]] <- tempArr
    if(length(externalList) == 0){
      return(externalList)
    }else{
      external_Num <- length(externalList)
      external_Max <- length(externalList[[1]])
      external_Min <- length(externalList[[1]])
      for(i in 1:length(externalList)){
        if(length(externalList[[i]]) >= external_Max){
          external_Max <- length(externalList[[i]])
        }
        if(length(externalList[[i]] <= external_Min)){
          external_Min <- length(externalList[[i]])
        }
      }
      external_All <- length(unlist(externalList))
      external_Mean <- external_All / external_Num
      attr(externalList,"number of bases in external loops") <- external_All
      attr(externalList,"number of external loops") <- external_Num
      attr(externalList,"Maximum length of external loops") <- external_Max
      attr(externalList,"Minimum length of external loops") <- external_Min
      attr(externalList,"Average length of external loops") <- external_Mean
  }

  }
  return(externalList)
}

