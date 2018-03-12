stem <- function(ctFile){
  RNAstructure <- matrix(c(as.numeric(ctFile[,5]),as.numeric(ctFile[,6])),ncol = 2,byrow = F)
  stem_list <- list()
  n <- 0
  stem_arr <- c()
  indexPair <- which(RNAstructure[,1] != 0)
  numPair <- length(indexPair)
  if(numPair == 0){
    print("There is no stems")
    return(stem_list)
  }else{
    if(numPair == 1){
      n <- n + 1
      stem_arr <- c(stem_arr,RNAstructure[indexPair,1],RNAstructure[indexPair,2])
      stem_list[[n]] <- sort(stem_arr)
    }else if(numPair >= 2){
      for(index in 2:numPair){
        if(abs(RNAstructure[indexPair[index],1] - RNAstructure[indexPair[index - 1],1]) == 1
           && abs(RNAstructure[indexPair[index],2] - RNAstructure[indexPair[index - 1],2]) == 1){
          stem_arr <- c(stem_arr,RNAstructure[indexPair[index],1],RNAstructure[indexPair[index],2])
          stem_arr <- c(stem_arr,RNAstructure[indexPair[index - 1],1],RNAstructure[indexPair[index - 1],2])
        }else{
          listL <- length(stem_list)
          if(listL == 0){
            n <- n + 1
            stem_arr <- sort(unique(stem_arr))
            stem_list[[n]] <- stem_arr
            stem_arr <- c()
            stem_arr <- c(stem_arr,RNAstructure[indexPair[index],1],RNAstructure[indexPair[index],2])
          }else{
            count <- 0
            for(i in 1:listL){
              if(length(setdiff(stem_arr,stem_list[[i]])) == 0
                 && length(setdiff(stem_list[[i]],stem_arr)) == 0){
                count <- count + 1
              }
            }
            if(count == 0){
              n <- n + 1
              stem_arr <- sort(unique(stem_arr))
              stem_list[[n]] <- stem_arr
              stem_arr <- c()
              stem_arr <- c(stem_arr,RNAstructure[indexPair[index],1],RNAstructure[indexPair[index],2])
            }else{
              stem_arr <- c()
              stem_arr <- c(stem_arr,RNAstructure[indexPair[index],1],RNAstructure[indexPair[index],2])
            }
          }
        }
      }
    }
    stem_number <- length(stem_list)
    stem_max <- length(stem_list[[1]])
    stem_min <- length(stem_list[[1]])
    for (i in 1:length(stem_list)) {
      if(length(stem_list[[i]]) > stem_max){
        stem_max <- length(stem_list[[i]])
      }
      if(length(stem_list[[i]]) < stem_min){
        stem_min <- length(stem_list[[i]])
      }
    }
    stem_length <- length(unlist(stem_list))
    stem_mean <- stem_length/stem_number
    attr(stem_list,"number of bases in stems") <- stem_length
    attr(stem_list,"number of stems") <- stem_number
    attr(stem_list,"Maximum length of stems") <- stem_max
    attr(stem_list,"Minimum length of stems") <- stem_min
    attr(stem_list,"Average length of stems") <- stem_mean
    return(stem_list)
  }
}
