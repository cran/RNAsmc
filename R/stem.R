stem <- function(ctFile){
  RNAstructure <- matrix(c(as.numeric(ctFile[,5]),as.numeric(ctFile[,6])),ncol = 2,byrow = F)
  stem_list <- list()
  n <- 0
  stem_arr <- c()
  for (i in 1:dim(RNAstructure)[1]) {
    if(RNAstructure[i,1] != 0){
      stem_arr <- c(stem_arr,RNAstructure[i,1],RNAstructure[i,2])
    }else{
      #print(stem_arr)
      if(!is.null(stem_arr)){
        n <- n + 1
        stem_list[[n]] <- sort(stem_arr)
        stem_arr <- c()
      }
    }
    
  }
  
  stem_list <- unique(stem_list)
  if(length(stem_list) == 0){
    print("There is no stem")
    return(stem_list)
  }else{
    
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

