##bulge_loop
bulge_loop <- function(ctFile){
  RNAstructure <- matrix(c(as.numeric(ctFile[,5]),as.numeric(ctFile[,6])),ncol = 2,byrow = F)
  #print(RNAstructure)
  bulge_loops <- list()
  n <- 1
  arr0 <- RNAstructure[,1]
  for(i in 1:(length(arr0)-1)){
  loop_length <- abs(arr0[i] - arr0[i + 1])
  num_min <- min(arr0[i],arr0[i+1]) + 1
  num_max <- max(arr0[i],arr0[i+1]) - 1
  if(arr0[i] !=0 && arr0[i+1] !=0 && loop_length != 1 && length(which(arr0[num_min:num_max] !=0 )) == 0){
      bulge_loops[[n]] <- num_min:num_max
      n <- n + 1
    }
  }
  
  if(length(bulge_loops) == 0){
    bulge_loops <- list()
    return(bulge_loops)
  }else{
    bulge_number <- length(bulge_loops)
    bulge_max <- length(bulge_loops[[1]])
    bulge_min <- length(bulge_loops[[1]])
    for (i in 1:length(bulge_loops)) {
      if(length(bulge_loops[[i]]) > bulge_max){
        bulge_max <- length(bulge_loops[[i]])
      }
      if(length(bulge_loops[[i]]) < bulge_min){
        bulge_min <- length(bulge_loops[[i]])
      }
    }
    bulge_length <- length(unlist(bulge_loops))
    bulge_mean <- bulge_length/bulge_number
    
    attr(bulge_loops,"number of bases in bulge loops") <- bulge_length
    attr(bulge_loops,"number of bulge loops") <- bulge_number
    attr(bulge_loops,"Maximum length of bulge loops") <- bulge_max
    attr(bulge_loops,"Minimum length of bulge loops") <- bulge_min
    attr(bulge_loops,"Average length of bulge loops") <- bulge_mean
    return(bulge_loops)
  }
  
}
