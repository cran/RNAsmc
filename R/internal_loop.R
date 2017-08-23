internal_loop <- function(ctFile){
  RNAstructure <- matrix(c(as.numeric(ctFile[,5]),as.numeric(ctFile[,6])),ncol = 2,byrow = F)
  internal_loops <- list()
  n <- 1
  boundStart <- 0
  boundEnd <- 0
  boundStart2 <- 0
  boundEnd2 <- 0
  loops <- c()
  for (i in 1:(dim(RNAstructure)[1] - 1)) {
    if(RNAstructure[i,1] != 0 && RNAstructure[i+1,1] == 0){
      boundStart <- i
     }
    if(RNAstructure[i,1] == 0 && RNAstructure[i+1,1] != 0){
      boundEnd <- i + 1
    }
    if(boundStart == 0 && boundEnd != 0){
      boundEnd <- 0
    }
    if(boundStart != 0 && boundEnd !=0){
      #print(paste(boundStart,"_",boundEnd))
      boundStart2 <- RNAstructure[boundStart,1]
      boundEnd2 <- RNAstructure[boundEnd,1]
      num_min <- min(boundStart2,boundEnd2) + 1
      num_max <- max(boundStart2,boundEnd2) - 1
      loop <- RNAstructure[num_min:num_max,1]
      if(length(which(loop != 0)) == 0 && (boundStart + 1) != num_min && (boundEnd - 1) != num_max){
        if(length(which(loops == (min(boundStart,boundEnd) + 1))) == 0){
          loops <- c(loops,boundStart,boundEnd,num_min,num_max)
          internal_loops[[n]] <- sort(c(num_min:num_max,(min(boundStart,boundEnd)+1):(max(boundStart,boundEnd)-1)))
          n <- n + 1
        }
      }
      boundStart <- 0
      boundEnd <- 0
      boundStart2 <- 0
      boundEnd2 <- 0
    }
  }
  if(length(internal_loops) == 0){
    #print("There is no intrenal loop")
    return(internal_loops)
  }else{
    
    internal_number <- length(internal_loops)
    internal_max <- length(internal_loops[[1]])
    internal_min <- length(internal_loops[[1]])
    for (i in 1:length(internal_loops)) {
      if(length(internal_loops[[i]]) > internal_max){
        internal_max <- length(internal_loops[[i]])
      }
      if(length(internal_loops[[i]]) < internal_min){
        internal_min <- length(internal_loops[[i]])
      }
    }
    internal_length <- length(unlist(internal_loops))
    internal_mean <- internal_length/internal_number
    
    attr(internal_loops,"number of bases in internal loops") <- internal_length
    attr(internal_loops,"number of internal loops") <- internal_number
    attr(internal_loops,"Maximum length of internal loops") <- internal_max
    attr(internal_loops,"Minimum length of internal loops") <- internal_min
    attr(internal_loops,"Average length of internal loops") <- internal_mean
    
    return(internal_loops)
  }
  
}

