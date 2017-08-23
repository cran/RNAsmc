####multi_branch_loop
multi_branch_loop <- function(ctFile){
  hairpin_bases <- unlist(hairpin_loop(ctFile))
  bulge_bases <- unlist(bulge_loop(ctFile))
  internal_bases <- unlist(internal_loop(ctFile))
  ctFile <- matrix(c(as.numeric(ctFile[,5]),as.numeric(ctFile[,6])),ncol = 2,byrow = F)
  pair_bases <- which(ctFile[,1] != 0)
  known_bases <- c(hairpin_bases,bulge_bases,internal_bases,pair_bases)
  multi_bases <- setdiff(ctFile[,2],known_bases)
  #
  if(length(multi_bases) != 0){
    if(length(which(multi_bases == 1)) !=0){
      num <- 1
      while(length(which(multi_bases == num)) != 0) {
        multi_bases <- multi_bases[-which(multi_bases == num)]
        num <- num + 1
      }
    }
    if(length(which(multi_bases == dim(ctFile)[1])) != 0){
      num <- dim(ctFile)[1]
      while (length(which(multi_bases == num)) != 0) {
        multi_bases <- multi_bases[-which(multi_bases == num)]
        num <- num - 1
      }
    }
    
    if(length(multi_bases) != 0){
      ##################
      links <- list()
      links_index <- 0
      for (i in 1:dim(ctFile)[1]) {
        if(ctFile[i,1] != 0){
          links_index <- links_index + 1
          links[[links_index]] <- c(ctFile[i,1],ctFile[i,2])
        }
      }
      length2 <- length(links) - 1
      while(length(links) != length2){
        length2 <- length(links)
        for (j in 1:length(links)) {
          for (k in 1:length(links)) {
            a <- min(links[[j]])
            b <- max(links[[j]])
            c <- min(links[[k]])
            d <- max(links[[k]])
            if((abs(a-c)==1&&abs(b-d)!=1)||(abs(a-d)==1&&abs(b-c)!=1)||(abs(b-c)==1&&abs(a-d)!=1)||(abs(b-d)==1&&abs(a-c)!=1)){
              links_index <- links_index + 1
              links[[links_index]] <- c(a,b,c,d)
              links <- unique(links)
              for (m in 1:length(links)) {
                if(length(links[m][[1]])==0){
                  links[m]<- NULL
                }
              }
              links <- unique(links)
            }
          }
        }
      }
      ########
      multi_bases_2 <- multi_bases
      links_2 <- list()
      base1 <- c(multi_bases_2[1])
      n <- 1
      multi_bases_2 <- multi_bases_2[-1]
      while (length(multi_bases_2) != 0) {
        if(multi_bases_2[1] == (max(base1) + 1)){
          base1 <- c(base1,multi_bases_2[1])
        }else{
          links_2[[n]] <- base1
          n <- n + 1
          base1 <- c(multi_bases_2[1])
        }
        multi_bases_2 <- multi_bases_2[-1]
      }
      links_2[[n]] <- base1
      links_4 <- list(c(0))
      #######
      while (!identical(links_4,links_2) &&  length(links_2) > 1) {
        links_4 <- links_2
        for (i in 1:(length(links_2) - 1)) {
          for (j in (i + 1):length(links_2)) {
            for (k in 1:length(links)) {
              if(((max(links_2[[i]]) == (min(links[[k]]) - 1)) 
                  && (min(links_2[[j]]) == (max(links[[k]]) + 1)))
                 ||((min(links_2[[i]]) == (min(links[[k]]) + 1)) 
                    && (max(links_2[[j]]) == (max(links[[k]]) - 1)))){
                links_2[[j]] <- c(links_2[[i]],links_2[[j]])
                links_2[[i]] <- c(0)
              }
              
            }
            
          }
        }
      } 
      links_min <- min(unlist(links))
      links_max <- max(unlist(links))
      for (index1 in 1:length(links_2)) {
        for (index2 in 1:length(links)) {
          if((max(links_2[[index1]]) == (min(links[[index2]]) - 1))
             || (min(links_2[[index1]]) == (max(links[[index2]]) + 1))){
            if((links_max %in% links[[index2]]) || (links_min %in% links[[index2]])){
              links_2[[index1]] <- c(0)
            }
          }
        }
      }
      for (i in 1:length(links_2)) {
        if(length(links_2[[i]]) == 1 && links_2[[i]] == 0){
          links_2[[i]] <- NA
        }
      }
      links_2 <- links_2[!is.na(links_2)]
      
      multi_bases <- unique(links_2)
      if(length(links_2) == 0){
        return(links_2)
      }else{
        
        multi_branch_number <- length(links_2)
        multi_branch_max <- length(links_2[[1]])
        multi_branch_min <- length(links_2[[1]])
        for (i in 1:length(links_2)) {
          if(length(links_2[[i]]) > multi_branch_max){
            multi_branch_max <- length(links_2[[i]])
          }
          if(length(links_2[[i]]) < multi_branch_min){
            multi_branch_min <- length(links_2[[i]])
          }
        }
        multi_branch_length <- length(unlist(links_2))
        multi_branch_mean <- multi_branch_length/multi_branch_number
        
        attr(links_2,"number of bases in multi_branch loops") <- multi_branch_length
        attr(links_2,"number of multi_branch loops") <- multi_branch_number
        attr(links_2,"Maximum length of multi_branch loops") <- multi_branch_max
        attr(links_2,"Minimum length of multi_branch loops") <- multi_branch_min
        attr(links_2,"Average length of multi_branch loops") <- multi_branch_mean
        
        return(links_2)
      }
    }else{
      links_2 <- list()
      return(links_2)
    }
  }else{
    links_2 <- list()
    return(links_2)
  }
}