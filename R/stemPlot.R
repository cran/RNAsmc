##plot hairpin loops
stemPlot <- function(ctFile){

  dot <- ct2dot(ctFile)
  ct <- makeCt(dot[[1]][1],dot[[2]][1])
  co <- ct2coord(ct)

  loops <- stem(ctFile)

  arr_min <- c()
  arr_max <- c()

  if(length(loops) != 0){
    for (i in 1:length(loops)) {
      arr_min <- c(arr_min,loops[[i]][1])
      if(length(loops[[i]]) > 1){
        for (j in 2:length(loops[[i]])) {
          if(loops[[i]][j-1] == loops[[i]][j] - 1){

          }else{
            arr_max <- c(arr_max,loops[[i]][j-1])
            arr_min <- c(arr_min,loops[[i]][j])
          }
        }
        arr_max <- c(arr_max,loops[[i]][j])
      }else{
        arr_max <- c(arr_max,loops[[i]][1])
      }

    }

    ranges=data.frame(min=arr_min,max=arr_max,col=19,
                      desc="stems"
    )
    RNAPlot(co,ranges,labTF=F,main = "RNA secondary structure")
    
    print("------------------------------------------------------")
    print("*                                                    *")
    print("*   what print below is the information of result    *")
    print("*                                                    *")
    print("------------------------------------------------------")
    #print("summary of stems:")
    #print(paste("the number of stems is:",length(loops)))
    #print(paste("the bases in stems are:",paste(unlist(loops),collapse = " ",sep = "")))
    loops_name <- c()
    for (i in 1:length(loops)) {
      loops_name <- c(loops_name,paste("bases in stem ",i))
    }
    names(loops) <- loops_name
    length_loop <- length(unlist(loops))
    max_length <- length(loops[[1]])
    min_length <- length(loops[[1]])
    for (i in 1:length(loops)) {
      if(length(loops[[i]]) > max_length){
        max_length <- length(loops[[i]])
      }
      if(length(loops[[i]]) < min_length){
        min_length <- length(loops[[i]])
      }
    }
    attr(loops,"1.Number of base pairs in stems") <- length_loop
    attr(loops,"2.Number of stems") <- length(loops)
    attr(loops,"3.Average length of a stem") <- length_loop/length(loops)
    attr(loops,"4.Maximum length of a stem") <- max_length
    attr(loops,"5.Minimum length of a stem") <- min_length
    attr(loops,"6.Base positions in stems") <- paste(unlist(loops),collapse = " ",sep = "")
    return(loops)
  }else{
    print("------------------------------------------------------")
    print("*                                                    *")
    print("*   what print below is the information of result    *")
    print("*                                                    *")
    print("------------------------------------------------------")
    print("There is no stem")
  }

}


