##ct2dot
ct2dot <- function(ctFile){
  ctFile <- as.matrix(ctFile)
  base <- ctFile[,2]
  dot <- list()
  for (i in 1:dim(ctFile)[1]) {
    if(as.numeric(ctFile[i,5]) == 0){
      dot[[i]] <- "."
    }else if(as.numeric(ctFile[i,6]) < as.numeric(ctFile[i,5])){
      dot[[i]] <- "("
      dot[[as.numeric(ctFile[i,5])]] <- ")"
    }
  }
  dot <- paste(unlist(dot),collapse = "",sep = "")
  base <- paste(base,collapse = "",sep = "")
  re_list <- list()
  re_list[[1]] <- dot
  re_list[[2]] <- base
  return(re_list)
}
