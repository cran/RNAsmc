RNAstrCluster <- function(ctFiles = list()){
  num_str <- length(ctFiles)
  if(num_str <= 2){
    return("There is less than 2 RNA structure,and could not cluster")
  }else{
    mat_score <- matrix(rep(0,num_str*num_str),nrow = num_str,byrow = T)
    for (i in 1:num_str) {
      for (j in 1:num_str) {
        print("RNAs being compared:")
        print(paste0("RNA a: ",i," RNA b: ",j))
        score <- compare(ctFiles[[i]],ctFiles[[j]])
        mat_score[i,j] <- score
      }
    }
    rownames(mat_score) <- names(ctFiles)
    colnames(mat_score) <- names(ctFiles)
    d_mat <- stats::dist(mat_score)
    d2 <- stats::hclust(d_mat)
    graphics::plot(d2,hang = -1,xlab = "cluster of RNAs",ylab = "height")
    result <- list(simility_mat = mat_score,cluster_tree = d2)
    return(result)
  }
  
}