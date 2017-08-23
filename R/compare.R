compare <- function(ctFile1,ctFile2){
  RNAstructure1 <- ctFile1
  RNAstructure2 <- ctFile2
  hairpin_loop_1 <- unlist(hairpin_loop(RNAstructure1))
  hairpin_loop_2 <- unlist(hairpin_loop(RNAstructure2))
  internal_loop_1 <- unlist(internal_loop(RNAstructure1))
  internal_loop_2 <- unlist(internal_loop(RNAstructure2))
  bulge_loop_1 <- unlist(bulge_loop(RNAstructure1))
  bulge_loop_2 <- unlist(bulge_loop(RNAstructure2))
  multi_branch_loop_1 <- unlist(multi_branch_loop(RNAstructure1))
  multi_branch_loop_2 <- unlist(multi_branch_loop(RNAstructure2))
  stem_1 <- unlist(stem(RNAstructure1))
  stem_2 <- unlist(stem(RNAstructure2))

  if(length(union(stem_1,stem_2)) == 0){
    score_1 <- 1
  }else{
    score_1 <-length(intersect(stem_1,stem_2))/length(union(stem_1,stem_2))
  }
  if(length(union(multi_branch_loop_1,multi_branch_loop_2)) == 0){
    score_2 <- 1
  }else{
    score_2 <-length(intersect(multi_branch_loop_1,multi_branch_loop_2))/length(union(multi_branch_loop_1,multi_branch_loop_2))
  }
  if(length(union(bulge_loop_1,bulge_loop_2)) == 0){
    score_3 <- 1
  }else{
    score_3 <-length(intersect(bulge_loop_1,bulge_loop_2))/length(union(bulge_loop_1,bulge_loop_2))
   }
  if(length(union(internal_loop_1,internal_loop_2)) == 0){
    score_4 <- 1
  }else{
    score_4 <-length(intersect(internal_loop_1,internal_loop_2))/length(union(internal_loop_1,internal_loop_2))
    }
  if(length(union(hairpin_loop_1,hairpin_loop_2)) == 0){
    score_5 <- 1
  }else{
    score_5 <-length(intersect(hairpin_loop_1,hairpin_loop_2))/length(union(hairpin_loop_1,hairpin_loop_2))
  }
  if(max(length(stem_1),length(stem_2)) == 0){
    score_6 <- 1
  }else{
    score_6 <- min(length(stem_1),length(stem_2))/max(length(stem_1),length(stem_2))
    }
  if(max(length(multi_branch_loop_1),length(multi_branch_loop_2))  == 0){
    score_7 <- 1
  }else{
    score_7 <- min(length(multi_branch_loop_1),length(multi_branch_loop_2))/max(length(multi_branch_loop_1),length(multi_branch_loop_2))
    }
  if(max(length(bulge_loop_1),length(bulge_loop_2))  == 0){
    score_8 <- 1
  }else{
    score_8 <- min(length(bulge_loop_1),length(bulge_loop_2))/max(length(bulge_loop_1),length(bulge_loop_2))
  }
  if(max(length(internal_loop_1),length(internal_loop_2)) == 0){
    score_9 <- 1
  }else{
    score_9 <- min(length(internal_loop_1),length(internal_loop_2))/max(length(internal_loop_1),length(internal_loop_2))
  }
  if(max(length(hairpin_loop_1),length(hairpin_loop_2)) == 0){
    score_10 <- 1
  }else{
    score_10 <- min(length(hairpin_loop_1),length(hairpin_loop_2))/max(length(hairpin_loop_1),length(hairpin_loop_2))
  }
  score <- score_1 + score_2 + score_3 + score_4 + score_5 + score_6 + score_7 + score_8 + score_9 + score_10

  return(score)
}
